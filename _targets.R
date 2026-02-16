# Created by use_targets().
# Setting up the environment----

# Load packages required to define the pipeline:
library(targets)


# Target options:
tar_option_set(
  packages = c("tidyverse", "janitor", "forcats", "brms", "broom.mixed", "emmeans", "glmmTMB"),
  format = "rds",
  seed = 811
)

# R scripts in the R/ folder with custom functions:
tar_source("Scripts/0_setup.R")

# List of targets:----
list(
  # Getting the files----
  tar_target(
    name = visit_dates_file,
    command = "data_raw/Visit_dates.csv",
    format = "file"
  ),
  tar_target(
    name = visit_dates,
    command = read_csv2(visit_dates_file)
  ),
  tar_target(
    name = filefjell_1972_2009_file,
    command = "data_raw/Filefjell_1972_2009.csv",
    format = "file"
  ),
  tar_target(
    name = filefjell_1972_2009,
    command = read_csv2(filefjell_1972_2009_file)
  ),
  tar_target(
    name = filefjell_2024_file,
    command = "data_raw/Filefjell_2024.csv",
    format = "file"
  ),
  tar_target(
    name = filefjell_2024,
    command = read_csv2(filefjell_2024_file)
  ),
  tar_target(
    name = filefjell_2025_file,
    command = "data_raw/Filefjell_2025.csv",
    format = "file"
  ),
  tar_target(
    name = filefjell_2025,
    command = read_csv2(filefjell_2025_file)
  ),
  tar_target(
    name = polygones_file,
    command = "data_raw/Filefjell_polygones.csv",
    format = "file"
  ),
  tar_target(
    name = polygones,
    command = read_csv2(polygones_file)
  ),
  tar_target(
    name = type_cover_file,
    command = "data_raw/Type_cover.csv",
    format = "file"
  ),
  tar_target(
    name = type_cover,
    command = read_csv2(type_cover_file)
  ),
  tar_target(
    name = filefjell_species_file,
    command = "data_raw/Filefjell_species.csv",
    format = "file"
  ),
  tar_target(
    name = filefjell_species,
    command = read_csv2(filefjell_species_file)
  ),
  tar_target(
    name = summit_data_file,
    command = "data_raw/Summit_data.csv",
    format = "file"
  ),
  tar_target(
    name = summit_data,
    command = read_csv2(summit_data_file)
  ),
  tar_target(
    name = summit_data_tidy,
    command = summit_data |>
      clean_names() |>
      mutate(summit = str_replace_all(summit, " ", "_")) |>
      rename(correct_height = height) |>
      rename(summit_hectare = area)
  ),
  # One file for each survey----
  tar_target(
    name = filefjell_1972_clean,
    command = filefjell_1972_2009 |>
      filter(Year == 1972) |>
      left_join(visit_dates |>
                  select(!c(Year2:Data2)),
                by = c("Summit", "Year")) |>
      pivot_longer(cols = !c(Summit:Year, Date, Recorder),
                   names_to = "species",
                   values_to = "distance",
                   values_drop_na = TRUE) |>
      data_cleaning(summit_data_tidy = summit_data_tidy,
                    filefjell_species = filefjell_species)
  ),
  tar_target(
    name = filefjell_2008_2009_clean,
    command = filefjell_1972_2009 |>
      filter(Year == 2009) |>
      select(!Year) |>
      left_join(visit_dates |>
                  filter(Year %in% c(2008, 2009)) |>
                  select(!c(Year2:Data2)),
                by = c("Summit")) |>
      pivot_longer(cols = !c(Summit, Height, Year:Recorder),
                   names_to = "species",
                   values_to = "distance",
                   values_drop_na = TRUE) |>
      data_cleaning(summit_data_tidy = summit_data_tidy,
                    filefjell_species = filefjell_species)
  ),
  tar_target(
    name = filefjell_2024_2025_clean,
    command = filefjell_2024 |>
      rbind(filefjell_2025 |>
              mutate(Rareness = NA,
                     Comments = NA,
                     Svar_Mikel = NA)) |>
      mutate(Year = ifelse(Top == "Storeknippa", 2024, Year)) |>
      mutate(Distance = Height - Altitude) |>
      select(!Altitude) |>
      relocate(Distance, .after = Species) |>
      data_cleaning(summit_data_tidy = summit_data_tidy,
                    filefjell_species = filefjell_species)
  ),
  # Habitat cover----
  tar_target(
    name = polygones_tidy,
    command = polygones |>
      clean_names() |>
      select(fid, kartleggin, layer, area_m2) |>
      rename(summit = layer) |>
      relocate(summit) |>
      mutate(summit = str_replace(summit, "_NiN3", "")) |>
      rename(type = kartleggin) |>
      mutate(type = str_replace(type, "-", "")) |>
      summarise(.by = c(summit, type), area_m2 = sum(area_m2)) |>
      group_by(summit) |>
      mutate(percentage = 100 * area_m2 / sum(area_m2)) |>
      ungroup() |>
      select(!area_m2) |>
      mutate(group = case_when(grepl("T1C", type) | grepl("Naken", type) ~ "T1",
                               grepl("T27", type) ~ "T27",
                               .default = type)) |>
      summarise(.by = c(summit, group), percentage = sum(percentage))
  ),
  tar_target(
    name = type_tidy,
    command = type_cover |>
      clean_names() |>
      mutate(summit = str_replace_all(summit, " ", "_")) |>
      select(summit, type, percentage) |>
      mutate(type = ifelse(type == "Naken berg", "T1", type)) |>
      mutate(group = case_when(grepl("T1C", type) | grepl("Naken", type) ~ "T1",
                               grepl("T27", type) ~ "T27",
                               .default = type)) |>
      summarise(.by = c(summit, group), percentage = sum(percentage))
  ),
  tar_target(
    name = types_with_species,
    command = filefjell_2024_2025_clean |>
      select(summit, type) |>
      distinct() |>
      mutate(group = case_when(grepl("T1C", type) | grepl("Naken", type) ~ "T1",
                               grepl("T27", type) ~ "T27",
                               .default = type)) |>
      select(summit, group) |>
      distinct()
  ),
  tar_target(
    name = habitat_cover,
    command = types_with_species |>
      full_join(polygones_tidy, by = c("summit", "group")) |>
      rename(percentage1 = percentage) |>
      full_join(type_tidy, by = c("summit", "group")) |>
      rename(percentage2 = percentage) |>
      mutate(percentage = ifelse(is.na(percentage1), percentage2, percentage1)) |>
      left_join(summit_data_tidy |>
                  select(summit, summit_hectare),
                by = "summit") |>
      mutate(group_decare = percentage * summit_hectare / 10,
             group_decare = ifelse(is.na(group_decare), 0.25, group_decare),
             habitat = str_extract(group, "[^C]*"),
             habitat = ifelse(grepl("V", habitat), "V6", habitat)) |>
      summarise(.by = c(summit, habitat), habitat_decare = sum(group_decare)) |>
      mutate(summit = factor(summit, levels = c("Berdalseken", "Suletinden", "Unnamed", "Storeknippa", "Graanosi", "Loppenosi", "Graveggi", "Krekanosi", "Rjupeskareggen", "Frostdalsnosi", "Krekanosi_S", "Slettningseggi", "Krekahoegdi")),
             habitat = factor(habitat, levels = c("T1", "T27", "T13", "T14", "T3", "T22", "T7", "V6"))) |>
      arrange(summit, habitat)
  ),
  # Clean files----
  tar_target(
    name = filefjell_data_clean,
    command = filefjell_1972_clean |>
      rbind(filefjell_2008_2009_clean) |>
      mutate(weather = NA) |>
      relocate(weather, .after = date) |>
      rbind(filefjell_2024_2025_clean |> select(!type:svar_mikel)) |>
      mutate(summit = factor(summit, levels = c("Berdalseken", "Suletinden", "Unnamed", "Storeknippa", "Graanosi", "Loppenosi", "Graveggi", "Krekanosi", "Rjupeskareggen", "Frostdalsnosi", "Krekanosi_S", "Slettningseggi", "Krekahoegdi"))) |>
      arrange(year, summit, specialisation, species)
  ),
  tar_target(
    name = habitat_species_clean,
    command = filefjell_2024_2025_clean |>
      select(summit, type, species:functional) |>
      mutate(habitat = str_extract(type, "[^C]*"),
             habitat = ifelse(grepl("V", habitat), "V6", habitat)) |>
      relocate(habitat, .before = type) |>
      mutate(habitat = case_when(species %in% c("Eri_ang", "Eri_sch", "Eri_vag") ~ "V6",
                                 .default = habitat)) |>
      left_join(habitat_cover, by = c("summit", "habitat")) |>
      mutate(habitat_decare = ifelse(habitat == "V6" & grepl("T", type), 0.25, habitat_decare)) |>
      mutate(summit = factor(summit, levels = c("Berdalseken", "Suletinden", "Unnamed", "Storeknippa", "Graanosi", "Loppenosi", "Graveggi", "Krekanosi", "Rjupeskareggen", "Frostdalsnosi", "Krekanosi_S", "Slettningseggi", "Krekahoegdi")),
             habitat = factor(habitat, levels = c("T1", "T27", "T14", "T3", "T22", "T7", "V6"))) |>
      arrange(summit, habitat)
  ),
  # General datasets----
  tar_target(
    name = summit_periods,
    command = filefjell_data_clean |>
      select(year, summit) |>
      mutate(year = ifelse(year == 2025 & summit == "Storeknippa", 2024, year)) |> # For simplicity's sake, we assume the five species recorded in Storeknippa in 2025 were also there in 2024
      distinct() |>
      pivot_wider(names_from = year, names_prefix = "y", values_from = year) |>
      mutate(first = y1972,
             second = coalesce(y2008, y2009),
             third = coalesce(y2024, y2025)) |>
      mutate(period1 = second - first,
             period2 = third - second) |>
      select(summit, period1, period2) |>
      pivot_longer(cols = c(period1, period2), names_to = "period", values_to = "time")
  ),
  tar_target(
    name = filefjell_simplified,
    command = filefjell_data_clean |>
      select(!c(date:recorder)) |>
      mutate(year = case_when(year == 1972 ~ "first",
                              year %in% c(2008, 2009) ~ "second",
                              year %in% c(2024, 2025) ~ "third"))
  ),
  # Status overview----
  tar_target(
    name = status_survey,
    command = filefjell_simplified |>
      pivot_wider(names_from = year, values_from = distance) |>
      mutate(status1 = case_when(!is.na(first) ~ "1972Present",
                                 is.na(first) ~ "1972Absent")) |>
      mutate(status2 = case_when(!is.na(first) & !is.na(second) ~ "2009Remained",
                                 !is.na(first) & is.na(second) ~ "2009Lost",
                                 is.na(first) & !is.na(second) ~ "2009New",
                                 is.na(first) & is.na(second) ~ "2009Absent")) |>
      mutate(status3 = case_when(!is.na(first) & !is.na(second) & !is.na(third) ~ "2024Remained",
                                 !is.na(first) & !is.na(second) & is.na(third) ~ "2024Lost",
                                 !is.na(first) & is.na(second) & !is.na(third) ~ "2024Reappeared",
                                 !is.na(first) & is.na(second) & is.na(third) ~ "2024Stayed_lost",
                                 is.na(first) & !is.na(second) & !is.na(third) ~ "2024Stayed",
                                 is.na(first) & !is.na(second) == 1 & is.na(third) ~ "2024Disappeared",
                                 is.na(first) & is.na(second) & !is.na(third) ~ "2024New")) |>
      select(status1:status3) |>
      pivot_longer(cols = status1:status3, names_to = "survey", values_to = "status") |>
      summarise(.by = "status", total = n()) |>
      arrange(status)
  ),
  tar_target(
    name = header_map,
    command = tibble(
      col_keys = c("1972status", "1972total", "2008/09status", "2008/09total", "2024/25status", "2024/25total"),
      level1   = c("1972", "1972", "2008/09", "2008/09", "2024/25", "2024/25")
    )
  ),
  tar_target(
    name = status_survey_ft,
    command = tibble(
      "1972status" = c("Present",
                       rep("", 6),
                       "Total present"),
      "1972total" = c(status_survey |> filter(status == "1972Present") |> pull(total),
                      rep("", 6),
                      status_survey |> filter(status == "1972Present") |> pull(total)),
      "2008/09status" = c("Remained", "", "Lost", "", "New", "", "", "Total present"),
      "2008/09total" = c(status_survey |> filter(status == "2009Remained") |> pull(total), "",
                         status_survey |> filter(status == "2009Lost") |> pull(total), "",
                         status_survey |> filter(status == "2009New") |> pull(total), "", "",
                         (status_survey |> filter(status == "2009Remained") |> pull(total)) + (status_survey |> filter(status == "2009New") |> pull(total))),
      "2024/25status" = c("Remained", "Lost", "Reappeared", "Did not reappear", "Remained", "Lost", "New", "Total present"),
      "2024/25total" = c(status_survey |> filter(status == "2024Remained") |> pull(total),
                         status_survey |> filter(status == "2024Lost") |> pull(total),
                         status_survey |> filter(status == "2024Reappeared") |> pull(total),
                         status_survey |> filter(status == "2024Stayed_lost") |> pull(total),
                         status_survey |> filter(status == "2024Stayed") |> pull(total),
                         status_survey |> filter(status == "2024Disappeared") |> pull(total),
                         status_survey |> filter(status == "2024New") |> pull(total),
                         (status_survey |> filter(status == "2024Remained") |> pull(total)) + (status_survey |> filter(status == "2024Reappeared") |> pull(total)) + (status_survey |> filter(status == "2024Stayed") |> pull(total)) + (status_survey |> filter(status == "2024New") |> pull(total)))
    ) |>
      flextable() |>
      set_header_df(mapping = header_map, key = "col_keys") |>
      merge_h(part = "header") |>
      bg(part = "header", bg = "black") |>
      color(part = "header", color = "white") |>
      bold(part = "header") |>
      align(part = "header", align = "center") |>
      bg(part = "body", bg = "white") |>
      color(part = "body", color = "black") |>
      bg(part = "body", i = 8, bg = "grey") |>
      hline(i = 4) |>
      hline(i = 2, j = 3:6) |>
      hline(i = 6, j = 3:6) |>
      hline(i = 7) |>
      border(i = c(1:6, 8), j = 3, border.left = officer::fp_border(color = "black")) |>
      border(i = 1:8, j = 5, border.left = officer::fp_border(color = "black")) |>
      border(part = "header", border.left = officer::fp_border(color = "white")) |>
      vline_left() |>
      vline_right() |>
      align(part = "body", align = "left") |>
      align(part = "body", j = c(2, 4, 6), align = "center") |>
      flextable::font(part = "all", fontname = "Times New Roman") |>
      autofit()
  ),
  tar_target(
    name = status_spe_survey,
    command = filefjell_simplified |>
      pivot_wider(names_from = year, values_from = distance) |>
      mutate(status1 = case_when(!is.na(first) ~ "1972Present",
                                 is.na(first) ~ "1972Absent")) |>
      mutate(status2 = case_when(!is.na(first) & !is.na(second) ~ "2009Remained",
                                 !is.na(first) & is.na(second) ~ "2009Lost",
                                 is.na(first) & !is.na(second) ~ "2009New",
                                 is.na(first) & is.na(second) ~ "2009Absent")) |>
      mutate(status3 = case_when(!is.na(first) & !is.na(second) & !is.na(third) ~ "2024Remained",
                                 !is.na(first) & !is.na(second) & is.na(third) ~ "2024Lost",
                                 !is.na(first) & is.na(second) & !is.na(third) ~ "2024Reappeared",
                                 !is.na(first) & is.na(second) & is.na(third) ~ "2024Stayed_lost",
                                 is.na(first) & !is.na(second) & !is.na(third) ~ "2024Stayed",
                                 is.na(first) & !is.na(second) == 1 & is.na(third) ~ "2024Disappeared",
                                 is.na(first) & is.na(second) & !is.na(third) ~ "2024New")) |>
      select(specialisation, status1:status3) |>
      pivot_longer(cols = status1:status3, names_to = "survey", values_to = "status") |>
      summarise(.by = c("specialisation", "status"), total = n()) |>
      arrange(status)
  ),
  tar_target(
    name = header_spe_map,
    command = tibble(
      col_keys = c("specialisation", "1972status", "1972total", "2008/09status", "2008/09total", "2024/25status", "2024/25total"),
      level1   = c("Specialisation", "1972", "1972", "2008/09", "2008/09", "2024/25", "2024/25")
    )
  ),
  tar_target(
    name = status_spe_survey_ft,
    command = tibble(
      "specialisation" = c("Specialists",
                           rep("", 7),
                           "Generalists",
                           rep("", 7)),
      "1972status" = c("Present",
                       rep("", 6),
                       "Total specialists",
                       "Present",
                       rep("", 6),
                       "Total generalists"),
      "1972total" = c(status_spe_survey |> filter(specialisation == "alpine" & status == "1972Present") |> pull(total),
                      rep("", 6),
                      status_spe_survey |> filter(specialisation == "alpine" & status == "1972Present") |> pull(total),
                      status_spe_survey |> filter(specialisation == "generalist" & status == "1972Present") |> pull(total),
                      rep("", 6),
                      status_spe_survey |> filter(specialisation == "generalist" & status == "1972Present") |> pull(total)),
      "2008/09status" = c("Remained", "", "Lost", "", "New", "", "", "Total present",
                          "Remained", "", "Lost", "", "New", "", "", "Total present"),
      "2008/09total" = c(status_spe_survey |> filter(specialisation == "alpine" & status == "2009Remained") |> pull(total),
                         "",
                         status_spe_survey |> filter(specialisation == "alpine" & status == "2009Lost") |> pull(total),
                         "",
                         status_spe_survey |> filter(specialisation == "alpine" & status == "2009New") |> pull(total),
                         "",
                         "",
                         (status_spe_survey |> filter(specialisation == "alpine" & status == "2009Remained") |> pull(total)) +
                           (status_spe_survey |> filter(specialisation == "alpine" & status == "2009New") |> pull(total)),
                         status_spe_survey |> filter(specialisation == "generalist" & status == "2009Remained") |> pull(total),
                         "",
                         status_spe_survey |> filter(specialisation == "generalist" & status == "2009Lost") |> pull(total),"
                     ",
                         status_spe_survey |> filter(specialisation == "generalist" & status == "2009New") |> pull(total),
                         "",
                         "",
                         (status_spe_survey |> filter(specialisation == "generalist" & status == "2009Remained") |> pull(total)) +
                           (status_spe_survey |> filter(specialisation == "generalist" & status == "2009New") |> pull(total))),
      "2024/25status" = c("Remained", "Lost", "Reappeared", "Did not reappear", "Remained", "Lost", "New", "Total present",
                          "Remained", "Lost", "Reappeared", "Did not reappear", "Remained", "Lost", "New", "Total present"),
      "2024/25total" = c(status_spe_survey |> filter(specialisation == "alpine" & status == "2024Remained") |> pull(total),
                         status_spe_survey |> filter(specialisation == "alpine" & status == "2024Lost") |> pull(total),
                         status_spe_survey |> filter(specialisation == "alpine" & status == "2024Reappeared") |> pull(total),
                         status_spe_survey |> filter(specialisation == "alpine" & status == "2024Stayed_lost") |> pull(total),
                         status_spe_survey |> filter(specialisation == "alpine" & status == "2024Stayed") |> pull(total),
                         status_spe_survey |> filter(specialisation == "alpine" & status == "2024Disappeared") |> pull(total),
                         status_spe_survey |> filter(specialisation == "alpine" & status == "2024New") |> pull(total),
                         (status_spe_survey |> filter(specialisation == "alpine" & status == "2024Remained") |> pull(total)) +
                           (status_spe_survey |> filter(specialisation == "alpine" & status == "2024Reappeared") |> pull(total)) +
                           (status_spe_survey |> filter(specialisation == "alpine" & status == "2024Stayed") |> pull(total)) +
                           (status_spe_survey |> filter(specialisation == "alpine" & status == "2024New") |> pull(total)),
                         status_spe_survey |> filter(specialisation == "generalist" & status == "2024Remained") |> pull(total),
                         status_spe_survey |> filter(specialisation == "generalist" & status == "2024Lost") |> pull(total),
                         status_spe_survey |> filter(specialisation == "generalist" & status == "2024Reappeared") |> pull(total),
                         status_spe_survey |> filter(specialisation == "generalist" & status == "2024Stayed_lost") |> pull(total),
                         status_spe_survey |> filter(specialisation == "generalist" & status == "2024Stayed") |> pull(total),
                         status_spe_survey |> filter(specialisation == "generalist" & status == "2024Disappeared") |> pull(total),
                         status_spe_survey |> filter(specialisation == "generalist" & status == "2024New") |> pull(total),
                         (status_spe_survey |> filter(specialisation == "generalist" & status == "2024Remained") |> pull(total)) +
                           (status_spe_survey |> filter(specialisation == "generalist" & status == "2024Reappeared") |> pull(total)) +
                           (status_spe_survey |> filter(specialisation == "generalist" & status == "2024Stayed") |> pull(total)) +
                           (status_spe_survey |> filter(specialisation == "generalist" & status == "2024New") |> pull(total)))
    ) |>
      flextable() |>
      set_header_df(mapping = header_spe_map, key = "col_keys") |>
      merge_h(part = "header") |>
      bg(part = "header", bg = "black") |>
      color(part = "header", color = "white") |>
      bold(part = "header") |>
      align(part = "header", j = 2:7, align = "center") |>
      bg(part = "body", bg = "white") |>
      color(part = "body", color = "black") |>
      bg(part = "body", i = c(8, 16), bg = "grey") |>
      hline(i = c(4, 12), j = 2:7) |>
      hline(i = c(2, 10), j = 4:7) |>
      hline(i = c(6, 14), j = 4:7) |>
      border(i = 8, border.bottom = officer::fp_border(width = 2)) |>
      border(i = c(1:6, 8, 9:14, 16), j = 4, border.left = officer::fp_border(color = "black")) |>
      border(i = 1:16, j = c(2, 6), border.left = officer::fp_border(color = "black")) |>
      border(part = "header", border.left = officer::fp_border(color = "white")) |>
      vline_left() |>
      vline_right() |>
      align(part = "body", align = "left") |>
      align(part = "body", j = c(3, 5, 7), align = "center") |>
      flextable::font(part = "all", fontname = "Times New Roman") |>
      autofit()
  ),
  # Richness----
  tar_target(
    name = richness,
    command = filefjell_simplified |>
      summarise(.by = c(year, summit, specialisation), richness = n())
  ),
  tar_target(
    name = richness_rate,
    command = richness |>
      pivot_wider(names_from = year, values_from = richness, values_fill = 0) |>
      mutate(period1 = second - first,
             period2 = third - second) |>
      select(!c(first:third)) |>
      pivot_longer(cols = c(period1, period2), names_to = "period", values_to = "change") |>
      left_join(summit_periods, by = c("summit", "period")) |>
      mutate(rate = change / time)
  ),
  tar_target(
    name = richness_mod,
    command = glmmTMB(
      rate ~
        period * specialisation + (1 | summit),
      dispformula = ~period,
      family = gaussian,
      data = richness_rate)
  ),
  tar_target(
    name = richness_results,
    command = richness_mod |>
      mod_summary()
  ),
  tar_target(
    name = richness10,
    command = filefjell_simplified |>
      filter(distance <= 10) |>
      summarise(.by = c(year, summit, specialisation), richness = n())
  ),
  tar_target(
    name = richness10_rate,
    command = richness10 |>
      pivot_wider(names_from = year, values_from = richness, values_fill = 0) |>
      mutate(period1 = second - first,
             period2 = third - second) |>
      select(!c(first:third)) |>
      pivot_longer(cols = c(period1, period2), names_to = "period", values_to = "change") |>
      left_join(summit_periods, by = c("summit", "period")) |>
      mutate(rate = change / time)
  ),
  tar_target(
    name = richness10_mod,
    command = glmmTMB(
      rate ~
        period * specialisation + (1 | summit),
      dispformula = ~period,
      family = gaussian,
      data = richness10_rate)
  ),
  tar_target(
    name = richness10_results,
    command = richness10_mod |>
      mod_summary()
  ),
  # Turnover----
  tar_target(
    name = turnover,
    command = filefjell_simplified |>
      pivot_wider(names_from = year, values_from = distance) |>
      mutate(new1 = ifelse(is.na(first) & !is.na(second), 1, 0),
             new2 = ifelse(is.na(second) & !is.na(third), 1, 0),
             lost1 = ifelse(!is.na(first) & is.na(second), 1, 0),
             lost2 = ifelse(!is.na(second) & is.na(third), 1, 0)) |>
      select(!c(first:third)) |>
      summarise(.by = c(summit, specialisation),
                newperiod1 = sum(new1),
                newperiod2 = sum(new2),
                lostperiod1 = sum(lost1),
                lostperiod2 = sum(lost2)) |>
      pivot_longer(cols = c(newperiod1, newperiod2, lostperiod1, lostperiod2),
                   names_to = c("type", "period"),
                   names_pattern = "^(new|lost)(period\\d+)$",
                   values_to = "value")
  ),
  tar_target(
    name = new_rate,
    command = turnover |>
      filter(type == "new") |>
      left_join(summit_periods, by = c("summit", "period")) |>
      mutate(rate = value / time)
  ),
  tar_target(
    name = new_mod,
    command = glmmTMB(
      rate ~
        period * specialisation + (1 | summit),
      family = gaussian,
      data = new_rate)
  ),
  tar_target(
    name = new_results,
    command = new_mod |>
      mod_summary()
  ),
  tar_target(
    name = lost_rate,
    command = turnover |>
      filter(type == "lost") |>
      left_join(summit_periods, by = c("summit", "period")) |>
      mutate(rate = value / time)
  ),
  tar_target(
    name = lost_mod,
    command = glmmTMB(
      rate ~
        period * specialisation + (1 | summit),
      dispformula = ~period,
      family = gaussian,
      data = lost_rate)
  ),
  tar_target(
    name = lost_results,
    command = lost_mod |>
      mod_summary()
  ),
  # Turnover 10m----
  tar_target(
    name = turnover10,
    command = filefjell_simplified |>
      filter(distance <= 10) |>
      pivot_wider(names_from = year, values_from = distance) |>
      mutate(new1 = ifelse(is.na(first) & !is.na(second), 1, 0),
             new2 = ifelse(is.na(second) & !is.na(third), 1, 0),
             lost1 = ifelse(!is.na(first) & is.na(second), 1, 0),
             lost2 = ifelse(!is.na(second) & is.na(third), 1, 0)) |>
      select(!c(first:third)) |>
      summarise(.by = c(summit, specialisation),
                newperiod1 = sum(new1),
                newperiod2 = sum(new2),
                lostperiod1 = sum(lost1),
                lostperiod2 = sum(lost2)) |>
      pivot_longer(cols = c(newperiod1, newperiod2, lostperiod1, lostperiod2),
                   names_to = c("type", "period"),
                   names_pattern = "^(new|lost)(period\\d+)$",
                   values_to = "value")
  ),
  tar_target(
    name = new10_rate,
    command = turnover10 |>
      filter(type == "new") |>
      left_join(summit_periods, by = c("summit", "period")) |>
      mutate(rate = value / time)
  ),
  tar_target(
    name = new10_mod,
    command = glmmTMB(
      rate ~
        period * specialisation + (1 | summit),
      dispformula = ~period,
      family = gaussian,
      data = new10_rate)
  ),
  tar_target(
    name = new10_results,
    command = new10_mod |>
      mod_summary()
  ),
  tar_target(
    name = lost10_rate,
    command = turnover10 |>
      filter(type == "lost") |>
      left_join(summit_periods, by = c("summit", "period")) |>
      mutate(rate = value / time)
  ),
  tar_target(
    name = lost10_mod,
    command = glmmTMB(
      rate ~
        period * specialisation + (1 | summit),
      dispformula = ~period,
      family = gaussian,
      data = lost10_rate)
  ),
  tar_target(
    name = lost10_results,
    command = lost10_mod |>
      mod_summary()
  ),
  # Altitude----
  tar_target(
    name = altitude_rate,
    command = filefjell_simplified |>
      pivot_wider(names_from = year, values_from = distance) |>
      mutate(period1 = (second - first) * (-1),
             period2 = (third - second) * (-1)) |> # Change the sign so that a positive value indicates upwards movement
      select(!c(first:third)) |>
      pivot_longer(cols = c(period1, period2), names_to = "period", values_to = "change") |>
      filter(!is.na(change)) |>
      left_join(summit_periods, by = c("summit", "period")) |>
      mutate(rate = change / time)
  ),
  tar_target(
    name = priors_t,
    command = c(
      # Mean model (as before)
      prior(normal(0, 0.5), class = "Intercept"),
      prior(normal(0, 0.5), class = "b"),
      prior(exponential(3), class = "sd"),
      prior(gamma(2, 0.4), class = "nu"),
      # Sigma model (log-scale)
      prior(normal(-1.098612, 0.5), class = "Intercept", dpar = "sigma"),
      prior(normal(0, 0.3), class = "b", dpar = "sigma")
    )
  ),
  tar_target(
    name = altitude_bay,
    command = brm(
      bf(rate ~
           period * specialisation + (1|summit) + (1|species),
         sigma ~ period),
      family = student(),
      prior = priors_t,
      data = altitude_rate,
      chains = 4, iter = 4000, seed = 811,
      control = list(adapt_delta = 0.95)
    )
  ),
  tar_target(
    name = altitude_results,
    command = altitude_bay |>
      mod_summary()
  ),
  tar_target(
    name = altitude10_rate,
    command = filefjell_simplified |>
      filter(distance <= 10) |>
      pivot_wider(names_from = year, values_from = distance) |>
      mutate(period1 = (second - first) * (-1),
             period2 = (third - second) * (-1)) |> # Change the sign so that a positive value indicates upwards movement
      select(!c(first:third)) |>
      pivot_longer(cols = c(period1, period2), names_to = "period", values_to = "change") |>
      filter(!is.na(change)) |>
      left_join(summit_periods, by = c("summit", "period")) |>
      mutate(rate = change / time)
  ),
  tar_target(
    name = altitude10_bay,
    command = brm(
      bf(rate ~
           period * specialisation + (1|summit) + (1|species),
         sigma ~ period),
      family = student(),
      prior = priors_t,
      data = altitude10_rate,
      chains = 4, iter = 4000, seed = 811,
      control = list(adapt_delta = 0.95)
    )
  ),
  tar_target(
    name = altitude10_results,
    command = altitude10_bay |>
      mod_summary()
  )
  # # # New species per nature type----
  # # tar_target(
  # #   name = colonizers_data,
  # #   command = filefjell_2024_clean |>
  # #     semi_join(
  # #       species_turnover_data |>
  # #         filter(turnover24 == 1) |>
  # #         select(summit:species)) |>
  # #     mutate(hovedtype = str_sub(type, start = 1L, end = 2L)) |>
  # #     relocate(hovedtype, .before = type)
  # # )
)

