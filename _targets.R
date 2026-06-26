# Created by use_targets()
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
                     Svar = NA)) |>
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
  tar_target(
    name = habitat_names_ft,
    command = habitat_cover |>
      select(habitat) |>
      distinct() |>
      mutate(habitat = factor(habitat, levels = c("T1", "T27", "T13", "T14", "T7", "V6", "T3", "T22"))) |>
      arrange(habitat) |>
      mutate("NiN name" = case_when(habitat == "T1" ~ "Bare rock",
                                    habitat == "T27" ~ "Boulder field",
                                    habitat == "T13" ~ "Scree",
                                    habitat == "T14" ~ "Ridge",
                                    habitat == "T7" ~ "Snowbed",
                                    habitat == "V6" ~ "Wet snowbed",
                                    habitat == "T3" ~ "Alpine heath, leeside and tundra",
                                    habitat == "T22" ~ "Alpine grassland and grass tundra"),
             "Habitat" = case_when(habitat == "T1" ~ "Bare rock",
                                   habitat == "T27" ~ "Boulder field",
                                   habitat == "T13" ~ "Scree",
                                   habitat == "T14" ~ "Ridge",
                                   habitat == "T7" ~ "Snowbed",
                                   habitat == "V6" ~ "Wet snowbed",
                                   habitat == "T3" ~ "Alpine heath",
                                   habitat == "T22" ~ "Alpine grassland")) |>
      rename("NiN code" = habitat) |>
      relocate(Habitat) |>
      clean_ft() |>
      fontsize(part = "header", size = 12) |>
      fontsize(part = "body", size = 11) |>
      autofit()
  ),
  # Clean files----
  tar_target(
    name = filefjell_data_clean,
    command = filefjell_1972_clean |>
      rbind(filefjell_2008_2009_clean) |>
      mutate(weather = NA) |>
      relocate(weather, .after = date) |>
      rbind(filefjell_2024_2025_clean |> select(!type:svar)) |>
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
    name = status,
    command = filefjell_simplified |>
      select(year, summit, species) |>
      mutate(presence = 1) |>
      pivot_wider(names_from = year, values_from = presence, values_fill = 0)
  ),
  tar_target(
    name = flows_all,
    command = status |>
      mutate(type12 = case_when(first  == 1 & second == 1 ~ "Persisted",
                                first  == 1 & second == 0 ~ "Lost",
                                first  == 0 & second == 1 ~ "New",
                                first  == 0 & second == 0 ~ "Absent"),
             type23 = case_when(second == 1 & third  == 1 ~ "Persisted",
                                second == 1 & third  == 0L ~ "Lost",
                                second == 0 & third  == 1 ~ "New",
                                second == 0 & third == 0 ~ "Absent")) |>
      count(first, second, third, type12, type23, name = "n") |>
      mutate(flow_id = row_number(),
             type12 = factor(type12, levels = c("Persisted","Lost","New","Absent")),
             type23 = factor(type23, levels = c("Persisted","Lost","New","Absent")))
  ),
  tar_target(
    name = lodes_12,
    command = flows_all |>
      select(flow_id, n, first, second, type12) |>
      ggalluvial::to_lodes_form(axes    = c("first","second"),
                                key     = "x",
                                value   = "stratum",
                                id      = "flow_id",
                                weight  = "n",
                                discern = TRUE) |>
      rename(survey = x) |>
      mutate(x = recode(survey, first = 1, second = 2.38),
             stratum = case_when(flow_id == 7 & survey == "first" ~ 7,
                                 flow_id == 6 & survey == "first" ~ 6,
                                 flow_id == 5 & survey == "first" ~ 5,
                                 flow_id == 4 & survey == "first" ~ 4,
                                 flow_id == 3 & survey == "first" ~ 3,
                                 flow_id == 2 & survey == "first" ~ 2,
                                 flow_id == 1 & survey == "first" ~ 1,
                                 flow_id == 7 & survey == "second" ~ 7, # 7
                                 flow_id == 6 & survey == "second" ~ 6, # 6
                                 flow_id == 5 & survey == "second" ~ 3, # 3
                                 flow_id == 4 & survey == "second" ~ 2, # 2
                                 flow_id == 3 & survey == "second" ~ 5, # 5
                                 flow_id == 2 & survey == "second" ~ 4, # 4
                                 flow_id == 1 & survey == "second" ~ 1), # 1
             stratum = factor(stratum, levels = c(1, 2, 3, 4, 5, 6, 7)),
             type    = type12,
             n = case_when(type == "New" & survey == "first" ~ 0,
                           type == "Absent" ~ 0,
                           TRUE ~ n),
             alpha_group = case_when(flow_id %in% c(2, 4, 6) ~ "light",
                                     TRUE ~ "dark")) |>
      select(flow_id, x, stratum, n, type, alpha_group)
  ),
  tar_target(
    name = lodes_23,
    command =  flows_all |>
      select(flow_id, n, second, third, type23) |>
      ggalluvial::to_lodes_form(axes = c("second","third"),
                                key = "x",
                                value = "stratum",
                                id = "flow_id",
                                weight = "n",
                                discern = TRUE) |>
      rename(survey = x) |>
      mutate(x = recode(survey, second = 2.38, third = 3),
             stratum = case_when(flow_id == 7 & survey == "second" ~ 7, # 7
                                 flow_id == 6 & survey == "second" ~ 6, # 6
                                 flow_id == 5 & survey == "second" ~ 3, # 3
                                 flow_id == 4 & survey == "second" ~ 2, # 2
                                 flow_id == 3 & survey == "second" ~ 5, # 5
                                 flow_id == 2 & survey == "second" ~ 4, # 4
                                 flow_id == 1 & survey == "second" ~ 1, # 1
                                 flow_id == 7 & survey == "third" ~ 7, # 7
                                 flow_id == 6 & survey == "third" ~ 3, # 3
                                 flow_id == 5 & survey == "third" ~ 5, # 5
                                 flow_id == 4 & survey == "third" ~ 1, # 1
                                 flow_id == 3 & survey == "third" ~ 6, # 6
                                 flow_id == 2 & survey == "third" ~ 2, # 2
                                 flow_id == 1 & survey == "third" ~ 4), # 4
             stratum = factor(stratum, levels = c(1, 2, 3, 4, 5, 6, 7)),
             type = type23,
             n = case_when(flow_id == 1 & survey == "second" ~ 0,
                           TRUE ~ n),
             alpha_group = case_when(flow_id %in% c(3, 5) ~ "light",
                                     TRUE ~ "dark")) |>
      select(flow_id, x, stratum, n, type, alpha_group)
  ),
  tar_target(
    name = strata,
    command = bind_rows(
      status |>
        mutate(x = 1,
               stratum = factor(first, levels = c(0, 1))) |>
        count(x, stratum, name = "n"),
      status |>
        mutate(x = 2.38,
               stratum = factor(second, levels = c(0, 1))) |>
        count(x, stratum, name = "n"),
      status |>
        mutate(x = 3,
               stratum = factor(third, levels = c(0, 1))) |>
        count(x, stratum, name = "n")) |>
      mutate(strata_fill = ifelse(stratum == 1, "#4DAF4A", "white"))
  ),
  tar_target(
    name = species_records_manually,
    command = tribble(
      ~x,     ~y,      ~label,
      1.690,  820.0, "Period 1",
      2.690,  820.0, "Period 2",
      0.930,  188.0,      "376",
      2.310,  177.5,      "355",
      2.310,  496.0,      "282",
      2.325,  680.0,       "21",
      3.070,  170.5,      "341",
      3.070,  453.5,      "225",
      3.055,  573.0,       "14",
      3.070,  639.0,      "118",
      3.055,  705.0,       "14",
      3.055,  740.5,       "57",
      3.040,  776.0,        "7"
    )
  ),
  tar_target(
    name = species_records_plot,
    command = ggplot() +
      geom_flow(data = lodes_12,
                aes(x = x, alluvium = flow_id, y = n, stratum = stratum, fill = type),
                alpha = 0.9,
                width = 0,
                knot.pos = 0.35,
                curve_type = "cubic") +
      geom_flow(data = lodes_23,
                aes(x = x, alluvium = flow_id, y = n, stratum = stratum, fill = type, alpha = alpha_group),
                width = 0,
                knot.pos = 0.35,
                curve_type = "cubic") +
      geom_stratum(data  = strata |> filter(x %in% c(1, 2.38)),
                   aes(x = x, y = n, stratum = stratum),
                   alpha = 1,
                   width = 0) +
      geom_stratum(data  = strata |> filter(x %in% c(2.38, 3)),
                   aes(x = x, y = n, stratum = stratum),
                   alpha = 1,
                   width = 0) +
      geom_text(data = species_records_manually,
                aes(x = x, y = y, label = label),
                family = "serif", size = 4) +
      scale_fill_manual(values = alluvial_palette,
                        name   = "Between surveys") +
      scale_alpha_manual(values = c("light" = 0.6, "dark" = 0.9)) +
      guides(alpha = "none") +
      scale_x_continuous(breaks = c(1, 2.38, 3),
                         labels = c("1972", "2008/09", "2024/25"),
                         expand = c(0.08, 0.05)) +
      labs(x = NULL,
           y = "Number of species records") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.title.y = element_markdown(margin = margin(r = 10)),
            legend.position = "top",
            text = element_text(size = 14, family = "serif"))
  ),
  # Richness----
  tar_target(
    name = richness,
    command = filefjell_simplified |>
      summarise(.by = c(year, summit, specialisation), richness = n())
  ),
  tar_target(
    name = richness_overview,
    command = richness |>
      pivot_wider(names_from = year, values_from = richness, values_fill = 0) |>
      mutate(change1 = case_when(second > first ~ "Increase",
                                 second == first ~ "No change",
                                 second < first ~ "Decrease"),
             change2 = case_when(third > second ~ "Increase",
                                 third == second ~ "No change",
                                 third < second ~ "Decrease")) |>
      arrange(summit)
  ),
  tar_target(
    name = richness_summits_ft,
    command = richness_overview |>
      select(!first:third) |>
      pivot_wider(names_from = specialisation, values_from = change1:change2) |>
      mutate(period1 = case_when(change1_alpine == "Increase" & change1_generalist == "Increase" ~ "++",
                                 change1_alpine == "Increase" & change1_generalist == "Decrease" ~ "+-",
                                 change1_alpine == "Decrease" & change1_generalist == "Increase" ~ "-+",
                                 change1_alpine == "Decrease" & change1_generalist == "Decrease" ~ "--"),
             period2 = case_when(change2_alpine == "Increase" & change2_generalist == "Increase" ~ "++",
                                 change2_alpine == "Increase" & change2_generalist == "Decrease" ~ "+-",
                                 change2_alpine == "No change" & change2_generalist == "Increase" ~ "0+",
                                 change2_alpine == "Decrease" & change2_generalist == "Increase" ~ "-+",
                                 change2_alpine == "Decrease" & change2_generalist == "Decrease" ~ "--")) |>
      select(period1, period2) |>
      pivot_longer(cols = period1:period2, names_to = "period", values_to = "change") |>
      summarise(.by = c(period, change), total = n()) |>
      pivot_wider(names_from = period, values_from = total, values_fill = 0) |>
      mutate(alpine = str_sub(change, end = 1L),
             generalist = str_sub(change, start = 2L, end = 2L),
             alpine = case_when(alpine == "+" ~ "Increase",
                                alpine == "0" ~ "No change",
                                alpine == "-" ~ "Decrease"),
             alpine = factor(alpine, levels = c("Increase", "No change", "Decrease")),
             generalist = case_when(generalist == "+" ~ "Increase",
                                    generalist == "0" ~ "No change",
                                    generalist == "-" ~ "Decrease"),
             generalist = factor(generalist, levels = c("Increase", "Decrease"))) |>
      select(!change) |>
      relocate(alpine, generalist) |>
      arrange(alpine, generalist) |>
      rename("Specialists" = alpine,
             "Generalists" = generalist,
             "1972-2008/09" = period1,
             "2008/09-2024/25" = period2) |>
      clean_ft() |>
      hline(i = c(2, 3), border = officer::fp_border(color = "grey")) |>
      merge_v(j = 1) |>
      align(j = 3:4, align = "center") |>
      fontsize(part = "header", size = 12) |>
      fontsize(part = "body", size = 11) |>
      autofit()
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
      mutate(dec_rate = 10 * change / time)
  ),
  tar_target(
    name = richness_mod,
    command = glmmTMB(
      dec_rate ~
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
      mutate(dec_rate = 10 * value / time)
  ),
  tar_target(
    name = new_mod,
    command = glmmTMB(
      dec_rate ~
        period * specialisation + (1 | summit),
      dispformula = ~period,
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
      mutate(dec_rate = 10 * value / time)
  ),
  tar_target(
    name = lost_mod,
    command = glmmTMB(
      dec_rate ~
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
  tar_target(
    name = original_lost,
    command = filefjell_simplified |>
      mutate(presence = ifelse(!is.na(distance), 1, 0)) |>
      select(!c(height:bedrock, distance)) |>
      pivot_wider(names_from = year, values_from = presence) |>
      filter(!is.na(first)) |>
      mutate(lost1 = ifelse(!is.na(first) & is.na(second), 1, 0),
             lost2 = ifelse(!is.na(second) & is.na(third), 1, 0)) |>
      select(!c(first:third)) |>
      summarise(.by = c(summit, specialisation),
                period1 = sum(lost1),
                period2 = sum(lost2)) |>
      pivot_longer(cols = c(period1, period2),
                   names_to = c("period"),
                   values_to = "value") |>
      left_join(summit_periods, by = c("summit", "period")) |>
      mutate(dec_rate = 10 * value / time)
  ),
  tar_target(
    name = orilost_mod,
    command = glmmTMB(
      dec_rate ~
        period * specialisation + (1 | summit),
      dispformula = ~period,
      family = gaussian,
      data = original_lost)
  ),
  tar_target(
    name = orilost_results,
    command = orilost_mod |>
      mod_summary()
  ),
  # New and lost species----
  tar_target(
    name = new_lost,
    command = filefjell_simplified |>
      pivot_wider(names_from = year, values_from = distance) |>
      mutate(status = case_when(!is.na(first) & !is.na(second) & !is.na(third) ~ "persisted",
                                !is.na(first) & !is.na(second) & is.na(third) ~ "lost_2",
                                !is.na(first) & is.na(second) & !is.na(third) ~ "reappeared",
                                !is.na(first) & is.na(second) & is.na(third) ~ "lost_1",
                                is.na(first) & !is.na(second) & !is.na(third) ~ "new_1",
                                is.na(first) & !is.na(second) & is.na(third) ~ "redisappeared",
                                is.na(first) & is.na(second) & !is.na(third) ~ "new_2")) |>
      summarise(.by = c("species", "specialisation", "functional", "status"), total = n()) |>
      mutate(status = factor(status, levels = c("persisted", "new_1", "new_2", "reappeared", "redisappeared", "lost_1", "lost_2"))) |>
      arrange(status) |>
      pivot_wider(names_from = status, values_from = total, values_fill = 0) |>
      arrange(species)
  ),
  tar_target(
    name = winners,
    command = new_lost |>
      filter((new_1 + new_2) > 6 & (lost_1 + lost_2) < 2)
  ),
  tar_target(
    name = winners_ft,
    command = winners |>
      select(species, specialisation, persisted:new_2) |>
      arrange(specialisation) |>
      mutate(dispersal = case_when(species == "Ant_dio" ~ "Wind",
                                   species == "Ath_dis" ~ "Wind",
                                   species == "Ave_fle" ~ "Wind",
                                   species == "Cry_cri" ~ "Wind",
                                   species == "Eri_ang" ~ "Wind",
                                   species == "Eri_sch" ~ "Wind",
                                   species == "Mic_ste" ~ "Water / Wind",
                                   species == "Oma_nor" ~ "Wind",
                                   species == "Ran_pyg" ~ "Water / Wind",
                                   species == "Vac_myr" ~ "Birds",
                                   species == "Vac_uli" ~ "Birds"),
             species = case_when(species == "Ant_dio" ~ "Antennaria dioica",
                                 species == "Ath_dis" ~ "Athyrium distentifolium",
                                 species == "Ave_fle" ~ "Avenella flexuosa",
                                 species == "Cry_cri" ~ "Cryptogramma crispa",
                                 species == "Eri_ang" ~ "Eriophorum angustifolium",
                                 species == "Eri_sch" ~ "Eriophorum scheuchzeri",
                                 species == "Mic_ste" ~ "Micranthes stellaris",
                                 species == "Oma_nor" ~ "Omalotheca norvegica",
                                 species == "Ran_pyg" ~ "Ranunculus pygmaeus",
                                 species == "Vac_myr" ~ "Vaccinium myrtillus",
                                 species == "Vac_uli" ~ "Vaccinium uliginosum"),
             specialisation = ifelse(specialisation == "alpine", "Specialist", "Generalist")) |>
      relocate(c(dispersal, new_1, new_2), .after = specialisation) |>
      clean_ft() |>
      set_header_labels(species = "Species",
                        specialisation = "Specialisation\nclass",
                        dispersal = "Primary dispersal\nmechanism",
                        new_1 = "# Summits new\n1972\u20132008/09",
                        new_2 = "# Summits new\n2008/09\u20132024/25",
                        persisted = "# Summits persisted\n1972\u20132024/25") |>
      fontsize(part = "body", size = 14) |>
      italic(part = "body", j = 1) |>
      hline(i = 4) |>
      align(part = "all", j = 4:6, align = "center") |>
      autofit() |>
      height(part = "header", height = 1) |>
      hrule(part = "header", rule = "exact") |>
      height(part = "body", height = 0.4) |>
      hrule(part = "body", rule = "exact") |>
      line_spacing(part = "header", space = 2)
  ),
  tar_target(
    name = new_species_2024_2025,
    command = filefjell_simplified |>
      pivot_wider(names_from = year, values_from = distance) |>
      filter(is.na(second) & !is.na(third)) |>
      mutate(status = "new") |>
      select(summit, species, status)
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
      mutate(dec_rate = 10 * change / time)
  ),
  tar_target(
    name = priors_t,
    command = c(
      prior(normal(0, 5), class = "Intercept"),
      prior(normal(0, 5), class = "b"),
      prior(exponential(0.3), class = "sd"),
      prior(gamma(2, 0.1), class = "nu")
    )
  ),
  tar_target(
    name = altitude_bay,
    command = brm(
      bf(dec_rate ~
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
  # Rates results----
  tar_target(
    name = richness_figure,
    command = richness_results$emmeans_df |>
    gg_results() +
    scale_x_continuous(limits = c(-3.12, 5.2),
                       labels = NULL) +
    labs(x = NULL, y = adj_label["richness"]) +
    theme(plot.margin = margin(0, 0, 10, 0))
  ),
  tar_target(
    name = new_figure,
    command = new_results$emmeans_df |>
    gg_results() +
    scale_x_continuous(limits = c(-3.12, 5.2),
                       labels = NULL) +
    labs(x = NULL, y = adj_label["new"]) +
    theme(plot.margin = margin(0, 0, 10, 0))
  ),
  tar_target(
    name = lost_figure,
    command = lost_results$emmeans_df |>
    gg_results() +
    scale_x_continuous(limits = c(-3.12, 5.2)) +
    labs(x = "Rate (species&nbsp;summit<sup>-1</sup>&nbsp;decade<sup>-1</sup>)", y = adj_label["lost"]) +
    theme(axis.title.x = element_markdown()) +
    theme(plot.margin = margin(0, 0, 10, 0))
  ),
  tar_target(
    name = altitude_figure,
    command = altitude_results$emmeans_df |>
    gg_results() +
    scale_x_continuous(limits = c(-0.78, 1.3)) +
    labs(x = "Rate (metres&nbsp;summit<sup>-1</sup>&nbsp;decade<sup>-1</sup>)", y = adj_label["altitude"]) +
    theme(axis.title.x = element_markdown(),
          plot.margin = margin(10, 0, 0, 0))
  ),
  tar_target(
    name = rates_figure,
    command = ggarrange(
    plotlist = list(richness_figure, new_figure, lost_figure, altitude_figure),
    ncol = 1,
    nrow = 4,
    align = "v",
    common.legend = TRUE,
    heights = c(1, 1, 1.35, 1.35)
    )
  ),
  # Habitats----
  tar_target(
    name = habitat_area,
    command = habitat_species_clean |>
    select(summit, habitat) |>
    distinct() |>
    full_join(habitat_cover, by = c("summit", "habitat")) |>
    mutate(habitat_decare = ifelse(is.na(habitat_decare), 0.25, habitat_decare)) |>
    summarise(.by = habitat, habitat_decare = sum(habitat_decare)) |>
    mutate(percentage = 100 * habitat_decare / sum(habitat_decare)) |>
    arrange(habitat)
  ),
  tar_target(
    name = habitat_new,
    command = habitat_species_clean |>
    left_join(new_species_2024_2025, by = c("summit", "species")) |>
    filter(status == "new") |>
    select(summit, habitat, specialisation, species) |>
    arrange(summit, habitat, specialisation, species) |>
    summarise(.by = c(habitat, specialisation), total = n()) |>
    arrange(habitat, specialisation)
  ),
  tar_target(
    name = habitat_new_proportions,
    command = habitat_new |>
    pivot_wider(names_from = specialisation, values_from = total, values_fill = 0) |>
    right_join(habitat_area, by = "habitat") |>
    mutate(across(alpine:generalist, ~ ifelse(is.na(.x), 0, .x)),
           total_nr = alpine + generalist,
           total_byha = total_nr / (habitat_decare / 10),
           alpine_byha = alpine / (habitat_decare / 10),
           generalist_byha = generalist / (habitat_decare / 10)) |>
    select(!habitat_decare) |>
    relocate(c(total_nr, total_byha), .after = habitat) |>
    relocate(alpine_byha, .after = alpine) |>
    relocate(generalist_byha, .after = generalist) |>
    mutate(habitat = factor(habitat, levels = c("T1", "T27", "T13", "T14", "T7", "V6", "T3", "T22"))) |>
    arrange(habitat)
  ),
  tar_target(
    name = habitat_new_header,
    command = tibble(
      col_keys = c("habitat",
                   "total",
                   "total_nr",
                   "total_byha",
                   "alpine",
                   "alpine_nr",
                   "alpine_byha",
                   "generalist",
                   "generalist_nr",
                   "generalist_byha",
                   "percentage"),
      header1 = c("Habitat",
                  "Total new occurrences",
                  "Total new occurrences",
                  "Total new occurrences",
                  "New specialists",
                  "New specialists",
                  "New specialists",
                  "New generalists",
                  "New generalists",
                  "New generalists",
                  "% Total\nsummit area"),
      header2 = c("Habitat",
                  "Total new occurrences",
                  "#",
                  "Per_ area",
                  "Specialists",
                  "#",
                  "Per_area",
                  "Generalists",
                  "#",
                  "Per_area",
                  "% Total\nsummit area")
    )
  ),
  tar_target(
    name = habitat_new_proportions_ft,
    command = habitat_new_proportions |>
      mutate(habitat = case_when(habitat == "T1" ~ "Bare rock",
                                 habitat == "T27" ~ "Boulder field",
                                 habitat == "T13" ~ "Scree",
                                 habitat == "T14" ~ "Ridge",
                                 habitat == "T7" ~ "Snowbed",
                                 habitat == "V6" ~ "Wet snowbed",
                                 habitat == "T3" ~ "Alpine heath",
                                 habitat == "T22" ~ "Alpine grassland"),
             across(where(is.numeric), ~ round(., 1))) |>
      rename("alpine_nr" = "alpine", "generalist_nr" = "generalist") |>
      flextable() |>
      separate_header() |>
      set_header_df(habitat_new_header) |>
      compose(part = "header", i = 2, j = c(3, 5, 7),
              value = as_paragraph("# / 10,000 m", as_sup("2"))) |>
      merge_h(part = "header", i = 1) |>
      merge_v(part = "header", j = c(1, 8)) |>
      bg(part = "header", bg = "black") |>
      color(part = "header", color = "white") |>
      bold(part = "header") |>
      bg(part = "body", bg = "white") |>
      color(part = "body", color = "black") |>
      align(part = "all", j = -1, align = "center") |>
      vline(j = c(1, 3, 5, 7)) |>
      flextable::font(part = "all", fontname = "Times New Roman") |>
      fontsize(part = "header", size = 13) |>
      fontsize(part = "body", size = 12) |>
      autofit()
    ),
  tar_target(
    name = habitat_new_proportions_v,
    command = habitat_area |>
      select(habitat) |>
      distinct() |>
      crossing(specialisation = levels(habitat_new$specialisation)) |>
      left_join(habitat_new, by = c("habitat", "specialisation")) |>
      mutate(total = coalesce(total, 0)) |>
      left_join(habitat_area, by = "habitat") |>
      mutate(total_byha = total / (habitat_decare / 10)) |>
      select(!habitat_decare) |>
      relocate(total_byha, .after = total) |>
      mutate(habitat = factor(habitat, levels = c("T1", "T27", "T13", "T14", "T7", "V6", "T3", "T22"))) |>
      arrange(habitat) |>
      mutate(across(where(is.numeric), ~ round(., 1)))
  ),
  tar_target(
    name = habitat_percentage_gg,
    command = habitat_area |>
      mutate(habitat = factor(habitat, levels = c("T1", "T27", "T13", "T14", "T7", "V6", "T3", "T22"))) |>
      ggplot() +
      geom_col(aes(x = habitat, y = percentage)) +
      labs(title = "Estimated % area across all summits",
           x = NULL,
           y = NULL) +
      scale_x_discrete(labels = adj_label) +
      scale_y_continuous(limits = c(0, 38.05)) +
      theme_minimal() +
      theme(text = element_text(size = 11, family = "serif"),
            plot.title = element_text(size = 9, hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.2),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_blank())
  ),
  tar_target(
    name = habitat_new_total_gg,
    command = habitat_new_proportions_v |>
      ggplot() +
      geom_col(aes(x = habitat, y = total, fill = specialisation)) +
      scale_fill_manual("Specialisation", values = colour_mapping$specialisation, labels = adj_label) +
      labs(title = "# New species occurrences",
           x = NULL,
           y = NULL) +
      scale_x_discrete(labels = adj_label) +
      scale_y_continuous(limits = c(0, 38.05)) +
      theme_minimal() +
      theme(text = element_text(size = 11, family = "serif"),
            plot.title = element_text(size = 9, hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.2),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            legend.position = "top",
            legend.title = element_blank(),
            legend.text = element_text(margin = margin(l = 9, r = 20, b = 4)),
            legend.box.margin = margin(l = 180))
  ),
  tar_target(
    name = habitat_new_proportions_gg,
    command = habitat_new_proportions_v |>
      ggplot() +
      geom_col(aes(x = habitat, y = total_byha, fill = specialisation)) +
      scale_fill_manual("Specialisation", values = colour_mapping$specialisation, labels = adj_label) +
      labs(title = expression("# New species occurrences / 10 000 m"^2),
           x = NULL,
           y = NULL) +
      scale_x_discrete(labels = adj_label) +
      scale_y_continuous(limits = c(0, 19.025)) +
      theme_minimal() +
      theme(text = element_text(size = 11, family = "serif"),
            plot.title = element_text(size = 9, hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.2),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_blank())
  ),
  tar_target(
    name = habitat_percentage_new_gg,
    command = ggarrange(habitat_percentage_gg, habitat_new_total_gg, habitat_new_proportions_gg,
                        ncol = 3, align = "h", common.legend = TRUE)
  )
  )

