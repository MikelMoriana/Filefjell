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
  # Tidy files----
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
    name = filefjell_1972_tidy,
    command = filefjell_1972_2009 |>
      filter(Year == 1972) |>
      left_join(visit_dates |>
                  select(!c(Year2:Data2)),
                by = c("Summit", "Year")) |>
      pivot_longer(cols = !c(Summit:Year, Date, Recorder),
                   names_to = "species",
                   values_to = "distance",
                   values_drop_na = TRUE) |>
      data_tidying() |>
      arrange(summit, year, species)
  ),
  tar_target(
    name = filefjell_2008_2009_tidy,
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
      data_tidying() |>
      arrange(summit, year, species)
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
    name = filefjell_2024_2025_tidy,
    command = filefjell_2024 |>
      select(c(Top:Altitude, Rareness)) |>
      rbind(filefjell_2025 |>
              select(!Type) |>
              mutate(Rareness = NA)) |>
      data_tidying() |>
      mutate(distance = height - altitude) |>
      select(!altitude) |>
      relocate(distance, .after = species) |>
      mutate(rareness = ifelse(rareness == "–", NA, rareness)) |>
      arrange(summit, year, species)
  ),
  tar_target(
    name = type_species_tidy,
    command = filefjell_2024 |>
      select(c(Top:Species, Type, Comments, `Svar Mikel`)) |>
      rbind(filefjell_2025 |>
              select(c(Top:Species, Type)) |>
              mutate(Comments = NA, `Svar Mikel` = NA)) |>
      data_tidying() |>
      mutate(main_type = str_extract(type, "[^C]*")) |>
      relocate(main_type, .before = type) |>
      arrange(summit, year, species)
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
    name = polygones_tidy,
    command = polygones |>
      clean_names() |>
      select(fid, kartleggin, layer, area_m2) |>
      mutate(year = 2024) |>
      relocate(year) |>
      rename(summit = layer) |>
      relocate(summit) |>
      mutate(summit = str_replace(summit, "_NiN3", "")) |>
      left_join(filefjell_2024_2025_tidy |>
                  select(summit, date, weather) |>
                  distinct(),
                by = "summit") |>
      relocate(c(date, weather), .after = year) |>
      mutate(recorder = "Helene") |>
      relocate(recorder, .after = weather) |>
      rename(type = kartleggin) |>
      mutate(type = str_replace(type, "-", ""))
  ),
  tar_target(
    name = polygones_cover,
    command = polygones_tidy |>
      summarise(.by = c(summit:recorder, type), area_m2 = sum(area_m2)) |>
      group_by(summit) |>
      mutate(percentage = 100 * area_m2 / sum(area_m2)) |>
      ungroup() |>
      select(!area_m2)
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
    name = type_cover_tidy,
    command = type_cover |>
      data_tidying() |>
      mutate(type = ifelse(type == "Naken berg", "T1", type))
  ),
  tar_target(
    name = maintype_cover_tidy,
    command = type_cover_tidy |>
      rbind(polygones_cover |>
              mutate(comments = NA)) |>
      mutate(main_type = str_extract(type, "[^C]*")) |>
      relocate(main_type, .before = type) |>
      summarise(.by = c(year, summit, date, weather, recorder, main_type),
                percentage = sum(percentage))
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
      rename(summit_hectare = area) |>
      mutate(summit_decare = 10 * summit_hectare) |>
      relocate(summit_decare, .after = summit_hectare)
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
  # Clean files----
  tar_target(
    name = filefjell_1972_clean,
    command = filefjell_1972_tidy |>
      left_join(summit_data_tidy |>
                  select(summit, height) |>
                  rename(height_correct = height),
                by = "summit") |>
      select(!height) |>
      rename(height = height_correct) |>
      relocate(height, .after = summit) |>
      mutate(species = case_when(species == "Cer_lan" ~ "Cer_alp_lan",
                                 species == "Jun_trif" ~ "Jun_tri",
                                 species == "Poa_x_jem" ~ "Poa_jem",
                                 species == "Sil_acu" ~ "Sil_aca",
                                 TRUE ~ species))
  ),
  tar_target(
    name = filefjell_2008_2009_clean,
    command = filefjell_2008_2009_tidy |>
      left_join(summit_data_tidy |>
                  select(summit, height) |>
                  rename(height_correct = height),
                by = "summit") |>
      select(!height) |>
      rename(height = height_correct) |>
      relocate(height, .after = summit) |>
      mutate(species = case_when(species == "Cer_lan" ~ "Cer_alp_lan",
                                 species == "Jun_trif" ~ "Jun_tri",
                                 species == "Poa_x_jem" ~ "Poa_jem",
                                 species == "Sil_acu" ~ "Sil_aca",
                                 TRUE ~ species))
  ),
  tar_target(
    name = filefjell_2024_2025_clean,
    command = filefjell_2024_2025_tidy |>
      left_join(summit_data_tidy |>
                  select(summit, height) |>
                  rename(height_correct = height),
                by = "summit") |>
      select(!height) |>
      rename(height = height_correct) |>
      relocate(height, .after = summit) |>
      mutate(species = case_when(species == "Alc_sp" ~ "Alc_glo",
                                 TRUE ~ species))
  ),
  tar_target(
    name = filefjell_data_clean,
    command = filefjell_1972_clean |>
      rbind(filefjell_2008_2009_clean) |>
      mutate(weather = NA,
             rareness = NA) |>
      relocate(weather, .after = date) |>
      rbind(filefjell_2024_2025_clean) |>
      left_join(filefjell_species, by = "species") |>
      mutate(species = ifelse(!is.na(new_name), new_name, species)) |>
      relocate(c(specialisation, functional), .after = species) |>
      select(!new_name) |>
      mutate(summit = factor(summit, levels = c("Berdalseken", "Suletinden", "Unnamed", "Storeknippa", "Graanosi", "Loppenosi", "Graveggi", "Krekanosi", "Rjupeskareggen", "Frostdalsnosi", "Krekanosi_S", "Slettningseggi", "Krekahoegdi"))) |>
      arrange(summit, year, species)
  ),
  tar_target(
    name = type_species_clean,
    command = type_species_tidy |>
      mutate(species = case_when(species == "Alc_sp" ~ "Alc_glo",
                                 TRUE ~ species)) |>
      left_join(filefjell_species, by = "species") |>
      mutate(species = ifelse(!is.na(new_name), new_name, species)) |>
      relocate(c(specialisation, functional), .after = species) |>
      select(!new_name) |>
      left_join(maintype_cover_tidy |>
                  select(summit, main_type, percentage),
                by = c("summit", "main_type")) |>
      relocate(percentage, .after = main_type) |>
      left_join(summit_data_tidy |>
                  select(summit, height, summit_decare) |>
                  rename(height_correct = height),
                by = "summit") |>
      select(!height) |>
      rename(height = height_correct) |>
      mutate(habitat_decare = summit_decare * percentage / 100) |>
      relocate(c(height, summit_decare), .after = summit) |>
      relocate(habitat_decare, .after = percentage) |>
      mutate(summit = factor(summit, levels = c("Berdalseken", "Suletinden", "Unnamed", "Storeknippa", "Graanosi", "Loppenosi", "Graveggi", "Krekanosi", "Rjupeskareggen", "Frostdalsnosi", "Krekanosi_S", "Slettningseggi", "Krekahoegdi"))) |>
      arrange(summit, year, species)
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
      select(!c(date:recorder, rareness)) |>
      mutate(year = case_when(year == 1972 ~ "first",
                              year %in% c(2008, 2009) ~ "second",
                              year %in% c(2024, 2025) ~ "third"))
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
    name = richrate_mod,
    command = glmmTMB(
      rate ~
        period * specialisation + (1 | summit),
      dispformula = ~period,
      family = gaussian,
      data = richness_rate)
  ),
  tar_target(
    name = richrate_results,
    command = richrate_mod |>
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
    name = richrate10_mod,
    command = glmmTMB(
      rate ~
        period * specialisation + (1 | summit),
      dispformula = ~period,
      family = gaussian,
      data = richness10_rate)
  ),
  tar_target(
    name = richrate10_results,
    command = richrate10_mod |>
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
  )
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
  )
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
  )
  # tar_target(
  #   name = turnover_grouped,
  #   command = turnover_species |> 
  #     summarise(.by = c("specialisation", "development"), total = n() / 2) |>
  #     arrange(development, specialisation)
  # ),
  # tar_target(
  #   name = observations,
  #   command = elevation_wide |> 
  #     select(!c(first:third, period1, period2)) |> 
  #     pivot_longer(cols = c(distance1:distance3), names_to = "year", values_to = "distance") |> 
  #     mutate(year = case_when(year == "distance1" ~ 1972,
  #                             year == "distance2" ~ 2009,
  #                             year == "distance3" ~ 2024)) |> 
  #     filter(!is.na(distance)) |> 
  #     summarise(.by = c(year, specialisation), observations = n()) |> 
  #     arrange(year) |> 
  #     pivot_wider(names_from = year, values_from = observations) |> 
  #     mutate(specialisation = ifelse(specialisation == "alpine", "Alpine", "Generalist")) |> 
  #     rbind(c("extra", "extra1", "extra2", "extra3"), 
  #           c("specialisation", "1972", "2009", "2024")) |> 
  #     row_to_names(row_number = 3, remove_rows_above = FALSE) |> 
  #     mutate(extra = factor(extra, levels = c("specialisation", "Alpine", "Generalist"))) |> 
  #     arrange(extra)
  # ),
  # tar_target(
  #   name = turnover_development,
  #   command = turnover_grouped |> 
  #     pivot_wider(names_from = specialisation, values_from = total) |> 
  #     mutate(status1 = case_when(development %in% c("Remained", "Disappeared2") ~ "Remained",
  #                                development %in% c("Appeared1", "Forth_back") ~ "New",
  #                                development %in% c("Disappeared1", "Back_forth") ~ "Lost",
  #                                development == "Appeared2" ~ NA),
  #            status2 = case_when(development %in% c("Remained", "Appeared1") ~ "Remained",
  #                                development %in% c("Appeared2", "Back_forth") ~ "New",
  #                                development %in% c("Forth_back", "Disappeared2") ~ "Lost",
  #                                development == "Disappeared1" ~ NA)) |> 
  #     relocate(status1, status2) |> 
  #     rbind(c("extra", "extra1", "extras", "extra2", "extra3"), 
  #           c("Status 2009", "Status 2024", "development", "Alpine", "Generalist")) |> 
  #     row_to_names(row_number = 8, remove_rows_above = FALSE) |> 
  #     mutate(extras = factor(extras, levels = c("development", "Remained", "Appeared1", "Forth_back", "Disappeared1", "Back_forth", "Appeared2", "Disappeared2"))) |>
  #     arrange(extras) |> 
  #     select(!extras)
  # ),
  # tar_target(
  #   name = observations_turnover_ft,
  #   command = observations |> 
  #     rbind(turnover_development) |> 
  #     mutate(across(where(is.factor), as.character)) |> 
  #     row_to_names(row_number = 1) |> 
  #     flextable() |> 
  #     bg(part = "header", bg = "black") |> 
  #     color(part = "header", color = "white") |> 
  #     bold(part = "header") |> 
  #     bg(part = "body", bg = "white") |> 
  #     color(part = "body", color = "black") |> 
  #     bg(part = "body", i = 3, bg = "black") |> 
  #     color(part = "body", i = 3, color = "white") |> 
  #     bold(part = "body", i = 3) |> 
  #     align(part = "all", j = 2:4, align = "center") |> 
  #     align(part = "body", i = 3:10, j = 2, align = "left") |> 
  #     flextable::font(part = "all", fontname = "Times New Roman") |> 
  #     autofit()
  # ),
  # # Elevation----
  # tar_target(
  #   name = elevation_wide_new,
  #   command = elevation_wide |>
  #     mutate(adj_dist1 = ifelse(is.na(distance1) & !is.na(distance2), 33, distance1),
  #            adj_dist2 = ifelse(is.na(distance2) & !is.na(distance3), 33, distance2),
  #            adj_dist3 = ifelse(is.na(distance3) & is.na(distance1) & !is.na(distance2), 33, distance3))
  # ),
  # tar_target(
  #   name = elerate_all,
  #   command = elevation_wide |>
  #     mutate(change1 = distance1 - distance2,
  #            change2 = distance2 - distance3) |>
  #     pivot_longer(cols = c(period1, period2), names_to = "period", values_to = "years") |>
  #     mutate(period = as.factor(period)) |>
  #     mutate(change = case_when(period == "period1" ~ change1,
  #                               period == "period2" ~ change2)) |>
  #     filter(!is.na(change)) |>
  #     mutate(rate = change / years)
  # ),
  # tar_target(
  #   name = priors_t,
  #   command = c(
  #     prior(normal(0, 0.5), class = "Intercept"),
  #     prior(normal(0, 0.5), class = "b"),
  #     prior(exponential(3), class = "sd"),
  #     prior(gamma(2, 0.4), class = "nu"),
  #     prior(normal(-1.098612, 0.5), class = "Intercept", dpar = "sigma"), 
  #     prior(normal(0, 0.3), class = "b", dpar = "sigma")
  #   )
  # ),
  # tar_target(
  #   name = elerate_all_bayest,
  #   command = brm(
  #     bf(rate ~ 
  #          period * specialisation + (1|summit) + (1|species),
  #        sigma ~ period),
  #     family = student(),
  #     prior = priors_t,
  #     data = elerate_all,
  #     chains = 4, iter = 4000, seed = 811,
  #     control = list(adapt_delta = 0.95)
  #   )
  # ),
  # tar_target(
  #   name = elerate_all_bayes,
  #   command = brm(
  #     bf(rate ~
  #          period * specialisation + (1|summit) + (1|species),
  #        sigma ~period),
  #     family = student(),
  #     prior = c(
  #       prior(normal(0, 0.5), class = "b"),
  #       prior(normal(0, 0.5), class = "Intercept"),
  #       prior(gamma(46, 1), class = "nu")
  #     ),
  #     data = elerate_all,
  #     control = list(adapt_delta = 0.999),
  #     seed = 811
  #   )
  # ),
  # tar_target(
  #   name = elerate_all_results,
  #   command = elerate_all_bayes |>
  #     mod_summary()
  # ),
  # tar_target(
  #   name = elerate_remained,
  #   command = elevation_wide |>
  #     mutate(change1 = distance1 - distance2,
  #            change2 = distance2 - distance3) |>
  #     filter(!is.na(change1) & !is.na(change2)) |>
  #     pivot_longer(cols = c(period1, period2), names_to = "period", values_to = "years") |>
  #     mutate(period = as.factor(period)) |>
  #     mutate(change = case_when(period == "period1" ~ change1,
  #                               period == "period2" ~ change2)) |>
  #     mutate(rate = change / years) |>
  #     select(!c(change1, change2))
  # ),
  # tar_target(
  #   name = elerate_rem_bayes,
  #   command = brm(
  #     bf(rate ~
  #          period * specialisation + (1|summit) + (1|species),
  #        sigma ~period),
  #     family = student(),
  #     prior = c(
  #       prior(normal(0, 0.5), class = "b"),
  #       prior(normal(0, 0.5), class = "Intercept"),
  #       prior(gamma(45, 1), class = "nu")
  #     ),
  #     data = elerate_remained,
  #     control = list(adapt_delta = 0.999),
  #     seed = 811
  #   )
  # ),
  # tar_target(
  #   name = elerate_rem_results,
  #   command = elerate_rem_bayes |>
  #     mod_summary()
  # ),
  # tar_target(
  #   name = elerate_new,
  #   command = elevation_wide_new |>
  #     mutate(change1 = adj_dist1 - adj_dist2,
  #            change2 = adj_dist2 - distance3) |>
  #     filter(!is.na(change1) & !is.na(change2)) |>
  #     pivot_longer(cols = c(period1, period2), names_to = "period", values_to = "years") |>
  #     mutate(period = as.factor(period)) |>
  #     mutate(change = case_when(period == "period1" ~ change1,
  #                               period == "period2" ~ change2)) |>
  #     mutate(rate = change / years) |>
  #     select(!c(change1, change2))
  # ),
  # tar_target(
  #   name = elerate_new_bayes,
  #   command = brm(
  #     bf(rate ~
  #          period * specialisation + (1|summit) + (1|species),
  #        sigma ~period),
  #     family = student(),
  #     data = elerate_new,
  #     control = list(adapt_delta = 0.999),
  #     seed = 811
  #   )
  # ),
  # tar_target(
  #   name = elerate_new_results,
  #   command = elerate_new_bayes |>
  #     mod_summary()
  # )
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

