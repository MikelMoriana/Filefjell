# Created by use_targets().
# Setting up the environment----

# Load packages required to define the pipeline:
library(targets)


# Target options:
tar_option_set(
  packages = c("tidyverse", "janitor", "forcats", "vegan", "glmmTMB", "lme4", "splines"),
  format = "rds", 
  seed = 811
)

# R scripts in the R/ folder with custom functions:
tar_source("Scripts/0_setup.R")

# List of targets:----
list(
  # Tidy files----
  tar_target(
    name = filefjell_visit_dates_file, 
    command = "data_raw/Visit_dates.csv", 
    format = "file"
  ), 
  tar_target(
    name = filefjell_visit_dates, 
    command = read_csv2(filefjell_visit_dates_file)
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
      left_join(filefjell_visit_dates |> 
                  select(!c(Year2:Data2)), 
                by = c("Summit", "Year")) |> 
      pivot_longer(cols = !c(Summit:Year, Date, Recorder), 
                   names_to = "species", 
                   values_to = "distance", 
                   values_drop_na = TRUE) |> 
      data_tidying()
  ),
  tar_target(
    name = filefjell_2008_2009_tidy,
    command = filefjell_1972_2009 |> 
      filter(Year == 2009) |> 
      select(!Year) |> 
      left_join(filefjell_visit_dates |> 
                  filter(Year %in% c(2008, 2009)) |> 
                  select(!c(Year2:Data2)), 
                by = c("Summit")) |> 
      pivot_longer(cols = !c(Summit, Height, Year:Recorder), 
                   names_to = "species", 
                   values_to = "distance", 
                   values_drop_na = TRUE) |> 
      data_tidying()
  ),
  tar_target(
    name = filefjell_1972_2008_2009_tidy,
    command = filefjell_1972_tidy |> 
      rbind(filefjell_2008_2009_tidy) |> 
      arrange(desc(elevation), year, species)
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
    name = filefjell_2024_tidy,
    command = filefjell_2024 |> 
      data_tidying() |> 
      mutate(distance = elevation - altitude) |> 
      select(!altitude) |> 
      relocate(distance, .after = species) |> 
      mutate(main_type = str_extract(type, "[^C]*")) |> 
      relocate(main_type, .before = type)
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
    name = filefjell_2025_tidy,
    command = filefjell_2025 |> 
      data_tidying() |> 
      mutate(distance = elevation - altitude) |> 
      select(!altitude) |> 
      relocate(distance, .after = species) |> 
      mutate(main_type = str_extract(type, "[^C]*")) |> 
      relocate(main_type, .before = type)
  ),
  tar_target(
    name = filefjell_2024_2025_tidy,
    command = filefjell_2024_tidy |> 
      select(year:type) |> 
      rbind(filefjell_2025_tidy) |> 
      arrange(desc(elevation), year, species)
  ),
  tar_target(
    name = filefjell_summit_data_file,
    command = "data_raw/Summit_data.csv",
    format = "file"
  ),
  tar_target(
    name = filefjell_summit_data,
    command = read_csv2(filefjell_summit_data_file)
  ),
  tar_target(
    name = filefjell_summit_data_tidy,
    command = filefjell_summit_data |> 
      clean_names() |> 
      mutate(summit = str_replace_all(summit, " ", "_"))
  ),
  tar_target(
    name = filefjell_type_cover_file,
    command = "data_raw/Type_cover.csv",
    format = "file"
  ),
  tar_target(
    name = filefjell_type_cover,
    command = read_csv2(filefjell_type_cover_file)
  ),
  tar_target(
    name = filefjell_type_cover_tidy,
    command = filefjell_type_cover |> 
      clean_names() |> 
      relocate(year) |> 
      mutate(date = dmy(date), 
             recorder = str_replace_all(recorder, " \\+ ", "_"), 
             type = ifelse(type == "Naken berg", "T1", type)) |> 
      rename(weather = vaer, 
             cover = percentage) |> 
      mutate(weather = str_replace_all(weather, c(" \\+ " = "_", ", " = "_", " " = "_")))
  ),
  tar_target(
    name = filefjell_maintype_cover_tidy,
    command = filefjell_type_cover_tidy |> 
      mutate(main_type = str_extract(type, "[^C]*")) |> 
      relocate(main_type, .before = type) |> 
      summarise(.by = c(year, summit, date, recorder, main_type), 
                cover = sum(cover))
  ),
  # Clean files----
  tar_target(
    name = filefjell_1972_2008_2009_clean,
    command = filefjell_summit_data_tidy |>
      mutate(elevation_correct = elevation) |>
      select(summit, elevation_correct) |>
      right_join(filefjell_1972_2008_2009_tidy, by = "summit") |>
      relocate(year) |> 
      select(!elevation) |>
      rename(elevation = elevation_correct) |>
      mutate(species = case_when(species == "Jun_trif" ~ "Jun_tri",
                                 species == "Poa_x_jem" ~ "Poa_jem",
                                 species == "Sil_acu" ~ "Sil_aca",
                                 TRUE ~ species))
  ),
  tar_target(
    name = filefjell_2024_2025_clean,
    command = filefjell_summit_data_tidy |>
      mutate(elevation_correct = elevation) |>
      select(summit, elevation_correct) |>
      right_join(filefjell_2024_2025_tidy, by = "summit") |>
      relocate(year) |> 
      select(!elevation) |> 
      rename(elevation = elevation_correct) |> 
      relocate(elevation, .after = summit) |> 
      mutate(species = case_when(species == "Alc_sp" ~ "Alc_glo", 
                                 species == "Cer_alp_lan" ~ "Cer_lan", 
                                 TRUE ~ species)) |> 
      left_join(filefjell_maintype_cover_tidy |> select(summit, main_type, cover), by = c("summit", "main_type")) |> 
      relocate(cover, .after = type)
  ),
  tar_target(
    name = filefjell_data_clean,
    command = filefjell_2024_2025_clean |> 
      select(!c(weather, main_type, type, cover)) |> 
      rbind(filefjell_1972_2008_2009_clean) |> 
      mutate(summit = factor(summit, levels = c("Berdalseken", "Suletinden", "Unnamed", "Storeknippa", "Graanosi", "Loppenosi", "Graveggi", "Krekanosi", "Rjupeskareggen", "Frostdalsnosi", "Krekanosi_S", "Slettningseggi", "Krekahoegdi"))) |> 
      arrange(summit, year, species)
  ),
  # Elevation----
  tar_read(
    name = filefjell_visit_years, 
    command = filefjell_data_clean |> 
      select(year, summit) |> 
      distinct() |> 
      pivot_wider(names_from = year, names_prefix = "y", values_from = year) |> 
      mutate(first = 1972, 
             second = coalesce(y2008, y2009),
             third = coalesce(y2024, y2025)) |> 
      select(summit, first, second, third)
  ),
  tar_target(
    name = elevation_species,
    command = filefjell_data_clean |> 
      select(!c(date, recorder)) |> 
      pivot_wider(names_from = year, names_prefix = "y", values_from = distance) |> 
      left_join(filefjell_visit_years, by = "summit") |> 
      mutate(third = ifelse(!is.na(y2025), 2025, third), 
             distance1 = y1972,
             distance2 = coalesce(y2008, y2009), 
             distance3 = coalesce(y2024, y2025)) |> 
      select(!c(y1972, y2008, y2009, y2024, y2025)) |> 
      mutate(period1 = second - first, 
             period2 = third - second)
  ),
  tar_target(
    name = elevation_species_new,
    command = elevation_species |> 
      mutate(adj_dist1 = ifelse(is.na(distance1) & !is.na(distance2), 33, distance1),
             adj_dist2 = ifelse(is.na(distance2) & !is.na(distance3), 33, distance2))
  ),
  tar_target(
    name = elerate_all,
    command = elevation_species |>
      mutate(change1 = distance1 - distance2, 
             change2 = distance2 - distance3) |> 
      filter(!is.na(change1) & !is.na(change2)) |>
      pivot_longer(cols = c(period1, period2), names_to = "period", values_to = "years") |>
      mutate(period = as.factor(period)) |>
      mutate(change = case_when(period == "period1" ~ change1,
                                period == "period2" ~ change2)) |>
      mutate(rate = change / years) |>
      select(!c(change1, change2))
  ),
  tar_target(
    name = elerate_new,
    command = elevation_species_new |>
      mutate(change1 = adj_dist1 - adj_dist2, 
             change2 = adj_dist2 - distance3) |> 
      filter(!is.na(change1) & !is.na(change2)) |>
      pivot_longer(cols = c(period1, period2), names_to = "period", values_to = "years") |>
      mutate(period = as.factor(period)) |>
      mutate(change = case_when(period == "period1" ~ change1,
                                period == "period2" ~ change2)) |>
      mutate(rate = change / years) |>
      select(!c(change1, change2))
  ),
  # tar_target(
  #   name = elevation_change_rate_three_mod,
  #   command = glmmTMB(
  #     elevation_change_rate ~ 
  #       period + (1 | summit) + (1 | species), 
  #     family = gaussian, 
  #     data = elevation_change_three_data)
  # ),
  tar_target(
    name = elerate_new_bayes,
    command = brm(
      bf(rate ~ 
           period + (1|summit) + (1|species),
         sigma ~ period),
      family = student(), 
      data = elerate_new,
      seed = 811)
  )
  # # Richness----
  # tar_target(
  #   name = richness_data,
  #   command = filefjell_data_clean |> 
  #     filter(summit != "Krekanosi_S") |> # TO REMOVE WHEN WE GET DATA FROM KREKANOSI S
  #     summarise(.by = c(year, summit, elevation),
  #               richness = n())
  # ), 
  # tar_target(
  #   name = richness_change_data, 
  #   command = richness_data |> 
  #     pivot_wider(names_from = year, names_prefix = "y", values_from = richness) |> 
  #     mutate(per1 = y2009 - y1972, 
  #            per2 = y2024 - y2009) |> 
  #     select(-c(y1972, y2009, y2024)) |> 
  #     pivot_longer(cols = c(per1, per2), names_to = "period", values_to = "richness_change") |> 
  #     mutate(richness_change_rate = case_when(period == "per1" ~ richness_change / 37, 
  #                                             period == "per2" ~ richness_change / 15)) |> 
  #     mutate(period = as.factor(period))
  # ), 
  # tar_target(
  #   name = richness_change_rate_mod, 
  #   command = glmmTMB(
  #     richness_change_rate ~ 
  #       period + (1 | summit), 
  #     family = gaussian, 
  #     data = richness_change_data)
  # ),
  # # Turnover----
  # tar_target(
  #   name = species_turnover_data,
  #   command = filefjell_data_clean |> 
  #     filter(summit != "Krekanosi_S") |> # TO REMOVE WHEN WE GET DATA FROM KREKANOSI S
  #     mutate(presence = ifelse(!is.na(distance), 1, 0)) |> 
  #     select(-distance) |> 
  #     arrange(species) |> 
  #     pivot_wider(names_from = "year", names_prefix = "y", values_from = "presence", values_fill = 0) |> 
  #     arrange(summit, species) |> 
  #     mutate(turnover09 = y2009 - y1972) |> 
  #     mutate(turnover24 = y2024 - y2009)
  #   ),
  # tar_target(
  #   name = turnover_data,
  #   command = species_turnover_data |> 
  #     summarise(.by = summit, 
  #               per1_lost = sum(turnover09 == -1), 
  #               per1_nochange = sum(turnover09 == 0), 
  #               per1_new = sum(turnover09 == 1), 
  #               per2_lost = sum(turnover24 == -1), 
  #               per2_nochange = sum(turnover24 == 0), 
  #               per2_new = sum(turnover24 == 1)) |> 
  #     pivot_longer(cols = starts_with("per"), 
  #                  names_to = c("period", ".value"), 
  #                  names_sep = "_") |> 
  #     mutate(number_years = ifelse(period == "per1", 37, 15)) |> 
  #     mutate(lost_rate = lost / number_years, 
  #            new_rate = new / number_years) |> 
  #     mutate(period = as.factor(period))
  # ),
  # tar_target(
  #   name = new_rate_mod, 
  #   command = glmmTMB(
  #     new_rate*555 ~ 
  #       period + (1 | summit), 
  #     family = nbinom2, 
  #     data = turnover_data)
  # ),
  # tar_target(
  #   name = lost_rate_mod, 
  #   command = glmmTMB(
  #     lost_rate*555 ~ 
  #       period + (1 | summit), 
  #     family = nbinom2, 
  #     ziformula = ~1, 
  #     data = turnover_data)
  # ),
  # # New species per nature type
  # tar_target(
  #   name = colonizers_data,
  #   command = filefjell_2024_clean |>
  #     semi_join(
  #       species_turnover_data |>
  #         filter(turnover24 == 1) |>
  #         select(summit:species)) |> 
  #     mutate(hovedtype = str_sub(type, start = 1L, end = 2L)) |> 
  #     relocate(hovedtype, .before = type)
  # )
)
