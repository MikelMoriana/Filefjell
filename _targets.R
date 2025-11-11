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
  # Data cleaning----
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
    name = filefjell_1972_2009_tidy, 
    command = filefjell_1972_2009 |> 
      relocate(Year) |> 
      mutate(Summit = str_replace_all(Summit, " ", "_")) |> 
      rename(Elevation = Height) |> 
      pivot_longer(cols = -c(Year:Elevation), names_to = "species", values_to = "distance") |> 
      clean_names() |> 
      mutate(species = str_replace_all(species, c(" " = "_", "\\." = ""))) |> 
      filter(!is.na(distance))
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
      clean_names() |> 
      relocate(year) |> 
      rename(summit = top) |> 
      mutate(date = dmy(date)) |> 
      rename(weather = vaer) |> 
      mutate(weather = str_replace_all(weather, c(" \\+ " = "_", ", " = "_", " " = "_", "/" = "_"))) |> 
      mutate(recorder = str_replace_all(recorder, " \\+ ", "_")) |> 
      rename(elevation = top_height) |> 
      mutate(distance = elevation - altitude) |> 
      select(-altitude) |> 
      relocate(distance, .after = species) |> 
      mutate(main_type = str_extract(type, "[^C]*")) |> 
      relocate(main_type, .before = type)
  ), 
  tar_target(
    name = filefjell_summit_data_file, 
    command = "data_raw/Summit_data.csv", 
    format = "file"
  ), 
  tar_target(
    name = filefjell_summit_data, 
    command = read_csv(filefjell_summit_data_file)
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
      rename(cover = percentage)
  ), 
  tar_target(
    name = filefjell_main_type_cover, 
    command = filefjell_type_cover_tidy |> 
      mutate(main_type = str_extract(type, "[^C]*")) |> 
      relocate(main_type, .before = type) |> 
      summarise(.by = c(year, summit,date, recorder, main_type), 
                cover = sum(cover))
  ), 
  tar_target(
    name = filefjell_1972_2009_clean, 
    command = filefjell_1972_2009_tidy |> 
      left_join(filefjell_summit_data_tidy |> select(summit, elevation), by = "summit", suffix = c("", "_correct")) |> 
      select(!elevation) |> 
      rename(elevation = elevation_correct) |> 
      relocate(elevation, .after = summit) |> 
      mutate(species = case_when(species == "Cer_lan" ~ "Cer_alp_lan", 
                                 species == "Jun_trif" ~ "Jun_tri", 
                                 species == "Poa_x_jem" ~ "Poa_jem", 
                                 species == "Sil_acu" ~ "Sil_aca", 
                                 TRUE ~ species))
  ), 
  tar_target(
    name = filefjell_2024_clean, 
    command = filefjell_2024_tidy |> 
      left_join(filefjell_summit_data_tidy |> select(summit, elevation), by = "summit", suffix = c("", "_correct")) |> 
      select(!elevation) |> 
      rename(elevation = elevation_correct) |> 
      relocate(elevation, .after = summit) |> 
      mutate(species = case_when(species == "Alc_sp" ~ "Alc_glo", 
                                 TRUE ~ species)) |> 
      left_join(filefjell_main_type_cover |> select(summit, main_type, cover), by = c("summit", "main_type")) |> 
      relocate(cover, .after = type)
  ), 
  tar_target(
    name = filefjell_data_clean, 
    command = filefjell_1972_2009_clean |> 
      rbind(filefjell_2024_clean |> select(year, summit, elevation, species, distance)) |> 
      arrange(desc(elevation), year, species) |> 
      mutate(summit = factor(summit, levels = c("Berdalseken", "Suletinden", "Unnamed", "Storeknippa", "Graanosi", "Loppenosi", "Graveggi", "Krekanosi", "Rjupeskareggen", "Frostdalsnosi", "Krekanosi_S", "Slettningseggi", "Krekahoegdi")))
  ), 
  # Elevation----
  tar_target(
    name = elevation_change_three_data, 
    command = filefjell_data_clean |> 
      pivot_wider(names_from = year, names_prefix = "y", values_from = distance) |> 
      filter(!is.na(y1972) & !is.na(y2009) & !is.na(y2024)) |> 
      mutate(period1 = y2009 - y1972, 
             period2 = y2024 - y2009) |> 
      select(-c(y1972, y2009, y2024)) |>
      pivot_longer(cols =c("period1", "period2"), names_to = "period", values_to = "distance_change") |> 
      mutate(elevation_change = distance_change * -1) |> 
      select(-distance_change) |> 
      mutate(elevation_change_rate = case_when(period == "period1" ~ elevation_change / (2009 - 1972), 
                                               period == "period2" ~ elevation_change / (2024 - 2009))) |> 
      mutate(period = as.factor(period)) |> 
      arrange(summit, species, period)
  ), 
  tar_target(
    name = elevation_change_new_data,
    command = filefjell_data_clean |> 
      pivot_wider(names_from = year, names_prefix = "y", values_from = distance) |> 
      mutate(period1 = case_when(is.na(y2009) ~ NA, 
                                 !is.na(y2009) & is.na(y1972) ~ y2009 - 33, 
                                 !is.na(y2009) & !is.na(y1972) ~ y2009 - y1972), 
             period2 = case_when(is.na(y2024) ~ NA, 
                                 !is.na(y2024) & is.na(y2009) ~ y2024 - 33, 
                                 !is.na(y2024) & !is.na(y2009) ~ y2024 - y2009)) |> 
      select(-c(y1972, y2009, y2024)) |> 
      pivot_longer(cols =c("period1", "period2"), names_to = "period", values_to = "distance_change") |> 
      mutate(elevation_change = distance_change * -1) |> 
      select(-distance_change) |> 
      mutate(elevation_change_rate = case_when(period == "period1" ~ elevation_change / 37, 
                                               period == "period2" ~ elevation_change / 15)) |> 
      filter(!is.na(elevation_change)) |> 
      mutate(period = as.factor(period)) |> 
      arrange(summit, species, period)
  ),
  tar_target(
    name = elevation_change_rate_three_mod,
    command = glmmTMB(
      elevation_change_rate ~ 
        period + (1 | summit) + (1 | species), 
      family = gaussian, 
      data = elevation_change_three_data)
  ),
  tar_target(
    name = elevation_change_rate_new_mod,
    command = glmmTMB(
      elevation_change_rate ~ 
        period + (1 | summit) + (1 | species), 
      family = gaussian, 
      data = elevation_change_new_data)
  ),
  # Richness----
  tar_target(
    name = richness_data,
    command = filefjell_data_clean |> 
      filter(summit != "Krekanosi_S") |> # TO REMOVE WHEN WE GET DATA FROM KREKANOSI S
      summarise(.by = c(year, summit, elevation),
                richness = n())
  ), 
  tar_target(
    name = richness_change_data, 
    command = richness_data |> 
      pivot_wider(names_from = year, names_prefix = "y", values_from = richness) |> 
      mutate(per1 = y2009 - y1972, 
             per2 = y2024 - y2009) |> 
      select(-c(y1972, y2009, y2024)) |> 
      pivot_longer(cols = c(per1, per2), names_to = "period", values_to = "richness_change") |> 
      mutate(richness_change_rate = case_when(period == "per1" ~ richness_change / 37, 
                                              period == "per2" ~ richness_change / 15)) |> 
      mutate(period = as.factor(period))
  ), 
  tar_target(
    name = richness_change_rate_mod, 
    command = glmmTMB(
      richness_change_rate ~ 
        period + (1 | summit), 
      family = gaussian, 
      data = richness_change_data)
  ),
  # Turnover----
  tar_target(
    name = species_turnover_data,
    command = filefjell_data_clean |> 
      filter(summit != "Krekanosi_S") |> # TO REMOVE WHEN WE GET DATA FROM KREKANOSI S
      mutate(presence = ifelse(!is.na(distance), 1, 0)) |> 
      select(-distance) |> 
      arrange(species) |> 
      pivot_wider(names_from = "year", names_prefix = "y", values_from = "presence", values_fill = 0) |> 
      arrange(summit, species) |> 
      mutate(turnover09 = y2009 - y1972) |> 
      mutate(turnover24 = y2024 - y2009)
    ),
  tar_target(
    name = turnover_data,
    command = species_turnover_data |> 
      summarise(.by = summit, 
                per1_lost = sum(turnover09 == -1), 
                per1_nochange = sum(turnover09 == 0), 
                per1_new = sum(turnover09 == 1), 
                per2_lost = sum(turnover24 == -1), 
                per2_nochange = sum(turnover24 == 0), 
                per2_new = sum(turnover24 == 1)) |> 
      pivot_longer(cols = starts_with("per"), 
                   names_to = c("period", ".value"), 
                   names_sep = "_") |> 
      mutate(number_years = ifelse(period == "per1", 37, 15)) |> 
      mutate(lost_rate = lost / number_years, 
             new_rate = new / number_years) |> 
      mutate(period = as.factor(period))
  ),
  tar_target(
    name = new_rate_mod, 
    command = glmmTMB(
      new_rate*555 ~ 
        period + (1 | summit), 
      family = nbinom2, 
      data = turnover_data)
  ),
  tar_target(
    name = lost_rate_mod, 
    command = glmmTMB(
      lost_rate*555 ~ 
        period + (1 | summit), 
      family = nbinom2, 
      ziformula = ~1, 
      data = turnover_data)
  ),
  # New species per nature type
  tar_target(
    name = colonizers_data,
    command = filefjell_2024_clean |>
      semi_join(
        species_turnover_data |>
          filter(turnover24 == 1) |>
          select(summit:species)) |> 
      mutate(hovedtype = str_sub(type, start = 1L, end = 2L)) |> 
      relocate(hovedtype, .before = type)
  )
)
