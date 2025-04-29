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
tar_source("functions.R")

# List of targets:
list(
  # Data cleaning----
  tar_target(
    name = filefjell_1972_2009_file, 
    command = "Raw_data/Filefjell_1972_2009.csv", 
    format = "file"
  ), 
  tar_target(
    name = filefjell_1972_2009, 
    command = read_csv2(filefjell_1972_2009_file)
  ), 
  tar_target(
    name = filefjell_2024_file, 
    command = "Raw_data/Filefjell_2024.csv", 
    format = "file"
  ), 
  tar_target(
    name = filefjell_2024, 
    command = read_csv2(filefjell_2024_file)
  ), 
  tar_target(
    name = filefjell_summit_data_file, 
    command = "Raw_data/Summit_data.csv", 
    format = "file"
  ), 
  tar_target(
    name = filefjell_summit_data, 
    command = read_csv(filefjell_summit_data_file)
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
    name = filefjell_2024_tidy, 
    command = filefjell_2024 |> 
      clean_names() |> 
      relocate(year) |> 
      rename(summit = top) |> 
      mutate(date = dmy(date)) |> 
      mutate(vaer = str_replace_all(vaer, c(" \\+ " = "_", ", " = "_", " " = "_", "/" = "_"))) |> 
      mutate(recorder = str_replace_all(recorder, " \\+ ", "_")) |> 
      rename(elevation = top_height) |> 
      mutate(distance = elevation - altitude) |> 
      select(-altitude) |> 
      relocate(distance, .after = species)
  ), 
  tar_target(
    name = filefjell_summit_data_tidy, 
    command = filefjell_summit_data |> 
      clean_names() |> 
      mutate(summit = str_replace_all(summit, " ", "_"))
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
                                 TRUE ~ species))
  ), 
  tar_target(
    name = filefjell_all_years, 
    command = filefjell_1972_2009_clean |> 
      rbind(filefjell_2024_clean |> select(year, summit, elevation, species, distance)) |> 
      arrange(desc(elevation), year, species)
  ), 
  # Distance to top----
  tar_target(
    name = filefjell_distance, 
    command = filefjell_all_years |> 
      pivot_wider(names_from = year, names_prefix = "y", values_from = distance) |> 
      filter(!is.na(y1972) & !is.na(y2009) & !is.na(y2024)) |> 
      pivot_longer(cols = c("y1972", "y2009", "y2024"), names_prefix = "y", names_to = "year", values_to = "distance") |> 
      relocate(year) |> 
      mutate(year = as.numeric(year))
  ), 
  tar_target(
    name = filefjell_distance_change, 
    command = filefjell_distance |> 
      pivot_wider(names_from = year, names_prefix = "y", values_from = distance) |> 
      mutate(int1 = y2009 - y1972, 
             int2 = y2024 - y2009) |> 
      select(-c(y1972, y2009, y2024)) |> 
      pivot_longer(cols =c("int1", "int2"), names_to = "interval", values_to = "distance_change") |> 
      mutate(interval = as.factor(interval))
  ), 
  tar_target(
    name = distance_change_model, 
    command = glmmTMB(
      distance_change ~ 
        interval + (1 | summit) + (1 | species), 
      family = gaussian, 
      data = filefjell_distance_change)
  ), 
  # Richness----
  tar_target(
    name = filefjell_richness, 
    command = filefjell_all_years |> 
      summarise(.by = c(year, summit, elevation), 
                richness = n())
  ), 
  tar_target(
    name = richness_mod, 
    command = glmer(
      richness ~ 
        bs(year, knots = c(2009)) + 
        (1 | summit), 
      family = poisson, 
      data = filefjell_richness)
  ), 
  # Turnover----
  tar_target(
    name = filefjell_turnover, 
    command = filefjell_all_years |> 
      mutate(presence = ifelse(!is.na(distance), 1, 0)) |> 
      select(-distance) |> 
      arrange(species) |> 
      pivot_wider(names_from = "year", names_prefix = "y", values_from = "presence", values_fill = 0) |> 
      arrange(summit, species) |> 
      mutate(turnover09 = y2009 - y1972) |> 
      mutate(turnover24 = y2024 - y2009) |> 
      summarise(.by = summit, 
                int1_lost = sum(turnover09 == -1), 
                int1_nochange = sum(turnover09 == 0), 
                int1_new = sum(turnover09 == 1), 
                int2_lost = sum(turnover24 == -1), 
                int2_nochange = sum(turnover24 == 0), 
                int2_new = sum(turnover24 == 1)) |> 
      pivot_longer(cols = starts_with("int"), 
                   names_to = c("interval", ".value"), 
                   names_sep = "_") |> 
      mutate(number_years = ifelse(interval == "int1", 37, 15)) |> 
      mutate(lost_rate = lost / number_years, 
             new_rate = new / number_years)
  ), 
  tar_target(
    name = filefjell_lost_aov, 
    command = aov(lost_rate ~ interval, data = filefjell_turnover)
  ), 
  tar_target(
    name = filefjell_new_aov, 
    command = aov(new_rate ~ interval, data = filefjell_turnover)
  )
)
