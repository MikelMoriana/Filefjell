# Created by use_targets().
# Setting up the environment----

# Load packages required to define the pipeline:
library(targets)


# Target options:
tar_option_set(
  packages = c("tidyverse", "janitor", "forcats", "vegan", "glmmTMB"),
  format = "rds", 
  seed = 811
)

# R scripts in the R/ folder with custom functions:
tar_source("functions.R")

# List of targets:
list(
  tar_target(
    name = filefjell_1972_2010_file, 
    command = "Raw_data/Filefjell_1972_2010.csv", 
    format = "file"
  ), 
  tar_target(
    name = filefjell_1972_2010, 
    command = read_csv2(filefjell_1972_2010_file)
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
    name = filefjell_1972_2010_tidy, 
    command = filefjell_1972_2010 |> 
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
    name = filefjell_1972_2010_clean, 
    command = filefjell_1972_2010_tidy |> 
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
    command = filefjell_1972_2010_clean |> 
      rbind(filefjell_2024_clean |> select(year, summit, elevation, species, distance)) |> 
      arrange(year, summit, species)
  )
)
