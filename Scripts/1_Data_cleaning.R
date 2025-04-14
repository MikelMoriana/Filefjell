# Libraries and data----

library(tidyverse)
library(janitor)

filefjell_1972_2010 <- read_csv2("Raw_data/Filefjell_1972_2010.csv")
filefjell_2024 <- read_csv2("Raw_data/Filefjell_2024.csv")


# We clean and make the 1972 and 2010 data long

filefjell_1972_2010_tidy <- filefjell_1972_2010 |> 
  relocate(Year) |> 
  mutate(Summit = str_replace_all(Summit, " ", "_")) |> 
  rename(Elevation = Height) |> 
  pivot_longer(cols = -c(Year:Elevation), names_to = "species", values_to = "distance") |> 
  clean_names() |> 
  mutate(species = str_replace_all(species, c(" " = "_", "\\." = ""))) |> 
  filter(!is.na(distance)) |> 
  arrange(year, summit, species)


# We did not get exactly the same altitude for all summits in 2024 as in the previous year (more accurate gps?). We standardize the values by calculating distance from top in 2024

filefjell_2024_tidy <- filefjell_2024 |> 
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

filefjell_2024_metadata <- filefjell_2024_tidy |> 
  select(date, vaer)
