# Libraries----

source("Scripts/0_setup.R")

library(tidyverse)
library(janitor)



# Data----

filefjell_1972_2009 <- read_csv2("Raw_data/Filefjell_1972_2009.csv")
filefjell_2024 <- read_csv2("Raw_data/Filefjell_2024.csv")
filefjell_2025 <- read_csv2("Raw_data/Filefjell_2025.csv")
filefjell_summit_data <- read_csv2("Raw_data/Summit_data.csv")
filefjell_type_cover <- read_csv2("Raw_data/Type_cover.csv")
filefjell_dahlr <- read_csv2("Raw_data/DahlR_values.csv")



# Tidying the data----

# We tidy and make the 1972_2009 data long

filefjell_1972_2009_tidy <- filefjell_1972_2009 |> 
  relocate(Year) |> 
  mutate(Summit = str_replace_all(Summit, " ", "_")) |> 
  rename(Elevation = Height) |> 
  pivot_longer(cols = -c(Year:Elevation), names_to = "species", values_to = "distance") |> 
  clean_names() |> 
  mutate(species = str_replace_all(species, c(" " = "_", "\\." = ""))) |> 
  filter(!is.na(distance)) |> 
  arrange(desc(elevation), year, species)

# We tidy the 2024 data and calculate distance to summit

filefjell_2024_tidy <- filefjell_2024 |> 
  clean_names() |> 
  relocate(year) |> 
  rename(summit = top) |> 
  mutate(date = dmy(date)) |> 
  rename(weather = vaer) |> 
  mutate(weather = str_replace_all(weather, c(" \\+ " = "_", ", " = "_", " " = "_", "/" = "_")), 
         recorder = str_replace_all(recorder, " \\+ ", "_")) |> 
  rename(elevation = top_height) |> 
  mutate(distance = elevation - altitude) |> 
  select(-altitude) |> 
  relocate(distance, .after = species) |> 
  mutate(main_type = str_extract(type, "[^C]*")) |> 
  relocate(main_type, .before = type) |> 
  arrange(desc(elevation), species)

# We tidy the 2025 data and calculate distance to summit

filefjell_2025_tidy <- filefjell_2025 |> 
  clean_names() |> 
  relocate(year) |> 
  rename(summit = top) |> 
  mutate(date = dmy(date)) |> 
  rename(weather = vaer) |> 
  mutate(weather = str_replace_all(weather, c(" \\+ " = "_", ", " = "_", " " = "_", "/" = "_")), 
         recorder = str_replace_all(recorder, " \\+ ", "_")) |> 
  rename(elevation = top_height) |> 
  mutate(distance = elevation - altitude) |> 
  select(-altitude) |> 
  relocate(distance, .after = species) |> 
  mutate(main_type = str_extract(type, "[^C]*")) |> 
  relocate(main_type, .before = type) |> 
  arrange(desc(elevation), species)

# We combine 2024 and 2025 into one object

filefjell_2024_2025_tidy <- filefjell_2024_tidy |> 
  select(year:type) |> 
  rbind(filefjell_2025_tidy) |> 
  arrange(desc(elevation), year, species)

# We tidy the summit data

filefjell_summit_data_tidy <- filefjell_summit_data |> 
  clean_names() |> 
  mutate(summit = str_replace_all(summit, " ", "_"))

# We tidy the type data

filefjell_type_cover_tidy <- filefjell_type_cover |> 
  clean_names() |> 
  relocate(year) |> 
  mutate(date = dmy(date), 
         recorder = str_replace_all(recorder, " \\+ ", "_"), 
         type = ifelse(type == "Naken berg", "T1", type)) |> 
  rename(weather = vaer, 
         cover = percentage) |> 
  mutate(weather = str_replace_all(weather, c(" \\+ " = "_", ", " = "_", " " = "_")))

filefjell_main_type_cover <- filefjell_type_cover_tidy |> 
  mutate(main_type = str_extract(type, "[^C]*")) |> 
  relocate(main_type, .before = type) |> 
  summarise(.by = c(year, summit, date, recorder, main_type), 
            cover = sum(cover))




# We identify errors to correct. THINK AGAIN ABOUT HOW YOU DO THIS, AND WHAT SPECIES NAMES YOU USE, AND DAHL R VALUES. AND TARGETS----

# Summits' elevations
# There are some differences among years in the summits elevations. We use the values from Norgeskart


# Species names

filefjell_tidy_lost <- filefjell_1972_2009_tidy |> 
  select(species) |> 
  arrange(species) |> 
  distinct() |> 
  anti_join(filefjell_2024_2025_tidy |> 
              select(species) |> 
              arrange(species) |> 
              distinct())

filefjell_tidy_new <- filefjell_2024_2025_tidy |> 
  select(species) |> 
  arrange(species) |> 
  distinct() |> 
  anti_join(filefjell_1972_2009_tidy |> 
              select(species) |> 
              arrange(species) |> 
              distinct())

filefjell_tidy_lost
filefjell_tidy_new
# In 2009 the Alchemilla found (not alpina) was called glomerulans. In 2024 we decided to call it sp. We reckon it is the same species, so we call it Alc glo in 2024 and 2025 as well
# In 2009 Cerastium alpinum ssp. lanatum was shortened to Cer lan, while in 2024 it was shortened to Cer_alp_lan
# In 2009 Juncus trifidus was shortened to Jun trif, while in 2024 it was shortened to Jun_tri
# In 2009 Poa x jemtlandica was shortened to Poa x jem, while in 2024 it was shortened to Poa_jem
# In 2009 Silene acaulis was shortened to Sil acu, while in 2024 it was shortened to Sil_aca



# We create the clean objects----

filefjell_1972_2009_clean <- filefjell_1972_2009_tidy |> 
  left_join(filefjell_summit_data_tidy |> select(summit, elevation), by = "summit", suffix = c("", "_correct")) |> 
  select(!elevation) |> 
  rename(elevation = elevation_correct) |> 
  relocate(elevation, .after = summit) |> 
  mutate(species = case_when(species == "Cer_lan" ~ "Cer_alp_lan", 
                             species == "Jun_trif" ~ "Jun_tri", 
                             species == "Poa_x_jem" ~ "Poa_jem", 
                             species == "Sil_acu" ~ "Sil_aca", 
                             TRUE ~ species))

filefjell_2024_2025_clean <- filefjell_2024_2025_tidy |> 
  left_join(filefjell_summit_data_tidy |> select(summit, elevation), by = "summit", suffix = c("", "_correct")) |> 
  select(!elevation) |> 
  rename(elevation = elevation_correct) |> 
  relocate(elevation, .after = summit) |> 
  mutate(species = case_when(species == "Alc_sp" ~ "Alc_glo", 
                             TRUE ~ species)) |> 
  left_join(filefjell_main_type_cover |> select(summit, main_type, cover), by = c("summit", "main_type")) |> 
  relocate(cover, .after = type)

filefjell_clean_lost <- 
  filefjell_1972_2009_clean |> 
  select(species) |> 
  arrange(species) |> 
  distinct() |> 
  anti_join(filefjell_2024_2025_clean |> 
              select(species) |> 
              arrange(species) |> 
              distinct())

filefjell_clean_new <- 
  filefjell_2024_2025_clean |> 
  select(species) |> 
  arrange(species) |> 
  distinct() |> 
  anti_join(filefjell_1972_2009_clean |> 
              select(species) |> 
              arrange(species) |> 
              distinct())

filefjell_clean_lost
filefjell_clean_new


filefjell_data_clean <- filefjell_1972_2009_clean |> 
  rbind(filefjell_2024_2025_clean |> select(year, summit, elevation, species, distance)) |> 
  arrange(desc(elevation), year, species) |> 
  mutate(summit = factor(summit, levels = c("Berdalseken", "Suletinden", "Unnamed", "Storeknippa", "Graanosi", "Loppenosi", "Graveggi", "Krekanosi", "Rjupeskareggen", "Frostdalsnosi", "Krekanosi_S", "Slettningseggi", "Krekahoegdi")))

filefjell_data_clean |> write_csv("Clean_data/Filefjell_data_clean.csv")
