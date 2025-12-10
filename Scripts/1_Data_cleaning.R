# Libraries----

source("Scripts/0_setup.R")



# Data----

elevation_1972_2009 <- read_csv2("data_raw/Filefjell_1972_2009.csv")
filefjell_2024 <- read_csv2("data_raw/Filefjell_2024.csv")
filefjell_2025 <- read_csv2("data_raw/Filefjell_2025.csv")
filefjell_species <- read_csv2("data_raw/Filefjell_species.csv")
visit_dates <- read_csv2("data_raw/Visit_dates.csv")
summit_data <- read_csv2("data_raw/Summit_data.csv")
type_cover <- read_csv2("data_raw/Type_cover.csv")
polygones <- read_csv2("data_raw/Filefjell_polygones.csv")



# Tidying the data----

# We divide 1972and 2009 into two dataframes and tidy them

elevation_1972_tidy <- elevation_1972_2009 |> 
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

elevation_2008_2009_tidy <- elevation_1972_2009 |> 
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


# We tidy the 2024 and 2025 data, calculate distance to summit and combine them

elevation_2024_2025_tidy <- filefjell_2024 |> 
  select(c(Top:Altitude, Rareness)) |> 
  rbind(filefjell_2025 |> 
          select(!Type) |> 
          mutate(Rareness = NA)) |> 
  data_tidying() |> 
  mutate(distance = elevation - altitude) |> 
  select(!altitude) |> 
  relocate(distance, .after = species) |> 
  mutate(rareness = ifelse(rareness == "–", NA, rareness)) |> 
  arrange(summit, year, species)


# We organize and tidy the type data

type_species_tidy <- filefjell_2024 |> 
  select(c(Top:Species, Type, Comments, `Svar Mikel`)) |> 
  rbind(filefjell_2025 |> 
          select(c(Top:Species, Type)) |> 
          mutate(Comments = NA, `Svar Mikel` = NA)) |> 
  data_tidying() |> 
  mutate(main_type = str_extract(type, "[^C]*")) |> 
  relocate(main_type, .before = type) |> 
  arrange(summit, year, species)


polygones_tidy <- polygones |> 
  clean_names() |> 
  select(fid, kartleggin, layer, area_m2) |> 
  mutate(year = 2024) |>
  relocate(year) |>
  rename(summit = layer) |> 
  relocate(summit) |> 
  mutate(summit = str_replace(summit, "_NiN3", "")) |> 
  left_join(elevation_2024_2025_tidy |> 
              select(summit, date, weather) |> 
              distinct(), 
            by = "summit") |> 
  relocate(c(date, weather), .after = year) |> 
  mutate(recorder = "Helene") |> 
  relocate(recorder, .after = weather) |> 
  rename(type = kartleggin) |> 
  mutate(type = str_replace(type, "-", ""))

polygones_cover <- polygones_tidy |> 
  summarise(.by = c(summit:recorder, type), area = sum(area_m2)) |> 
  group_by(summit) |> 
  mutate(percentage = 100 * area / sum(area)) |> 
  ungroup() |> 
  select(!area)

type_cover_tidy <- type_cover |> 
  data_tidying() |> 
  mutate(type = ifelse(type == "Naken berg", "T1", type))

maintype_cover_tidy <- type_cover_tidy |> 
  rbind(polygones_cover |> 
          mutate(comments = NA)) |> 
  mutate(main_type = str_extract(type, "[^C]*")) |> 
  relocate(main_type, .before = type) |> 
  summarise(.by = c(year, summit, date, weather, recorder, main_type), 
            percentage = sum(percentage))


# We tidy the summit

summit_data_tidy <- summit_data |> 
  clean_names() |> 
  mutate(summit = str_replace_all(summit, " ", "_"))




# We identify errors to correct----

## Summits' elevations
# There are some differences among years in the summits elevations. We use the values from Norgeskart


## Species names

species_tidy_lost <- elevation_1972_tidy |> 
  rbind(elevation_2008_2009_tidy) |> 
  select(species) |> 
  arrange(species) |> 
  distinct() |> 
  anti_join(elevation_2024_2025_tidy |> 
              select(species) |> 
              arrange(species) |> 
              distinct(),
            by = "species")

species_tidy_new <- elevation_2024_2025_tidy |> 
  select(species) |> 
  arrange(species) |> 
  distinct() |> 
  anti_join(elevation_1972_tidy |> 
              rbind(elevation_2008_2009_tidy) |> 
              select(species) |> 
              arrange(species) |> 
              distinct(),
            by = "species")

species_tidy_lost
# Alc_glo, Arc_uva, Cer_lan, Ger_syl, Jun_trif, Luz_fri, Poa_x_jem, Sal_phy, Sil_acu, Sil_wah
species_tidy_new
# Agr_cap, Alc_sp, Cal_phr, Car_sax, Cer_alp_lan, Gen_niv, Jun_tri, Lyc_ann, Lyc_cla, Poa_jem, Sal_sp, Sil_aca, Vah_atr

# In 2009 the Alchemilla found (not alpina) was called glomerulans. In 2024 we decided to call it sp. We reckon it is the same species, so we call it Alc glo in 2024 and 2025 as well
# In 2009 Cerastium alpinum ssp. lanatum was shortened to Cer lan, while in 2024 it was shortened to Cer_alp_lan. We use the latter
# In 2009 Juncus trifidus was shortened to Jun trif, while in 2024 it was shortened to Jun_tri. We use the latter
# In 2009 Poa x jemtlandica was shortened to Poa x jem, while in 2024 it was shortened to Poa_jem. We use the latter
# Sal phy was found in Graveggi in 2009, but not in the resampling. An unidentified Salix was found in Unnamed in 2024. Could have been phy, but not sure. And it was a different summit quite far from the other anyways. We keep them as they are
# In 2009 Silene acaulis was shortened to Sil acu, while in 2024 it was shortened to Sil_aca. We use the latter

# 5 species have disappeared from the dataset: Arc uva, Ger syl, Luz fri, Sal phy and Sil wah
# 8 species have appeared: Agr cap, Cal phr, Car sax, Gen niv, Lyc ann, Lyc cla, Vah atr and  Sal sp (maybe same as Sal phy, but in a different summit anyways)


# We create the clean objects----

elevation_1972_clean <- elevation_1972_tidy |> 
  left_join(summit_data_tidy |> 
              select(summit, elevation) |> 
              rename(elevation_correct = elevation),
            by = "summit") |> 
  select(!elevation) |>
  rename(elevation = elevation_correct) |>
  relocate(elevation, .after = summit) |> 
  mutate(species = case_when(species == "Cer_lan" ~ "Cer_alp_lan", 
                             species == "Jun_trif" ~ "Jun_tri",
                             species == "Poa_x_jem" ~ "Poa_jem",
                             species == "Sil_acu" ~ "Sil_aca",
                             TRUE ~ species))

elevation_2008_2009_clean <- elevation_2008_2009_tidy |> 
  left_join(summit_data_tidy |> 
              select(summit, elevation) |> 
              rename(elevation_correct = elevation),
            by = "summit") |> 
  select(!elevation) |>
  rename(elevation = elevation_correct) |>
  relocate(elevation, .after = summit) |> 
  mutate(species = case_when(species == "Cer_lan" ~ "Cer_alp_lan", 
                             species == "Jun_trif" ~ "Jun_tri",
                             species == "Poa_x_jem" ~ "Poa_jem",
                             species == "Sil_acu" ~ "Sil_aca",
                             TRUE ~ species))

elevation_2024_2025_clean <-elevation_2024_2025_tidy |>
  left_join(summit_data_tidy |>
              select(summit, elevation) |> 
              rename(elevation_correct = elevation),
            by = "summit") |>
  select(!elevation) |> 
  rename(elevation = elevation_correct) |> 
  relocate(elevation, .after = summit) |> 
  mutate(species = case_when(species == "Alc_sp" ~ "Alc_glo", 
                             TRUE ~ species))


# Double checking species loss

species_clean_lost <- elevation_1972_clean |> 
  rbind(elevation_2008_2009_clean) |> 
  select(species) |> 
  arrange(species) |> 
  distinct() |> 
  anti_join(elevation_2024_2025_clean |> 
              select(species) |> 
              arrange(species) |> 
              distinct(),
            by = "species")

species_clean_new <- elevation_2024_2025_clean |> 
  select(species) |> 
  arrange(species) |> 
  distinct() |> 
  anti_join(elevation_1972_clean |> 
              rbind(elevation_2008_2009_clean) |> 
              select(species) |> 
              arrange(species) |> 
              distinct(),
            by = "species")

species_clean_lost
species_clean_new


# Final files

elevation_data_clean <- elevation_1972_clean |> 
  rbind(elevation_2008_2009_clean) |> 
  mutate(weather = NA,
         rareness = NA) |> 
  relocate(weather, .after = date) |> 
  rbind(elevation_2024_2025_clean) |> 
  left_join(filefjell_species, by = "species") |> 
  mutate(species = ifelse(!is.na(new_name), new_name, species)) |> 
  relocate(specialization, .before = species) |> 
  select(!new_name) |> 
  mutate(summit = factor(summit, levels = c("Berdalseken", "Suletinden", "Unnamed", "Storeknippa", "Graanosi", "Loppenosi", "Graveggi", "Krekanosi", "Rjupeskareggen", "Frostdalsnosi", "Krekanosi_S", "Slettningseggi", "Krekahoegdi"))) |> 
  arrange(summit, year, species)

type_species_clean <- type_species_tidy |> 
  mutate(species = case_when(species == "Alc_sp" ~ "Alc_glo", 
                             TRUE ~ species)) |> 
  left_join(filefjell_species, by = "species") |> 
  mutate(species = ifelse(!is.na(new_name), new_name, species)) |> 
  relocate(specialization, .before = species) |> 
  select(!new_name) |> 
  left_join(maintype_cover_tidy |> 
              select(summit, main_type, percentage),
            by = c("summit", "main_type")) |> 
  relocate(percentage, .after = main_type) |> 
  left_join(summit_data_tidy |>
              select(summit, elevation) |> 
              rename(elevation_correct = elevation),
            by = "summit") |>
  select(!elevation) |> 
  rename(elevation = elevation_correct) |> 
  relocate(elevation, .after = summit) |> 
  mutate(summit = factor(summit, levels = c("Berdalseken", "Suletinden", "Unnamed", "Storeknippa", "Graanosi", "Loppenosi", "Graveggi", "Krekanosi", "Rjupeskareggen", "Frostdalsnosi", "Krekanosi_S", "Slettningseggi", "Krekahoegdi"))) |> 
  arrange(summit, year, species)

elevation_data_clean |> write_csv("data_clean/Elevation_data_clean.csv")
type_species_clean |> write_csv("data_clean/Type_species_clean.csv")