# Libraries----

source("Scripts/0_setup.R")



# Data----

filefjell_1972_2009 <- read_csv2("data_raw/Filefjell_1972_2009.csv")
filefjell_visit_dates_2008_2009 <- read_csv2("data_raw/Filefjell_visit_dates_2008_2009.csv")
filefjell_2024 <- read_csv2("data_raw/Filefjell_2024.csv")
filefjell_2025 <- read_csv2("data_raw/Filefjell_2025.csv")
filefjell_summit_data <- read_csv2("data_raw/Summit_data.csv")
filefjell_type_cover <- read_csv2("data_raw/Type_cover.csv")
filefjell_dahlr <- read_csv2("data_raw/DahlR_values.csv")



# Tidying the data----

# We divide 1972and 2009 into two dataframes and tidy them

filefjell_1972_tidy <- filefjell_1972_2009 |> 
  filter(Year == 1972) |> 
  pivot_longer(cols = !c(Summit:Year), names_to = "species", values_to = "distance", values_drop_na = TRUE) |> 
  data_tidying()

filefjell_2009_tidy <- filefjell_1972_2009 |> 
  filter(Year == 2009) |> 
  left_join(filefjell_visit_dates_2008_2009, by = "Summit") |> 
  mutate(Year = if_else(grepl("2008", Date), 2008, Year)) |> 
  pivot_longer(cols = !c(Summit:Year, Date, Recorder), names_to = "species", values_to = "distance", values_drop_na = TRUE) |> 
  data_tidying()

# We tidy the 2024 data, calculate distance to summit and create column for main type

filefjell_2024_tidy <- filefjell_2024 |> 
  data_tidying() |> 
  mutate(distance = elevation - altitude) |> 
  select(!altitude) |> 
  relocate(distance, .after = species) |> 
  mutate(main_type = str_extract(type, "[^C]*")) |> 
  relocate(main_type, .before = type)

# We tidy the 2025 data, calculate distance to summit and create column for main type

filefjell_2025_tidy <- filefjell_2025 |> data_tidying() |> 
  mutate(distance = elevation - altitude) |> 
  select(!altitude) |> 
  relocate(distance, .after = species) |> 
  mutate(main_type = str_extract(type, "[^C]*")) |> 
  relocate(main_type, .before = type)


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

filefjell_maintype_cover_tidy <- filefjell_type_cover_tidy |> 
  mutate(main_type = str_extract(type, "[^C]*")) |> 
  relocate(main_type, .before = type) |> 
  summarise(.by = c(year, summit, date, recorder, main_type), 
            cover = sum(cover))




# We identify errors to correct. THINK AGAIN ABOUT HOW YOU DO THIS, AND DAHL R VALUES----

## Summits' elevations
# There are some differences among years in the summits elevations. We use the values from Norgeskart


## Species names

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
# Alc_glo, Arc_uva, Cer_lan, Ger_syl, Jun_trif, Luz_fri, Poa_x_jem, Sal_phy, Sil_acu, Sil_wah
filefjell_tidy_new
# Agr_cap, Alc_sp, Cal_phr, Car_sax, Cer_alp_lan, Gen_niv, Jun_tri, Lyc_ann, Lyc_cla, Poa_jem, Sal_sp, Sil_aca, Vah_atr

# In 2009 the Alchemilla found (not alpina) was called glomerulans. In 2024 we decided to call it sp. We reckon it is the same species, so we call it Alc glo in 2024 and 2025 as well
# In 2009 Cerastium alpinum ssp. lanatum was shortened to Cer lan, while in 2024 it was shortened to Cer_alp_lan. We use the former
# In 2009 Juncus trifidus was shortened to Jun trif, while in 2024 it was shortened to Jun_tri. We use the latter
# In 2009 Poa x jemtlandica was shortened to Poa x jem, while in 2024 it was shortened to Poa_jem. We use the latter
# Sal phy was found in Graveggi in 2009, but not in the resampling. An unidentified Salix was found in Unnamed in 2024. Could have been phy, but not sure. And it was a different summit quite far from the other anyways. We keep them as they are
# In 2009 Silene acaulis was shortened to Sil acu, while in 2024 it was shortened to Sil_aca. We use the latter

# 5 species have disappeared from the dataset: Arc uva, Ger syl, Luz fri, Sal phy and Sil wah
# 8 species have appeared: Agr cap, Cal phr, Car sax, Gen niv, Lyc ann, Lyc cla, Vah atr and  Sal sp (maybe same as Sal phy, but in a different summit anyways)


# We create the clean objects----

filefjell_1972_2009_clean <- filefjell_1972_2009_tidy |> 
  left_join(filefjell_summit_data_tidy |> select(summit, elevation), 
            by = "summit", 
            suffix = c("", "_correct")) |> 
  select(!elevation) |> 
  rename(elevation = elevation_correct) |> 
  relocate(elevation, .after = summit) |> 
  mutate(species = case_when(species == "Jun_trif" ~ "Jun_tri", 
                             species == "Poa_x_jem" ~ "Poa_jem", 
                             species == "Sil_acu" ~ "Sil_aca", 
                             TRUE ~ species))

filefjell_2024_2025_clean <- filefjell_2024_2025_tidy |> 
  left_join(filefjell_summit_data_tidy |> select(summit, elevation), 
            by = "summit", 
            suffix = c("", "_correct")) |> 
  select(!elevation) |> 
  rename(elevation = elevation_correct) |> 
  relocate(elevation, .after = summit) |> 
  mutate(species = case_when(species == "Alc_sp" ~ "Alc_glo", 
                             species == "Cer_alp_lan" ~ "Cer_lan", 
                             TRUE ~ species)) |> 
  left_join(filefjell_maintype_cover_tidy |> select(summit, main_type, cover), by = c("summit", "main_type")) |> 
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


filefjell_data_clean <- filefjell_2024_2025_clean |> 
  select(year, summit, elevation, species, distance) |> 
  rbind(filefjell_1972_2009_clean) |> 
  arrange(desc(elevation), year, species) |> 
  mutate(summit = factor(summit, levels = c("Berdalseken", "Suletinden", "Unnamed", "Storeknippa", "Graanosi", "Loppenosi", "Graveggi", "Krekanosi", "Rjupeskareggen", "Frostdalsnosi", "Krekanosi_S", "Slettningseggi", "Krekahoegdi")))

filefjell_data_clean |> write_csv("data_clean/Filefjell_data_clean.csv")
