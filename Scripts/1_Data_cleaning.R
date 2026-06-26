# Libraries----

source("Scripts/0_setup.R")



# Data----

filefjell_1972_2009 <- read_csv2("data_raw/Filefjell_1972_2009.csv")
filefjell_2024 <- read_csv2("data_raw/Filefjell_2024.csv")
filefjell_2025 <- read_csv2("data_raw/Filefjell_2025.csv")
filefjell_species <- read_csv2("data_raw/Filefjell_species.csv")
visit_dates <- read_csv2("data_raw/Visit_dates.csv")
summit_data <- read_csv2("data_raw/Summit_data.csv")
type_cover <- read_csv2("data_raw/Type_cover.csv")
polygones <- read_csv2("data_raw/Filefjell_polygones.csv")



# We tidy the summit data. We use summit elevations from Norgeskart

summit_data_tidy <- summit_data |>
  clean_names() |>
  mutate(summit = str_replace_all(summit, " ", "_")) |>
  rename(correct_height = height) |>
  rename(summit_hectare = area)



# Species names----
## We check what species names are found in some of the surveys but not the others, and evaluate whether these are actually different species or different nomenclature or typos

filefjell_1972_2009_species <- filefjell_1972_2009 |>
  pivot_longer(cols = !c(Summit, Height, Year), names_to = "Species", values_to = "Distance") |>
  select(Species) |>
  mutate(Species = str_replace_all(Species, " ", "_")) |>
  mutate(Species = str_replace_all(Species, "\\.", "")) |>
  distinct()

filefjell_2024_2025_species <- filefjell_2024 |>
  select(Species) |>
  rbind(filefjell_2025 |> select(Species)) |>
  distinct()

species_lost <- filefjell_1972_2009_species |>
  anti_join(filefjell_2024_2025_species,
            by = "Species") |>
  arrange(Species)

species_new <- filefjell_2024_2025_species |>
  anti_join(filefjell_1972_2009_species,
            by = "Species") |>
  arrange(Species)

species_lost
# Alc_glo, Arc_uva, Cer_lan, Ger_syl, Jun_trif, Luz_fri, Poa_x_jem, Sal_phy, Sil_acu, Sil_wah
species_new
# Agr_cap, Alc_sp, Cal_phr, Car_sax, Cer_alp_lan, Gen_niv, Jun_tri, Lyc_ann, Lyc_cla, Poa_jem, Sal_sp, Sil_aca, Vah_atr


# In 2009 the Alchemilla found (not alpina) was called glomerulans. In 2024 we decided to call it sp. We reckon it is the same species, so we call it Alc glo in 2024 and 2025 as well
# In 2009 Cerastium alpinum ssp. lanatum was shortened to Cer lan, while in 2024 it was shortened to Cer_alp_lan. We use the latter
# In 2009 Juncus trifidus was shortened to Jun trif, while in 2024 it was shortened to Jun_tri. We use the latter
# In 2009 Poa x jemtlandica was shortened to Poa x jem, while in 2024 it was shortened to Poa_jem. We use the latter
# Sal phy was found in Graveggi in 2009, but not in the resampling. An unidentified Salix was found in Unnamed in 2024. Could have been phy, but not sure. And it was a different summit quite far from the other anyways. We keep them as they are
# In 2009 Silene acaulis was shortened to Sil acu, while in 2024 it was shortened to Sil_aca. We use the latter


# 5 species have disappeared from the dataset: Arc uva, Ger syl, Luz fri, Sal phy and Sil wah
# 8 species have appeared: Agr cap, Cal phr, Car sax, Gen niv, Lyc ann, Lyc cla, Vah atr and  Sal sp (maybe same as Sal phy, but in a different summit anyways)


# We have to double-check where the different Eriophorum species were registered

eriophorum <- filefjell_2024 |>
  select(Top, Species, Type, Height, Altitude) |>
  rbind(filefjell_2025 |>
          select(Top, Species, Type, Height, Altitude)) |>
  filter(grepl("Eri_", Species) & Species != "Eri_uni")
# Eriophorum was found in "wetland" areas created by the extremely late snowmelt, or pockets within other habitats where the snow had stayed long. This is habitat V6

# In unnamed, many species were registered as in snowbed. However, the estimation of the habitat's cover did not include snowbeds. This was because all these snowbeds were just small patches between the boulders



# We create one file for each survey----

filefjell_1972_clean <- filefjell_1972_2009 |>
  # We need the correct years, and to put the data in the correct format
  filter(Year == 1972) |>
  left_join(visit_dates |>
              select(!c(Year2:Data2)),
            by = c("Summit", "Year")) |>
  pivot_longer(cols = !c(Summit:Year, Date),
               names_to = "species",
               values_to = "distance",
               values_drop_na = TRUE) |>
  data_cleaning(summit_data_tidy, filefjell_species)

filefjell_2008_2009_clean <- filefjell_1972_2009 |>
  # We need the correct years, and to put the data in the correct format
  filter(Year == 2009) |>
  select(!Year) |>
  left_join(visit_dates |>
              filter(Year %in% c(2008, 2009)) |>
              select(!c(Year2:Data2)),
            by = c("Summit")) |>
  pivot_longer(cols = !c(Summit, Height, Year:Date),
               names_to = "species",
               values_to = "distance",
               values_drop_na = TRUE) |>
  data_cleaning(summit_data_tidy, filefjell_species)

filefjell_2024_2025_clean <- filefjell_2024 |>
  rbind(filefjell_2025) |>
  # For simplicity's sake, we assume the species found in Storeknippa in 2025 were in the exact same location in 2024
  mutate(Year = ifelse(Top == "Storeknippa", 2024, Year)) |>
  # We calculate distance
  mutate(Distance = Height - Altitude) |>
  select(!Altitude) |>
  relocate(Distance, .after = Species) |>
  data_cleaning(summit_data_tidy, filefjell_species)


# We double-check the species

species_clean_lost <- filefjell_1972_clean |>
  rbind(filefjell_2008_2009_clean) |>
  select(species) |>
  arrange(species) |>
  distinct() |>
  anti_join(filefjell_2024_2025_clean |>
              select(species) |>
              arrange(species) |>
              distinct(),
            by = "species")

species_clean_new <- filefjell_2024_2025_clean |>
  select(species) |>
  arrange(species) |>
  distinct() |>
  anti_join(filefjell_1972_clean |>
              rbind(filefjell_2008_2009_clean) |>
              select(species) |>
              arrange(species) |>
              distinct(),
            by = "species")

species_clean_lost
species_clean_new



# We work on habitat cover at each summit----

# For this file I won't consider year, date or recorder, that information can be found in the main filefjell file
# We group types T1_ and T27_

polygones_tidy <- polygones |>
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
  select(!area_m2)

polygones_simplified <- polygones_tidy |>
  mutate(group = case_when(grepl("T1C", type) | grepl("Naken", type) ~ "T1",
                           grepl("T27", type) ~ "T27",
                           .default = type)) |>
  summarise(.by = c(summit, group), percentage = sum(percentage))

type_cover_tidy <- type_cover |>
  clean_names() |>
  mutate(summit = str_replace_all(summit, " ", "_")) |>
  select(summit, type, percentage) |>
  mutate(type = ifelse(type == "Naken berg", "T1", type))

type_cover_simplified <- type_cover_tidy |>
  mutate(group = case_when(grepl("T1C", type) | grepl("Naken", type) ~ "T1",
                           grepl("T27", type) ~ "T27",
                           .default = type)) |>
  summarise(.by = c(summit, group), percentage = sum(percentage))

types_with_species <- filefjell_2024_2025_clean |>
  select(summit, type) |>
  distinct()

types_with_species_simplified <- types_with_species |>
  mutate(group = case_when(grepl("T1C", type) | grepl("Naken", type) ~ "T1",
                           grepl("T27", type) ~ "T27",
                           .default = type)) |>
  select(summit, group) |>
  distinct()

habitat_cover <- types_with_species_simplified |>
  full_join(polygones_simplified, by = c("summit", "group")) |>
  rename(percentage1 = percentage) |>
  full_join(type_cover_simplified, by = c("summit", "group")) |>
  rename(percentage2 = percentage) |>
  mutate(percentage = ifelse(is.na(percentage1), percentage2, percentage1)) |>
  left_join(summit_data_tidy |>
              select(summit, summit_hectare),
            by = "summit") |>
  # summit_hectare * percentage / 100 = area of habitats in hectares. We want the area in decares, so we multiply by 10
  # For those habitats too small to be estimated we give them an area of 0.25 decares (250 m2)
  mutate(group_decare = percentage * summit_hectare / 10,
         group_decare = ifelse(is.na(group_decare), 0.25, group_decare),
         habitat = str_extract(group, "[^C]*"),
         habitat = ifelse(grepl("V", habitat), "V6", habitat)) |>
  summarise(.by = c(summit, habitat), habitat_decare = sum(group_decare)) |>
  mutate(summit = factor(summit, levels = c("Berdalseken", "Suletinden", "Unnamed", "Storeknippa", "Graanosi", "Loppenosi", "Graveggi", "Krekanosi", "Rjupeskareggen", "Frostdalsnosi", "Krekanosi_S", "Slettningseggi", "Krekahoegdi")),
         habitat = factor(habitat, levels = c("T1", "T27", "T13", "T14", "T3", "T22", "T7", "V6"))) |>
  arrange(summit, habitat)



# Final files----

filefjell_data_clean <- filefjell_1972_clean |>
  rbind(filefjell_2008_2009_clean) |>
  rbind(filefjell_2024_2025_clean |> select(!type)) |>
  mutate(summit = factor(summit, levels = c("Berdalseken", "Suletinden", "Unnamed", "Storeknippa", "Graanosi", "Loppenosi", "Graveggi", "Krekanosi", "Rjupeskareggen", "Frostdalsnosi", "Krekanosi_S", "Slettningseggi", "Krekahoegdi"))) |>
  arrange(year, summit, specialisation, species)

habitat_species_clean <- filefjell_2024_2025_clean |>
  select(summit, type, species:functional) |>
  # We change all våtmark to V6, and all Eriophorum to it
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

filefjell_data_clean |> write_csv("data_clean/Filefjell_data_clean.csv")
habitat_species_clean |> write_csv("data_clean/Habitat_species_clean.csv")

