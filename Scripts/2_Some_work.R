# Libraries and data----

library(tidyverse)
library(vegan)
library(glmmTMB)


community_data <- read_csv2("Data/Filefjell_data.csv")
community_order <- community_data |> 
  relocate(year, .before = summit) |> 
  select(-c(height, no_species)) |> 
  pivot_longer(cols = c(sal_her:arc_uva), names_to = "species", values_to = "distance") |> 
  arrange(year, summit, species) |> 
  mutate(presence = ifelse(is.na(distance) == TRUE, 0, 1))

community_presence <- community_order |> 
  select(-c(distance)) |> 
  filter(presence != 0) |> 
  arrange(species) |> 
  pivot_wider(names_from = species, values_from = presence, values_fill = 0) |> 
  arrange(year, summit)

community_metadata <- community_presence |> select(year:summit)
community_species <- community_presence |> select(-c(year:summit))



# Unconstrained ordination----

set.seed(811)

community_nmds <- community_species |> metaMDS(k = 2, distance = "jaccard", trymax = 1000)
community_nmds_sites <- community_nmds |> scores(display = "sites") |> as_tibble()
community_nmds_species <- community_nmds |> scores(display = "species") |> as_tibble()

community_presence_sites <- community_metadata |> 
  cbind(community_nmds_sites)

community_presence_sites |> 
  ggplot() + 
  geom_point(aes(x = NMDS1, y = NMDS2, colour = summit), size = 5) + 
  geom_segment(data = community_presence_sites |> 
                 group_by(summit) |> 
                 mutate(next_NMDS1 = lead(NMDS1),
                        next_NMDS2 = lead(NMDS2)) |> 
                 filter(!is.na(next_NMDS1)),
               aes(x = NMDS1, y = NMDS2, xend = next_NMDS1, yend = next_NMDS2),
               arrow = arrow(length = unit(0.3, "cm"))) + 
  geom_text(data = community_presence_sites |> 
              filter(year == 1972), 
            aes(x = NMDS1, y = NMDS2 - 0.035, label = summit), size = 5) + 
  theme_bw()



species_list <- community_species |> 
  pivot_longer(cols = ach_mil:vis_alp, names_to = "species") |> 
  filter(value == 1) |> 
  arrange(species) |> 
  distinct()

community_presence_species <- species_list |> 
  select(-value) |> 
  cbind(community_nmds_species)

community_presence_species |> 
  ggplot() + 
  geom_text(aes(x = NMDS1, y = NMDS2, label = species), size = 5) + 
  theme_bw()



# Distance to top----

community_distance <- community_order |> 
  filter(presence == 1) |> 
  select(-presence)

community_distance_years <- community_distance |> 
  pivot_wider(names_from = year, names_prefix = "y_", values_from = distance) |> 
  arrange(summit, species)

community_all_years <- community_distance_years |> 
  filter(!is.na(y_1972) & !is.na(y_2010) & !is.na(y_2024))

community_all_years2 <- community_all_years |> 
  pivot_longer(cols = c(y_1972, y_2010, y_2024), names_to = "year", values_to = "distance")


distance_mod <- glmmTMB(
    distance ~ year + (1|summit/species), 
    data = community_all_years2
  )
distance_mod |> summary()
