# Libraries and data----

library(conflicted)
library(targets)
library(tidyverse)
library(vegan)
library(glmmTMB)
library(lme4)
library(splines)
library(ggeffects)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("margin", "ggplot2")

source("functions.R")


filefjell_data_clean <- tar_read(filefjell_data_clean) |> 
  filter(summit != "Krekanosi_S") |> 
  mutate(summit = factor(summit, levels = c("Berdalseken", "Suletinden", "Unnamed", "Storeknippa", "Graanosi", "Loppenosi", "Graveggi", "Krekanosi", "Rjupeskareggen", "Frostdalsnosi", "Slettningseggi", "Krekahoegdi")))


# Distance

distance_change_data <- filefjell_data_clean |> 
  pivot_wider(names_from = year, names_prefix = "y", values_from = distance) |> 
  mutate(int1 = case_when(is.na(y2009) ~ NA, 
                          !is.na(y2009) & is.na(y1972) ~ y2009 - 33, 
                          !is.na(y2009) & !is.na(y1972) ~ y2009 - y1972), 
         int2 = case_when(is.na(y2024) ~ NA, 
                          !is.na(y2024) & is.na(y2009) ~ y2024 - 33, 
                          !is.na(y2024) & !is.na(y2009) ~ y2024 - y2009)) |> 
  select(-c(y1972, y2009, y2024)) |>
  pivot_longer(cols =c("int1", "int2"), names_to = "interval", values_to = "distance_change") |> 
  mutate(distance_change_rate = case_when(interval == "int1" ~ distance_change / 37, 
                                          interval == "int2" ~ distance_change / 15)) |> 
  filter(!is.na(distance_change)) |> 
  mutate(interval = as.factor(interval)) |> 
  arrange(summit, species, interval)


# Richness

richness_data <- filefjell_data_clean |> 
  summarise(.by = c(year, summit, elevation), 
            richness = n())

richness_change_data <- richness_data |> 
  pivot_wider(names_from = year, names_prefix = "y", values_from = richness) |> 
  mutate(int1 = y2009 - y1972, 
         int2 = y2024 - y2009) |> 
  select(-c(y1972, y2009, y2024)) |> 
  pivot_longer(cols = c(int1, int2), names_to = "interval", values_to = "richness_change") |> 
  mutate(richness_change_rate = case_when(interval == "int1" ~ richness_change / 37, 
                                          interval == "int2" ~ richness_change / 15)) |> 
  mutate(interval = as.factor(interval))


# Turnover

species_turnover_data <- filefjell_data_clean |> 
  mutate(presence = ifelse(!is.na(distance), 1, 0)) |> 
  select(-distance) |> 
  arrange(species) |> 
  pivot_wider(names_from = "year", names_prefix = "y", values_from = "presence", values_fill = 0) |> 
  arrange(summit, species) |> 
  mutate(turnover09 = y2009 - y1972) |> 
  mutate(turnover24 = y2024 - y2009)

turnover_data <- species_turnover_data |> 
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


# New species by nature type

filefjell_2024_data <- tar_read(filefjell_2024_clean)

colonizers_data <- filefjell_2024_data |> 
  semi_join(
    species_turnover |> 
      filter(turnover24 == 1) |> 
      select(summit:species))


# community_order <- community_data |> 
#   select(-c(height, no_species)) |> 
#   pivot_longer(cols = c(sal_her:arc_uva), names_to = "species", values_to = "distance") |> 
#   arrange(year, summit, species) |> 
#   mutate(presence = ifelse(is.na(distance) == TRUE, 0, 1))

# community_presence <- community_order |> 
#   select(-c(distance)) |> 
#   filter(presence != 0) |> 
#   arrange(species) |> 
#   pivot_wider(names_from = species, values_from = presence, values_fill = 0) |> 
#   arrange(year, summit)

# community_metadata <- community_presence |> select(year:summit)
# community_species <- community_presence |> select(-c(year:summit))



# Distance change----

distance_change_data |> 
  ggplot(aes(x = interval, y = distance_change_rate)) +
  geom_violin() + 
  labs(title = " Distances by Year",
       x = "Year",
       y = "Vertical Distance to Top (meters)") +
  theme_minimal()

distance_change_data |> 
  ggplot(aes(x = summit:interval, y = distance_change_rate, color = summit)) +
  geom_boxplot() +
  labs(title = "Line Plot of Vertical Distances for Each Species Across Years",
       x = "Year",
       y = "Vertical Distance to Top (meters)") +
  theme_minimal() +
  theme(legend.position = "none")

distance_change_data |> 
  ggplot(aes(x = interval, y = distance_change_rate, color = species, group = species)) + 
  facet_wrap(~summit) + 
  geom_line() +
  labs(title = "Scatter Plot of Vertical Distances by Year for Each Mountain",
       x = "Year",
       y = "Vertical Distance to Top (meters)") +
  theme_minimal() +
  theme(legend.position = "none")


## Modelling change

hist(distance_change_data$distance_change_rate)

distance_change_rate_mod <- glmmTMB(
  distance_change_rate ~ 
    interval + (1 | summit) + (1 | species), 
  family = gaussian, 
  data = distance_change_data)

# distance_change_mod |> model_diagnosis()
# distance_change_mod |> model_homoscedasticity()
distance_change_rate_mod |> summary()




# Richness change----

## Exploratoy graphs

richness_change_data |> 
  ggplot(aes(x = interval, y = richness_change_rate)) +
  geom_violin() + 
  labs(title = " Distances by Year",
       x = "Year",
       y = "Vertical Distance to Top (meters)") +
  theme_minimal()

richness_change_data |> 
  ggplot(aes(x = summit:interval, y = richness_change_rate, color = summit)) +
  geom_point() +
  labs(title = "Plot of richness change per year",
       x = "Year",
       y = "Richness change (species/year)") +
  theme_minimal() +
  theme(legend.position = "none")

# Some summits gained species at a faster rate, some at a slower, and some lost species in the second interval


## Modelling

richness_change_rate_mod <- glmmTMB(
  richness_change_rate ~ 
    interval + (1 | summit), 
  family = gaussian, 
  data = richness_change_data)

richness_change_rate_mod |> model_diagnosis()
richness_change_rate_mod |> summary()




# Turnover----

turnover_data |> 
  summarise(.by = interval,
            lost_mean = mean(lost), 
            nochange_mean = mean(nochange), 
            new_mean = mean(new))

turnover_data |> 
  summarise(.by = interval,
            lost_mean = mean(lost_rate), 
            new_mean = mean(new_rate))

# I don't analyse or plot nochange. One being greater than the other is just a measure of richness, not of change


# Lost species

turnover_data |> 
  ggplot(aes(x = interval, y = lost_rate)) + 
  geom_violin()

lost_rate_mod <- glmmTMB(
  lost_rate ~ 
    interval + (1 | summit), 
  family = gaussian, 
  data = turnover_data)

lost_rate_mod |> model_diagnosis()
lost_rate_mod |> summary()
# Greater species loss in the second period


# New species

turnover_data |> 
  ggplot(aes(x = interval, y = new_rate)) + 
  geom_violin()

new_rate_mod <- glmmTMB(
  new_rate ~ 
    interval + (1 | summit), 
  family = gaussian, 
  data = turnover_data)

new_rate_mod |> model_diagnosis()
new_rate_mod |> summary()

# Fewer new species in the second period, but not significantly


turnover_raw_graph <- turnover |> 
  select(summit, interval, lost_rate, new_rate) |> 
  pivot_longer(cols = c(lost_rate, new_rate), names_to = "species_change", values_to = "change") |> 
  ggplot(aes(x = interval, y = change, fill = interval)) + 
  geom_violin() + 
  stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", colour = "white") + 
  facet_grid(cols = vars(species_change), labeller = adj_label) + 
  scale_fill_manual("Interval", values = c("int1" = "#859395", "int2" = "#f58800"), labels = c("1972-2009", "2009-2024")) + 
  labs(x = "Interval", y = "Species / Year") + 
  theme_bw() + 
  theme(text = element_text(size = 20, family = "serif"), 
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.title.y = element_text(margin = margin(r = 5)), 
        legend.position = "top", 
        legend.title = element_text(margin = margin(r = 30)), 
        legend.text = element_text(margin = margin(l = 09, r = 20)))
turnover_raw_graph
turnover_raw_graph |> ggsave(file = "Graphs/Turnover.jpeg")




# Colonizers. When doing this analysis consider whether species present in 1972 (that disappeared in 2010) should be taken into account ----

## Exploratoy graphs

colonizers_data |> 
  summarise(.by = c(type), colonizers = n()) |> 
  ggplot(aes(x = type, y = colonizers)) +
  geom_col() + 
  labs(title = " Distances by Year",
       x = "Year",
       y = "Vertical Distance to Top (meters)") +
  theme_minimal()









# Unconstrained ordination----

set.seed(811)

community_nmds <- community_species |> metaMDS(k = 2, distance = "jaccard", trymax = 0900)
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



