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


filefjell_all_years <- tar_read(filefjell_all_years) |> 
  filter(summit != "Krekanosi_S") |> 
  mutate(summit = factor(summit, levels = c("Berdalseken", "Suletinden", "Unnamed", "Storeknippa", "Graanosi", "Loppenosi", "Graveggi", "Krekanosi", "Rjupeskareggen", "Frostdalsnosi", "Slettningseggi", "Krekahoegdi")))


# Distance

filefjell_distance <- filefjell_all_years |> 
  pivot_wider(names_from = year, names_prefix = "y", values_from = distance) |> 
  filter(!is.na(y1972) & !is.na(y2009) & !is.na(y2024)) |> 
  pivot_longer(cols = c("y1972", "y2009", "y2024"), names_prefix = "y", names_to = "year", values_to = "distance") |> 
  relocate(year) |> 
  mutate(year = as.numeric(year))

filefjell_distance_change <- filefjell_distance |> 
  pivot_wider(names_from = year, names_prefix = "y", values_from = distance) |> 
  mutate(int1 = y2009 - y1972, 
         int2 = y2024 - y2009) |> 
  select(-c(y1972, y2009, y2024)) |> 
  pivot_longer(cols =c("int1", "int2"), names_to = "interval", values_to = "distance_change") |> 
  mutate(interval = as.factor(interval))


# Richness

filefjell_richness <- filefjell_all_years |> 
  summarise(.by = c(year, summit, elevation), 
            richness = n())


# Turnover

filefjell_turnover <- filefjell_all_years |> 
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




# Distance to top----

## Exploratoy graphs

filefjell_distance |> 
  ggplot(aes(x = as.factor(year), y = distance)) +
  geom_violin() + 
  labs(title = " Distances by Year",
       x = "Year",
       y = "Vertical Distance to Top (meters)") +
  theme_minimal()

filefjell_distance |> 
  ggplot(aes(x = summit:as.factor(year), y = distance, color = summit)) +
  geom_boxplot() +
  labs(title = "Line Plot of Vertical Distances for Each Species Across Years",
       x = "Year",
       y = "Vertical Distance to Top (meters)") +
  theme_minimal() +
  theme(legend.position = "none")

filefjell_distance |> 
  ggplot(aes(x = year, y = distance, color = species)) + 
  facet_wrap(~summit) + 
  geom_line() +
  labs(title = "Scatter Plot of Vertical Distances by Year for Each Mountain",
       x = "Year",
       y = "Vertical Distance to Top (meters)") +
  theme_minimal() +
  theme(legend.position = "none")

# There seems to be a lot of variation in the second period


## I am not interested in the distance per se, but in change in distance. I check that

filefjell_distance_change |> 
  ggplot(aes(x = interval, y = distance_change)) +
  geom_violin() + 
  labs(title = " Distances by Year",
       x = "Year",
       y = "Vertical Distance to Top (meters)") +
  theme_minimal()

filefjell_distance_change |> 
  ggplot(aes(x = summit:interval, y = distance_change, color = summit)) +
  geom_boxplot() +
  labs(title = "Line Plot of Vertical Distances for Each Species Across Years",
       x = "Year",
       y = "Vertical Distance to Top (meters)") +
  theme_minimal() +
  theme(legend.position = "none")

filefjell_distance_change |> 
  ggplot(aes(x = interval, y = distance_change, color = species, group = species)) + 
  facet_wrap(~summit) + 
  geom_line() +
  labs(title = "Scatter Plot of Vertical Distances by Year for Each Mountain",
       x = "Year",
       y = "Vertical Distance to Top (meters)") +
  theme_minimal() +
  theme(legend.position = "none")



## Modelling

hist(filefjell_distance_change$distance_change)

distance_change_model <- glmmTMB(
  distance_change ~ 
    interval + (1 | summit) + (1 | species), 
  family = gaussian, 
  data = filefjell_distance_change)

# distance_change_model |> model_diagnosis()
# distance_change_model |> model_homoscedasticity()
distance_change_model |> summary()




# Richness----

## Exploratoy graphs

filefjell_richness |> 
  ggplot(aes(x = as.factor(year), y = richness)) +
  geom_violin() + 
  labs(title = " Distances by Year",
       x = "Year",
       y = "Vertical Distance to Top (meters)") +
  theme_minimal()

filefjell_richness |> 
  ggplot(aes(x = year, y = richness, color = summit)) +
  geom_line(lwd = 3) +
  labs(title = "Line Plot of Vertical Distances for Each Species Across Years",
       x = "Year",
       y = "Vertical Distance to Top (meters)") +
  theme_minimal()

# Some summits gained species at a faster rate, some at a slower, and some lost species in the second interval



## Modelling

richness_mod <- glmer(
  richness ~ 
    bs(year, knots = c(2009)) + 
    (1 | summit), 
  family = poisson, 
  data = filefjell_richness)
richness_mod |> model_diagnosis()
richness_mod |> summary()


filefjell_richness_predicted <- richness_mod |> 
  ggpredict(terms = "year") |> 
  rename(year = x)

richness_model_graph <- ggplot() + 
  geom_point(data = filefjell_richness, aes(x = year, y = richness), size = 2) + # Points for each observation
  geom_hline(yintercept = c(20, 40, 60, 80), colour = "lightgrey") + 
  geom_line(data = filefjell_richness_predicted, aes(x = year, y = predicted), lwd = 0.75, colour = "blue") + # Line for predicted values
  geom_ribbon(data = filefjell_richness_predicted, aes(x = year, ymin = conf.low, ymax = conf.high), fill = "blue", alpha = 0.2) + # Confidence interval
  labs(x = "Year", y = "Richness") +
  theme_classic() + 
  theme(text = element_text(size = 20, family = "serif"), 
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)), 
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)))
richness_model_graph
richness_model_graph |> ggsave(file = "Graphs/Richness.jpeg")


# Rate of species increase in interval 1
(filefjell_richness_predicted[2, 2] - filefjell_richness_predicted[1, 2]) / (filefjell_richness_predicted[2, 1] - filefjell_richness_predicted[1, 1])

# Rate of species increase in interval 2
(filefjell_richness_predicted[3, 2] - filefjell_richness_predicted[2, 2]) / (filefjell_richness_predicted[3, 1] - filefjell_richness_predicted[2, 1])




# Turnover----

filefjell_turnover |> 
  summarise(.by = interval,
            lost_mean = mean(lost), 
            nochange_mean = mean(nochange), 
            new_mean = mean(new))

filefjell_turnover |> 
  summarise(.by = interval,
            lost_mean = mean(lost_rate), 
            nochange_mean = mean(nochange_rate), 
            new_mean = mean(new_rate))

# I don't analyse or plot nochange. One being greater than the other is just a measure of richness, not of change


# Lost species

filefjell_turnover |> 
  ggplot(aes(x = interval, y = lost_rate)) + 
  geom_violin()

filefjell_lost_aov <- aov(lost_rate ~ interval, data = filefjell_turnover)
filefjell_lost_aov |> summary()
# Greater species lost in the second period


# New species

filefjell_turnover |> 
  ggplot(aes(x = interval, y = new_rate)) + 
  geom_violin()

filefjell_new_aov <- aov(new_rate ~ interval, data = filefjell_turnover)
filefjell_new_aov |> summary()
# Fewer new species in the second period


turnover_raw_graph <- filefjell_turnover |> 
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



