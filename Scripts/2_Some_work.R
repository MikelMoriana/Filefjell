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

# distance_data <- filefjell_data_clean |> 
#   pivot_wider(names_from = year, names_prefix = "y", values_from = distance) |> 
#   filter(!is.na(y1972) & !is.na(y2009) & !is.na(y2024)) |> 
#   pivot_longer(cols = c("y1972", "y2009", "y2024"), names_prefix = "y", names_to = "year", values_to = "distance") |> 
#   relocate(year) |> 
#   mutate(year = as.numeric(year))

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



# Distance. All species----

## Exploratory graphs

filefjell_data_clean |> 
  ggplot(aes(x = as.factor(year), y = distance)) +
  geom_violin() + 
  labs(title = " Distances by Year",
       x = "Year",
       y = "Vertical Distance to Top (meters)") +
  theme_minimal()

filefjell_data_clean |> 
  ggplot(aes(x = summit:as.factor(year), y = distance, color = summit)) +
  geom_boxplot() +
  labs(title = "Line Plot of Vertical Distances for Each Species Across Years",
       x = "Year",
       y = "Vertical Distance to Top (meters)") +
  theme_minimal() +
  theme(legend.position = "none")

filefjell_data_clean |> 
  ggplot(aes(x = year, y = distance, color = species)) + 
  facet_wrap(~summit) + 
  geom_line() +
  labs(title = "Scatter Plot of Vertical Distances by Year for Each Mountain",
       x = "Year",
       y = "Vertical Distance to Top (meters)") +
  theme_minimal() +
  theme(legend.position = "none")


## Modelling

distance_all_modsp1 <- glmer(
  distance ~ 
    bs(year, knots = c(2009), degree = 1) + (1 | species), 
  family = poisson, 
  data = filefjell_data_clean)
# If I were to consider summit
# distance_modsp2 <- glm(
#   distance ~ 
#     bs(year, knots = c(2009), degree = 1) + summit, 
#   family = poisson, 
#   data = distance)
# anova(distance_modsp1, distance_modsp2) # I keep the interaction

distance_all_modsp1 |> summary()


distance_all_predicted <- distance_all_modsp1 |> 
  ggpredict(terms = c("year")) |> 
  rename(year = x, summit = group)

distance_all_mod_graph <- 
  ggplot() + 
  geom_jitter(data = filefjell_data_clean, aes(x = year, y = distance), size = 2, width = 0.5, height = 0.5) + # Points for each observation
  geom_hline(yintercept = c(10, 20, 30), colour = "lightgrey") + 
  geom_line(data = distance_all_predicted, aes(x = year, y = predicted), lwd = 0.75, colour = "blue") + # Line for predicted values
  geom_ribbon(data = distance_all_predicted, aes(x = year, ymin = conf.low, ymax = conf.high), fill = "blue", alpha = 0.2) + # Confidence interval
  labs(x = "Year", y = "Distance to top") +
  theme_classic() + 
  theme(text = element_text(size = 20, family = "serif"), 
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)), 
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)))
distance_all_mod_graph
distance_all_mod_graph |> ggsave(file = "Graphs/Distance_all.jpeg")




# Distance. Only species found all three years----

## Exploratory graphs

distance_data |> 
  ggplot(aes(x = as.factor(year), y = distance)) +
  geom_violin() + 
  labs(title = " Distances by Year",
       x = "Year",
       y = "Vertical Distance to Top (meters)") +
  theme_minimal()

distance_data |> 
  ggplot(aes(x = summit:as.factor(year), y = distance, color = summit)) +
  geom_boxplot() +
  labs(title = "Line Plot of Vertical Distances for Each Species Across Years",
       x = "Year",
       y = "Vertical Distance to Top (meters)") +
  theme_minimal() +
  theme(legend.position = "none")

distance_data |> 
  ggplot(aes(x = year, y = distance, color = species)) + 
  facet_wrap(~summit) + 
  geom_line() +
  labs(title = "Scatter Plot of Vertical Distances by Year for Each Mountain",
       x = "Year",
       y = "Vertical Distance to Top (meters)") +
  theme_minimal() +
  theme(legend.position = "none")

# There seems to be a lot of variation


## Modelling

distance_modsp1 <- glmer(
  distance ~ 
    bs(year, knots = c(2009), degree = 1) + (1 | species), 
  family = poisson, 
  data = distance_data)
# If I were to consider summit
# distance_modsp2 <- glm(
#   distance ~ 
#     bs(year, knots = c(2009), degree = 1) + summit, 
#   family = poisson, 
#   data = distance)
# anova(distance_modsp1, distance_modsp2) # I keep the interaction

distance_modsp1 |> summary()

distance_predicted <- distance_modsp1 |> 
  ggpredict(terms = c("year")) |> 
  rename(year = x, summit = group)

distance_mod_graph <- 
  ggplot() + 
  geom_jitter(data = distance, aes(x = year, y = distance), size = 2, width = 0.5, height = 0.5) + # Points for each observation
  geom_hline(yintercept = c(10, 20, 30), colour = "lightgrey") + 
  geom_line(data = distance_predicted, aes(x = year, y = predicted), lwd = 0.75, colour = "blue") + # Line for predicted values
  geom_ribbon(data = distance_predicted, aes(x = year, ymin = conf.low, ymax = conf.high), fill = "blue", alpha = 0.2) + # Confidence interval
  labs(x = "Year", y = "Distance to top") +
  theme_classic() + 
  theme(text = element_text(size = 20, family = "serif"), 
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)), 
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)))
distance_mod_graph
distance_mod_graph |> ggsave(file = "Graphs/Distance.jpeg")




# Distance change----

distance_change_data |> 
  ggplot(aes(x = interval, y = distance_change)) +
  geom_violin() + 
  labs(title = " Distances by Year",
       x = "Year",
       y = "Vertical Distance to Top (meters)") +
  theme_minimal()

distance_change_data |> 
  ggplot(aes(x = summit:interval, y = distance_change, color = summit)) +
  geom_boxplot() +
  labs(title = "Line Plot of Vertical Distances for Each Species Across Years",
       x = "Year",
       y = "Vertical Distance to Top (meters)") +
  theme_minimal() +
  theme(legend.position = "none")

distance_change_data |> 
  ggplot(aes(x = interval, y = distance_change, color = species, group = species)) + 
  facet_wrap(~summit) + 
  geom_line() +
  labs(title = "Scatter Plot of Vertical Distances by Year for Each Mountain",
       x = "Year",
       y = "Vertical Distance to Top (meters)") +
  theme_minimal() +
  theme(legend.position = "none")


## Modelling change

hist(distance_change_data$distance_change)

distance_change_mod <- glmmTMB(
  distance_change ~ 
    interval + (1 | summit) + (1 | species), 
  family = gaussian, 
  data = distance_change_data)

# distance_change_mod |> model_diagnosis()
# distance_change_mod |> model_homoscedasticity()
distance_change_mod |> summary()


distance_change_predicted <- distance_change_mod |> 
  ggpredict(terms = c("interval")) |> 
  rename(interval = x)

distance_mod_graph <- 
  ggplot() + 
  geom_violin(data = distance_change_data, aes(x = interval, y = distance_change)) + 
  geom_hline(yintercept = 0, colour = "black") + 
  geom_hline(yintercept = c(-30, -15, 15, 30), colour = "lightgrey") + 
  geom_line(data = distance_change_predicted, aes(x = as.numeric(interval), y = predicted), lwd = 0.75, colour = "blue") + # Line for predicted values
  geom_ribbon(data = distance_change_predicted, aes(x = as.numeric(interval), ymin = conf.low, ymax = conf.high), fill = "blue", alpha = 0.2) + # Confidence interval
  labs(x = "Interval", y = "Distance to top") +
  theme_classic() + 
  theme(text = element_text(size = 20, family = "serif"), 
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)), 
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)))
distance_mod_graph
distance_mod_graph |> ggsave(file = "Graphs/Distance.jpeg")



# Distance change. Rate----

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



# Distance. Species not in the top get a distance of 33----

## Exploratoy graphs

distance33 |> 
  ggplot(aes(x = as.factor(year), y = adj_distance)) +
  geom_violin() + 
  labs(title = " Distances by Year",
       x = "Year",
       y = "Vertical Distance to Top (meters)") +
  theme_minimal()

distance33 |> 
  ggplot(aes(x = summit:as.factor(year), y = adj_distance, color = summit)) +
  geom_boxplot() +
  labs(title = "Line Plot of Vertical Distances for Each Species Across Years",
       x = "Year",
       y = "Vertical Distance to Top (meters)") +
  theme_minimal() +
  theme(legend.position = "none")

distance33 |> 
  ggplot(aes(x = year, y = adj_distance, color = species)) + 
  facet_wrap(~summit) + 
  geom_line() +
  labs(title = "Scatter Plot of Vertical Distances by Year for Each Mountain",
       x = "Year",
       y = "Vertical Distance to Top (meters)") +
  theme_minimal() +
  theme(legend.position = "none")

# Difficult to say anything
# All models give huge errors


## I am not interested in the distance per se, but in change in distance. I check that

distance33_change |> 
  ggplot(aes(x = interval, y = distance_change)) +
  geom_violin() + 
  labs(title = " Distances by Year",
       x = "Year",
       y = "Vertical Distance to Top (meters)") +
  theme_minimal()

distance33_change |> 
  ggplot(aes(x = summit:interval, y = distance_change, color = summit)) +
  geom_boxplot() +
  labs(title = "Line Plot of Vertical Distances for Each Species Across Years",
       x = "Year",
       y = "Vertical Distance to Top (meters)") +
  theme_minimal() +
  theme(legend.position = "none")

distance33_change |> 
  ggplot(aes(x = interval, y = distance_change, color = species, group = species)) + 
  facet_wrap(~summit) + 
  geom_line() +
  labs(title = "Scatter Plot of Vertical Distances by Year for Each Mountain",
       x = "Year",
       y = "Vertical Distance to Top (meters)") +
  theme_minimal() +
  theme(legend.position = "none")



## Modelling

hist(distance33_change$distance_change)

distance33_change_mod <- glmmTMB(
  distance_change ~ 
    interval + (1 | summit) + (1 | species), 
  family = gaussian, 
  data = distance33_change)

# distance_change_mod |> model_diagnosis()
# distance_change_mod |> model_homoscedasticity()
distance33_change_mod |> summary()







# Each interval by its own----

distance_int1 <- filefjell_data_clean |> 
  filter(year != 2024) |> 
  pivot_wider(names_from = year, names_prefix = "y", values_from = distance) |> 
  filter(!is.na(y1972) & !is.na(y2009)) |> 
  pivot_longer(cols = c("y1972", "y2009"), names_prefix = "y", names_to = "year", values_to = "distance") |> 
  relocate(year)

distance_int1_aov <- aov(distance ~ year, data = distance_int1)
distance_int1_aov |> summary()


distance_int2 <- filefjell_data_clean |> 
  filter(year != 1972) |> 
  pivot_wider(names_from = year, names_prefix = "y", values_from = distance) |> 
  filter(!is.na(y2009) & !is.na(y2024)) |> 
  pivot_longer(cols = c("y2009", "y2024"), names_prefix = "y", names_to = "year", values_to = "distance") |> 
  relocate(year)

distance_int2_aov <- aov(distance ~ year, data = distance_int2)
distance_int2_aov |> summary()





# Richness----

## Exploratoy graphs

richness_data |> 
  ggplot(aes(x = as.factor(year), y = richness)) +
  geom_violin() + 
  labs(title = " Distances by Year",
       x = "Year",
       y = "Vertical Distance to Top (meters)") +
  theme_minimal()

richness_data |> 
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
  data = richness_data)
richness_mod |> model_diagnosis()
richness_mod |> summary()


richness_predicted <- richness_mod |> 
  ggpredict(terms = "year") |> 
  rename(year = x)

richness_mod_graph <- ggplot() + 
  geom_point(data = richness_data, aes(x = year, y = richness), size = 2) + # Points for each observation
  geom_hline(yintercept = c(20, 40, 60, 80), colour = "lightgrey") + 
  geom_line(data = richness_predicted, aes(x = year, y = predicted), lwd = 0.75, colour = "blue") + # Line for predicted values
  geom_ribbon(data = richness_predicted, aes(x = year, ymin = conf.low, ymax = conf.high), fill = "blue", alpha = 0.2) + # Confidence interval
  labs(x = "Year", y = "Richness") +
  theme_classic() + 
  theme(text = element_text(size = 20, family = "serif"), 
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)), 
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)))
richness_mod_graph
richness_mod_graph |> ggsave(file = "Graphs/Richness.jpeg")



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

colonizers |> 
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



