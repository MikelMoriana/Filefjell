# Libraries----

library(ggeffects)
library(vegan)

source("Scripts/0_setup.R")



# Data----

filefjell_data_clean <- tar_read(filefjell_data_clean)
filefjell_visit_dates <- read_csv2("data_raw/Visit_dates.csv")
filefjell_visit_dates_wide <- filefjell_visit_dates |> 
  clean_names() |> 
  select(summit, year) |>
  mutate(summit =ifelse(summit == "Krekanosi S", "Krekanosi_S", summit)) |> 
  pivot_wider(names_from = year, names_prefix = "y", values_from = year) |> 
  rename(first = y1972) |> 
  mutate(second = coalesce(y2008, y2009)) |> 
  mutate(third = coalesce(y2024, y2025)) |> 
  select(summit, first, second, third)

## Distance

# Since we are using change, we decide to use change in elevation instead of change in distance, since it is more intuitive (we subtract the value the first year from the value the second year)

elevation_change <- filefjell_data_clean |>
  select(!c(date, recorder)) |>
  pivot_wider(names_from = year, names_prefix = "y", values_from = distance) |>
  mutate(distance2 = coalesce(y2008, y2009),
         distance3 = coalesce(y2024, y2025)) |>
  mutate(year1 = case_when(!is.na(y1972) ~ 1972),
         year2 = case_when(!is.na(y2008) ~ 2008,
                           !is.na(y2009) ~ 2009),
         year3 = case_when(!is.na(y2024) ~ 2024,
                           !is.na(y2025) ~ 2025)) |> 
  relocate(year1, .before = y1972) |>
  relocate(year2, .after = y1972) |>
  relocate(year3, .before = distance3) |>
  select(!c(y2008, y2009, y2024, y2025)) |>
  rename(distance1 = y1972)

elevation_change_new <- elevation_change |> 
  left_join(filefjell_visit_dates_wide, by = "summit") |> 
  mutate(year1 = ifelse(is.na(year1) & !is.na(distance2), first, year1),
         adj_distance1 = ifelse(is.na(distance1) & !is.na(distance2), 33, distance1),
         year2 = ifelse(is.na(year2) & !is.na(distance3), second, year2),
         adj_distance2 = ifelse(is.na(distance2) & !is.na(distance3), 33, distance2)) |>
  relocate(adj_distance1, .after = distance1) |> 
  relocate(adj_distance2, .after = distance2) |> 
  select(!c(first, second, third))


# Considering only species found all years at a summit

elecha_all <- elevation_change |>
  mutate(period1 = year2 - year1,
         change1 = distance1 - distance2,
         period2 = year3 - year2,
         change2 = distance2 - distance3) |>
  filter(!is.na(change1) & !is.na(change2)) |>
  pivot_longer(cols = c(period1, period2), names_to = "period", values_to = "years") |>
  mutate(period = as.factor(period)) |>
  mutate(change = case_when(period == "period1" ~ change1,
                            period == "period2" ~ change2)) |>
  mutate(rate = change / years) |>
  select(!c(change1, change2))

# Considering also new species, giving them a conservative value of 33 metres below the top

elecha_new <- elevation_change_new |>
  mutate(period1 = year2 - year1,
         change1 = adj_distance1 - adj_distance2,
         period2 = year3 - year2,
         change2 = adj_distance2 - distance3) |>
  filter(!is.na(change1) & !is.na(change2)) |>
  pivot_longer(cols = c(period1, period2), names_to = "period", values_to = "years") |>
  mutate(period = as.factor(period)) |>
  mutate(change = case_when(period == "period1" ~ change1,
                            period == "period2" ~ change2)) |>
  mutate(rate = change / years) |>
  select(!c(change1, change2))


# # Considering the two periods separately (i.e., all species found in both samplings of a period are considered)
# 
# elevation_change_two_data <- filefjell_data_clean |> 
#   pivot_wider(names_from = year, names_prefix = "y", values_from = distance) |> 
#   mutate(per1 = case_when(is.na(y1972) ~ NA, 
#                           is.na(y2009) ~ NA, 
#                           !is.na(y2009) & !is.na(y1972) ~ y2009 - y1972), 
#          per2 = case_when(is.na(y2009) ~ NA, 
#                           is.na(y2024) ~ NA, 
#                           !is.na(y2024) & !is.na(y2009) ~ y2024 - y2009)) |> 
#   select(-c(y1972, y2009, y2024)) |> 
#   pivot_longer(cols =c("per1", "per2"), names_to = "period", values_to = "distance_change") |> 
#   mutate(elevation_change = distance_change * -1) |> 
#   select(-distance_change) |> 
#   mutate(elevation_change_rate = case_when(period == "per1" ~ elevation_change / 37, 
#                                           period == "per2" ~ elevation_change / 15)) |> 
#   filter(!is.na(elevation_change)) |> 
#   mutate(period = as.factor(period)) |> 
#   arrange(summit, species, period)
# 
# 
# 
# ## Richness
# 
# richness_data <- filefjell_data_clean |> 
#   summarise(.by = c(year, summit, elevation), 
#             richness = n())
# 
# richness_change_data <- richness_data |> 
#   pivot_wider(names_from = year, names_prefix = "y", values_from = richness) |> 
#   mutate(per1 = y2009 - y1972, 
#          per2 = y2024 - y2009) |> 
#   select(-c(y1972, y2009, y2024)) |> 
#   pivot_longer(cols = c(per1, per2), names_to = "period", values_to = "richness_change") |> 
#   mutate(richness_change_rate = case_when(period == "per1" ~ richness_change / 37, 
#                                           period == "per2" ~ richness_change / 15)) |> 
#   mutate(period = as.factor(period))
# 
# 
# # Turnover
# 
# species_turnover_data <- filefjell_data_clean |> 
#   mutate(presence = ifelse(!is.na(distance), 1, 0)) |> 
#   select(-distance) |> 
#   arrange(species) |> 
#   pivot_wider(names_from = "year", names_prefix = "y", values_from = "presence", values_fill = 0) |> 
#   arrange(summit, species) |> 
#   mutate(turnover09 = y2009 - y1972) |> 
#   mutate(turnover24 = y2024 - y2009)
# 
# turnover_data <- species_turnover_data |> 
#   summarise(.by = summit, 
#             per1_lost = sum(turnover09 == -1), 
#             per1_nochange = sum(turnover09 == 0), 
#             per1_new = sum(turnover09 == 1), 
#             per2_lost = sum(turnover24 == -1), 
#             per2_nochange = sum(turnover24 == 0), 
#             per2_new = sum(turnover24 == 1)) |> 
#   pivot_longer(cols = starts_with("per"), 
#                names_to = c("period", ".value"), 
#                names_sep = "_") |> 
#   mutate(number_years = ifelse(period == "per1", 37, 15)) |> 
#   mutate(lost_rate = lost / number_years, 
#          new_rate = new / number_years) |> 
#   mutate(period = as.factor(period))
# 
# 
# # New species by nature type
# 
# filefjell_2024_clean <- tar_read(filefjell_2024_clean)
# 
# colonizers_data <- filefjell_2024_clean |> 
#   semi_join(
#     species_turnover_data |> 
#       filter(turnover24 == 1) |> 
#       select(summit:species)) |> 
#   mutate(hovedtype = str_extract(type, "[^C]*")) |> 
#   relocate(hovedtype, .before = type)
# 
# colonizers_hovedtype_data <- colonizers_data |> 
#   summarise(.by = c(summit, hovedtype, cover), 
#             new_species = n()) |> 
#   left_join(filefjell_2024_clean |> summarise(.by = c(summit, hovedtype), total_species = n())) |> 
#   mutate(hovedtype = as.factor(hovedtype), 
#          new_area = new_species / cover, 
#          ratio_species = (new_species / total_species) * 0.999 + 0.0005)

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



# Elevation change----

## Only species found all three years TO FINISH

# Model

elecha_all |> 
  ggplot() +
  geom_histogram(aes(x = rate))

elecha_all_rate_mod <- glmmTMB(
  rate ~ 
    period + (1 | summit) + (1 | species), 
  family = gaussian, 
  data = elecha_all)

elecha_all_rate_mod |> model_diagnosis()
elecha_all_rate_mod |> model_homoscedasticity()
elecha_all_rate_mod |> summary()

# No distributions I try get closely to fitting. I try bayesian

elecha_all_rate_bayes <- brm(
  rate ~ 
    period + (1|summit) + (1|species),
  family = student(), 
  prior(gamma(55, 0.1), class = "nu"),
  data = elecha_all,
  seed = 811)

# Diagnosis

elecha_all_rate_bayes |> summary() # Converged
elecha_all_rate_bayes |> plot() # "hairy caterpillars"

withr::with_seed(811, pp_check(elecha_all_rate_bayes, type = "dens_overlay"))
withr::with_seed(811, pp_check(elecha_all_rate_bayes, type = "hist"))
withr::with_seed(811, pp_check(elecha_all_rate_bayes, type = "scatter_avg"))
withr::with_seed(811, pp_check(elecha_all_rate_bayes, type = "stat"))

elecha_all_df <- data.frame(fitted = fitted(elecha_all_rate_bayes)[,1],
                            resid = residuals(elecha_all_rate_bayes)[,1],
                            predictor = elecha_all_rate_bayes$data$period)
plot(elecha_all_df$fitted, elecha_all_df$resid, 
     xlab = "Fitted values", ylab = "Residuals")
ggplot(elecha_all_df, aes(x = predictor, y = resid)) +
  geom_point() +
  geom_smooth(method = "loess") +
  theme_minimal()
    
bayes_R2(elecha_all_rate_bayes)
loo(elecha_all_rate_bayes)

elecha_all_rate_results <- elecha_all_rate_mod |> 
  ggeffects::ggpredict(terms = "period") |> 
  rename(period = x) |> 
  as.data.frame() |> 
  mutate(model = "elevation") |> 
  relocate(model)


## Including new species

elecha_new |> 
  ggplot() +
  geom_histogram(aes(x = rate))

elecha_new_rate_mod <- glmmTMB(
  rate ~ 
    period + (1 | summit) + (1 | species), 
  family = t_family,
  data = elecha_new)

elecha_new_rate_mod |> model_diagnosis()
elecha_new_rate_mod |> model_homoscedasticity()
elecha_new_rate_mod |> summary()

# No distributions I try get closely to fitting. I try bayesian

elecha_new_rate_bayes <- brm(
  rate ~ 
    period + (1|summit) + (1|species),
  family = student(), 
  data = elecha_new,
  seed = 811)

# Diagnosis

elecha_new_rate_bayes |> summary() # Converged
elecha_new_rate_bayes |> plot() # "hairy caterpillars"

withr::with_seed(811, pp_check(elecha_new_rate_bayes, type = "dens_overlay"))
withr::with_seed(811, pp_check(elecha_new_rate_bayes, type = "hist"))
withr::with_seed(811, pp_check(elecha_new_rate_bayes, type = "scatter_avg"))
withr::with_seed(811, pp_check(elecha_new_rate_bayes, type = "stat"))

# Extract posterior draws
draws <- as_draws_df(elecha_new_rate_bayes)

# Compute probabilities
prob_intercept_gt0 <- mean(draws$b_Intercept > 0)
prob_period_lt0 <- mean(draws$b_periodperiod2 < 0)

prob_intercept_gt0
prob_period_lt0

bayesplot::mcmc_areas(
    draws,
    pars = c("b_Intercept", "b_periodperiod2"),
    prob = 0.95  # 95% credible interval
  )

# The intercept had a posterior mean of 0.16 (95% CI: 0.12–0.19), with a posterior probability of being > 0 equal to 1.00.
# The effect of period was -0.12 (95% CI: -0.16 to -0.08), with a posterior probability of being < 0 equal to 1.00.







pp_check(elecha_new_rate_bayes)

elecha_new_rate_bayes |> summary()

hypothesis(elecha_new_rate_bayes, "Intercept > 0")  # Is period 1 > 0?
hypothesis(elecha_new_rate_bayes, "Intercept + periodperiod2 > 0")  # Is period 2 > 0?
hypothesis(elecha_new_rate_bayes, "periodperiod2 < 0")  # Is period 2 lower than period 1?

em <- emmeans::emmeans(elecha_new_rate_bayes, ~ period)
summary(em)
plot(em)


# ## Species found either in 1972 and 2009, or 2009 and 2024
# 
# hist(elevation_change_two_data$elevation_change_rate)
# 
# elevation_change_two_rate_mod <- glmmTMB(
#   elevation_change_rate ~ 
#     period + (1 | summit) + (1 | species), 
#   family = gaussian, 
#   data = elevation_change_two_data)
# 
# # elevation_change_two_rate_mod |> model_diagnosis()
# # elevation_change_two_rate_mod |> model_homoscedasticity()
# elevation_change_two_rate_mod |> summary()
# 
# elevation_two_results <- elevation_change_two_rate_mod |> 
#   ggpredict(terms = "period") |> 
#   rename(period = x) |> 
#   as.data.frame() |> 
#   mutate(model = "elevation") |> 
#   relocate(model)
# 
# test_elevation_two <- elevation_change_two_data |> 
#   mutate(period = factor(period, level = c("per2", "per1")))
# test_elevation_two_mod <- glmmTMB(
#   elevation_change_rate ~ 
#     period + (1 | summit) + (1 | species), 
#   family = gaussian, 
#   data = test_elevation_two)
# test_elevation_two_mod |> summary()
# 
# 
# 
# 
# # Richness change----
# 
# ## Exploratoy graphs
# 
# richness_change_data |> 
#   ggplot(aes(x = period, y = richness_change_rate)) +
#   geom_violin() + 
#   labs(title = " elevations by Year",
#        x = "Year",
#        y = "Vertical elevation to Top (meters)") +
#   theme_minimal()
# 
# richness_change_data |> 
#   ggplot(aes(x = summit:period, y = richness_change_rate, color = summit)) +
#   geom_point() +
#   labs(title = "Plot of richness change per year",
#        x = "Year",
#        y = "Richness change (species/year)") +
#   theme_minimal() +
#   theme(legend.position = "none")
# 
# # Some summits gained species at a faster rate, some at a slower, and some lost species in the second period
# 
# 
# ## Modelling
# 
# hist(richness_change_data$richness_change_rate)
# 
# richness_change_rate_mod <- glmmTMB(
#   richness_change_rate ~ 
#     period + (1 | summit), 
#   family = gaussian, 
#   data = richness_change_data)
# 
# richness_change_rate_mod |> model_diagnosis()
# richness_change_rate_mod |> summary()
# 
# richness_results <- richness_change_rate_mod |> 
#   ggpredict(terms = "period") |> 
#   rename(period = x) |> 
#   as.data.frame() |> 
#   mutate(model = "richness") |> 
#   relocate(model)
# 
# 
# 
# 
# # Turnover----
# 
# turnover_data |> 
#   summarise(.by = period, 
#             new_mean = mean(new), 
#             nochange_mean = mean(nochange),
#             lost_mean = mean(lost))
# 
# turnover_data |> 
#   summarise(.by = period, 
#             new_mean = mean(new_rate),
#             lost_mean = mean(lost_rate))
# 
# # I don't analyse or plot nochange. One being greater than the other is just a measure of richness, not of change
# 
# # New species
# 
# turnover_data |> 
#   ggplot(aes(x = period, y = new_rate)) + 
#   geom_violin()
# 
# new_rate_mod <- glmmTMB(
#   new_rate*555 ~ 
#     period + (1 | summit), 
#   family = nbinom2, 
#   data = turnover_data)
# 
# new_rate_mod |> model_diagnosis()
# new_rate_mod |> summary()
# 
# new_results <- new_rate_mod |> 
#   ggpredict(terms = "period") |> 
#   rename(period = x) |> 
#   as.data.frame() |> 
#   mutate(predicted = predicted/555, 
#          conf.low = conf.low/555, 
#          conf.high = conf.high/555) |> 
#   mutate(model = "new") |> 
#   relocate(model)
# 
# # More new species in the second period, but not significantly
# 
# 
# 
# # Lost species
# 
# turnover_data |> 
#   ggplot(aes(x = period, y = lost_rate)) + 
#   geom_violin()
# 
# hist(turnover_data$lost_rate*555)
# 
# lost_rate_mod <- glmmTMB(
#   lost_rate*555 ~ 
#     period + (1 | summit), 
#   family = nbinom2, 
#   ziformula = ~1, 
#   data = turnover_data)
# 
# lost_rate_mod |> model_diagnosis()
# lost_rate_mod |> summary()
# # Greater species loss in the second period
# 
# lost_results <- lost_rate_mod |> 
#   ggpredict(terms = "period") |> 
#   rename(period = x) |> 
#   as.data.frame() |> 
#   mutate(predicted = predicted/555, 
#          conf.low = conf.low/555, 
#          conf.high = conf.high/555) |> 
#   mutate(model = "lost") |> 
#   relocate(model)
# 
# 
# 
# turnover_raw_graph <- turnover_data |> 
#   select(summit, period, lost_rate, new_rate) |> 
#   pivot_longer(cols = c(lost_rate, new_rate), names_to = "species_change", values_to = "change") |> 
#   ggplot(aes(x = period, y = change, fill = period)) + 
#   geom_violin() + 
#   stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", colour = "white") + 
#   facet_grid(cols = vars(species_change), labeller = adj_label) + 
#   scale_fill_manual("period", values = c("per1" = "#859395", "per2" = "#f58800"), labels = c("1972-2009", "2009-2024")) + 
#   labs(x = "period", y = "Species / Year") + 
#   theme_bw() + 
#   theme(text = element_text(size = 20, family = "serif"), 
#         axis.title.x = element_blank(), 
#         axis.text.x = element_blank(), 
#         axis.ticks.x = element_blank(), 
#         axis.title.y = element_text(margin = margin(r = 5)), 
#         legend.position = "top", 
#         legend.title = element_text(margin = margin(r = 30)), 
#         legend.text = element_text(margin = margin(l = 9, r = 20)))
# turnover_raw_graph
# turnover_raw_graph |> ggsave(file = "Graphs/Turnover.jpeg")
# 
# 
# 
# 
# # Results----
# 
# results_table <- elevation_results |> 
#   rbind(richness_results) |> 
#   rbind(new_results) |> 
#   rbind(lost_results) |> 
#   mutate(model = factor(model, levels = c("elevation", "richness", "new", "lost")))
# 
# facet_labels <- data.frame(model = unique(results_table$model), label = letters[1:4], x = -0.2, y = 1.5)
# 
# results_graph <- results_table |> 
#   mutate(period = factor(period, levels = c("per2", "per1"))) |> 
#   ggplot(aes(x = predicted, y = period, colour = period)) + 
#   theme_minimal() + 
#   facet_grid(rows = vars(model), switch = "y", labeller = adj_label) + 
#   geom_vline(xintercept = 0, colour = "black") +
#   geom_vline(xintercept = c(-0.2, 0.2, 0.4, 0.6, 0.8), colour = "lightgrey") + 
#   geom_point(size = 3) + 
#   geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.3) + 
#   scale_x_continuous(limits = c(-0.2, 0.92), breaks = c(0.0, 0.4, 0.8)) + 
#   scale_colour_manual("Period", values = colour_mapping$period, labels = adj_label) + 
#   guides(colour = guide_legend(reverse = TRUE)) + 
#   labs(x = "Rate") + 
#   theme(panel.spacing.y = unit(2, "lines"), 
#         text = element_text(size = 16, family = "serif"), 
#         axis.title.x = element_text(hjust = 0.535), 
#         axis.text.x = element_text(margin = margin(t = 10, b = 10)), 
#         strip.text.y.left = ggtext::element_markdown(angle = 0, hjust = 0), 
#         axis.title.y = element_blank(), 
#         axis.text.y = element_blank(), 
#         panel.grid.major.y = element_blank(),
#         legend.position = "top", 
#         legend.box.margin = margin(l = 85), 
#         legend.title = element_text(margin = margin(b = 5, r = 40)), 
#         legend.text = element_text(margin = margin(l = 9, r = 20, b = 4)))
# results_graph
# results_graph |> ggsave(file = "Graphs/Results.jpeg", width = 20, height = 15, units = "cm")
# 
# 
# 
# 
# # Colonizers. When doing this analysis consider whether species present in 1972 (that disappeared in 2010) should be taken into account ----
# 
# ## Total
# 
# colonizers_hovedtype_data |> 
#   ggplot(aes(x = hovedtype, y = new_species)) +
#   geom_boxplot() + 
#   labs(title = " New species by hovedtype",
#        x = "Year",
#        y = "New species") +
#   theme_minimal()
# 
# colonizers_hovedtype_mod <- glmmTMB(
#   new_species ~ 
#     hovedtype + (1 | summit), 
#   family = poisson, 
#   data = colonizers_hovedtype_data
# )
# colonizers_hovedtype_mod |> model_diagnosis()
# colonizers_hovedtype_mod |> summary()
# 
# 
# ## Ratio
# 
# colonizers_hovedtype_data |> 
#   ggplot(aes(x = hovedtype, y = ratio_species)) +
#   geom_boxplot() + 
#   labs(title = " New species by hovedtype",
#        x = "Year",
#        y = "New species") +
#   theme_minimal()
# 
# colonizers_hovedtype_ratio_mod <- glmmTMB(
#   ratio_species ~ 
#     hovedtype + (1 | summit), 
#   family = beta_family, 
#   data = colonizers_hovedtype_data
# )
# colonizers_hovedtype_ratio_mod |> model_diagnosis()
# colonizers_hovedtype_ratio_mod |> summary()
# 
# hovedtype_ratio_results <- colonizers_hovedtype_ratio_mod |> 
#   ggpredict(terms = "hovedtype") |> 
#   rename(hovedtype = x) |> 
#   as.data.frame() |> 
#   mutate(model = "ratio_species") |> 
#   relocate(model)
# 
# 
# 
# ## By area
# 
# colonizers_hovedtype_data |> 
#   filter(!is.na(new_area)) |> 
#   ggplot(aes(x = hovedtype, y = new_area)) +
#   geom_boxplot() + 
#   labs(title = " New species ratio by hovedtype",
#        x = "Year",
#        y = "New species ratio") +
#   theme_minimal()
# hist(colonizers_hovedtype_data$new_area)
# 
# colonizers_hovedtype_area_mod <- glmmTMB(
#   new_area ~ 
#     hovedtype + (1 | summit), 
#   family = Gamma, 
#   data = colonizers_hovedtype_data
# )
# colonizers_hovedtype_area_mod |> model_diagnosis()
# colonizers_hovedtype_area_mod |> summary()
# 
# hovedtype_area_results <- colonizers_hovedtype_area_mod |> 
#   ggpredict(terms = "hovedtype") |> 
#   rename(hovedtype = x) |> 
#   as.data.frame() |> 
#   mutate(model = "ratio_species") |> 
#   relocate(model)
# 
# 
# 
# # Unconstrained ordination----
# 
# set.seed(811)
# 
# community_nmds <- community_species |> metaMDS(k = 2, distance = "jaccard", trymax = 0900)
# community_nmds_sites <- community_nmds |> scores(display = "sites") |> as_tibble()
# community_nmds_species <- community_nmds |> scores(display = "species") |> as_tibble()
# 
# community_presence_sites <- community_metadata |> 
#   cbind(community_nmds_sites)
# 
# community_presence_sites |> 
#   ggplot() + 
#   geom_point(aes(x = NMDS1, y = NMDS2, colour = summit), size = 5) + 
#   geom_segment(data = community_presence_sites |> 
#                  group_by(summit) |> 
#                  mutate(next_NMDS1 = lead(NMDS1),
#                         next_NMDS2 = lead(NMDS2)) |> 
#                  filter(!is.na(next_NMDS1)),
#                aes(x = NMDS1, y = NMDS2, xend = next_NMDS1, yend = next_NMDS2),
#                arrow = arrow(length = unit(0.3, "cm"))) + 
#   geom_text(data = community_presence_sites |> 
#               filter(year == 1972), 
#             aes(x = NMDS1, y = NMDS2 - 0.035, label = summit), size = 5) + 
#   theme_bw()
# 
# 
# 
# species_list <- community_species |> 
#   pivot_longer(cols = ach_mil:vis_alp, names_to = "species") |> 
#   filter(value == 1) |> 
#   arrange(species) |> 
#   distinct()
# 
# community_presence_species <- species_list |> 
#   select(-value) |> 
#   cbind(community_nmds_species)
# 
# community_presence_species |> 
#   ggplot() + 
#   geom_text(aes(x = NMDS1, y = NMDS2, label = species), size = 5) + 
#   theme_bw()
# 
# 

# Old----

filefjell_data_clean <- tar_read(filefjell_data_clean) |> 
  filter(summit != "Krekanosi_S") |> filter(year != 2025)

elevation_change_three_data <- filefjell_data_clean |> 
  pivot_wider(names_from = year, names_prefix = "y", values_from = distance) |> 
  filter(!is.na(y1972) & !is.na(y2009) & !is.na(y2024)) |> 
  mutate(period1 = y2009 - y1972, 
         period2 = y2024 - y2009) |> 
  select(-c(y1972, y2009, y2024)) |>
  pivot_longer(cols =c("period1", "period2"), names_to = "period", values_to = "distance_change") |> 
  mutate(elevation_change = distance_change * -1) |> 
  select(-distance_change) |> 
  mutate(elevation_change_rate = case_when(period == "period1" ~ elevation_change / (2009 - 1972), 
                                           period == "period2" ~ elevation_change / (2024 - 2009))) |> 
  mutate(period = as.factor(period)) |> 
  arrange(summit, species, period)


# Considering also new species, giving them a conservative value of 33 metres below the top

elevation_change_new_data <- filefjell_data_clean |> 
  pivot_wider(names_from = year, names_prefix = "y", values_from = distance) |> 
  mutate(period1 = case_when(is.na(y2009) ~ NA, 
                             !is.na(y2009) & is.na(y1972) ~ y2009 - 33, 
                             !is.na(y2009) & !is.na(y1972) ~ y2009 - y1972), 
         period2 = case_when(is.na(y2024) ~ NA, 
                             !is.na(y2024) & is.na(y2009) ~ y2024 - 33, 
                             !is.na(y2024) & !is.na(y2009) ~ y2024 - y2009)) |> 
  select(-c(y1972, y2009, y2024)) |> 
  pivot_longer(cols =c("period1", "period2"), names_to = "period", values_to = "distance_change") |> 
  mutate(elevation_change = distance_change * -1) |> 
  select(-distance_change) |> 
  mutate(elevation_change_rate = case_when(period == "period1" ~ elevation_change / 37, 
                                           period == "period2" ~ elevation_change / 15)) |> 
  filter(!is.na(elevation_change)) |> 
  mutate(period = as.factor(period)) |> 
  arrange(summit, species, period)

hist(elevation_change_new_data$elevation_change_rate)

elevation_change_rate_mod <- glmmTMB(
  elevation_change_rate ~
    period + (1 | summit) + (1 | species), 
  family = gaussian, 
  data = elevation_change_new_data)
elevation_change_rate_mod |> summary()

# elevation_change_new_data |> 
#   ggplot(aes(x = period, y = elevation_change_rate)) +
#   geom_violin() + 
#   labs(title = "Change in elevations by Year",
#        x = "Year",
#        y = "Vertical elevation to Top (meters)") +
#   theme_minimal()
# 
# elevation_change_new_data |> 
#   ggplot(aes(x = summit:period, y = elevation_change_rate, color = summit)) +
#   geom_boxplot() +
#   labs(title = "Line Plot of Vertical elevations for Each Species Across Years",
#        x = "Year",
#        y = "Vertical elevation to Top (meters)") +
#   theme_minimal() +
#   theme(legend.position = "none")
# 
# elevation_change_data |> 
#   ggplot(aes(x = period, y = elevation_change_rate, color = species, group = species)) + 
#   facet_wrap(~summit) + 
#   geom_line() +
#   labs(title = "Scatter Plot of Vertical elevations by Year for Each Mountain",
#        x = "Year",
#        y = "Vertical elevation to Top (meters)") +
#   theme_minimal() +
#   theme(legend.position = "none")
# 
# 
# ## Modelling
# 