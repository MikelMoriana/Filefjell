# Libraries----

library(ggeffects)
library(vegan)

source("Scripts/0_setup.R")



# Data----

filefjell_data_clean <- tar_read(filefjell_data_clean)
filefjell_visit_years <- filefjell_data_clean |> 
  select(year, summit) |> 
  distinct() |> 
  pivot_wider(names_from = year, names_prefix = "y", values_from = year) |> 
  mutate(first = 1972, 
         second = coalesce(y2008, y2009),
         third = coalesce(y2024, y2025)) |> 
  select(summit, first, second, third)


## Distance

elevation_species <- filefjell_data_clean |> 
  select(!c(date, recorder)) |> 
  pivot_wider(names_from = year, names_prefix = "y", values_from = distance) |> 
  left_join(filefjell_visit_years, by = "summit") |> 
  mutate(third = ifelse(!is.na(y2025), 2025, third), 
         distance1 = y1972,
         distance2 = coalesce(y2008, y2009), 
         distance3 = coalesce(y2024, y2025)) |> 
  select(!c(y1972, y2008, y2009, y2024, y2025)) |> 
  mutate(period1 = second - first, 
         period2 = third - second)

elevation_species_new <- elevation_species |> 
  mutate(adj_dist1 = ifelse(is.na(distance1) & !is.na(distance2), 33, distance1),
         adj_dist2 = ifelse(is.na(distance2) & !is.na(distance3), 33, distance2))

# As we are using change, we decide to use change in elevation instead of change in distance, since it is more intuitive (we subtract the value the first year from the value the second year)

# Considering only species found all years at a summit
elerate_all <- elevation_species |>
  mutate(change1 = distance1 - distance2, 
         change2 = distance2 - distance3) |> 
  filter(!is.na(change1) & !is.na(change2)) |>
  pivot_longer(cols = c(period1, period2), names_to = "period", values_to = "years") |>
  mutate(period = as.factor(period)) |>
  mutate(change = case_when(period == "period1" ~ change1,
                            period == "period2" ~ change2)) |>
  mutate(rate = change / years) |>
  select(!c(change1, change2))

# Considering also new species, giving them a conservative value of 33 metres below the top
elerate_new <- elevation_species_new |>
  mutate(change1 = adj_dist1 - adj_dist2, 
         change2 = adj_dist2 - distance3) |> 
  filter(!is.na(change1) & !is.na(change2)) |>
  pivot_longer(cols = c(period1, period2), names_to = "period", values_to = "years") |>
  mutate(period = as.factor(period)) |>
  mutate(change = case_when(period == "period1" ~ change1,
                            period == "period2" ~ change2)) |>
  mutate(rate = change / years) |>
  select(!c(change1, change2))


## Turnover and richness

turnover_species <- elevation_species |>
  mutate(presence1 = ifelse(is.na(distance1), 0, 1),
         presence2 = ifelse(is.na(distance2), 0, 1),
         presence3 = ifelse(is.na(distance3), 0, 1)) |>
  select(!c(distance1, distance2, distance3)) |> 
  mutate(turnover1 = presence2 - presence1,
         turnover2 = presence3 - presence2) |> 
  mutate(rate1 = turnover1 / period1,
         rate2 = turnover2 / period2) |> 
  pivot_longer(cols = c(period1, period2), names_to = "period", values_to = "years") |> 
  mutate(period = as.factor(period)) |> 
  mutate(rate = ifelse(period == "period1", rate1, rate2))

richness_rate <- turnover_species |> 
  summarise(.by = c(summit, period), rate = sum(rate))

turnover_summit <- turnover_species |> 
  summarise(.by = c(summit, period), 
            new = sum(case_when(rate > 0 ~ rate), na.rm = TRUE), 
            nochange = sum(case_when(rate == 0 ~ rate), na.rm = TRUE), 
            lost = sum(case_when(rate < 0 ~ rate), na.rm = TRUE))



# # New species by nature type
# 
# filefjell_2024_clean <- tar_read(filefjell_2024_clean)
# 
# colonizers_data <- filefjell_2024_clean |> 
#   semi_join(
#     turnover_species_data |> 
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



# Elevation change - Only species present all years----

## Model

elerate_all |> 
  ggplot() +
  geom_histogram(aes(x = rate))

elerate_all_mod <- glmmTMB(
  rate ~ 
    period + (1 | summit) + (1 | species), 
  family = gaussian, 
  data = elerate_all)

elerate_all_mod |> model_diagnosis()
elerate_all_mod |> model_homoscedasticity()
elerate_all_mod |> summary()


# No distributions I try get closely to fitting. I try bayesian. THIS IS NOT FINISHED, I FOCUS ON NEW

elerate_all_gbayes <- brm(
  rate ~ 
    period + (1|summit) + (1|species),
  family = gaussian(), 
  data = elerate_all,
  seed = 811)
withr::with_seed(811, pp_check(elerate_all_gbayes, type = "dens_overlay")) # The shape does not really fit, though the range is similar

elerate_all_tbayes <- brm(
  rate ~ 
    period + (1|summit) + (1|species),
  family = student(), 
  data = elerate_all,
  seed = 811)
withr::with_seed(811, pp_check(elerate_all_tbayes, type = "dens_overlay")) # Some extreme values 

loo(elerate_all_gbayes, elerate_all_tbayes) # It seems student t is better, we have to fix the range
# I have also tried beta (using 32 as maximum possible change), but doesn't really fit

elerate_all_tbayesp <- brm(
  rate ~ 
    period + (1 | summit) + (1 | species), 
  family = student(), 
  prior = c(
    prior(normal(0, 0.5), class = "b"),
    prior(normal(0, 0.5), class = "Intercept"),
    prior(student_t(3, 0, 0.3), class = "sigma"),
    prior(gamma(55, 1), class = "nu")
    ),
  data = elerate_all,
  seed = 811
)
withr::with_seed(811, pp_check(elerate_all_tbayesp, type = "dens_overlay", size = 2))

loo(elerate_all_tbayes, elerate_all_tbayesp) # The one without priors seems slightly better, but the posterior distribution doesn't match, we keep use priors


## Diagnosis

# Model convergence and sample quality
elerate_all_tbayesp |> summary()   # Rhat = 1, no divergent transitions, large ESS (effective sample size)
elerate_all_tbayesp |> plot() # hairy caterpillars

# Posterior predictive check
withr::with_seed(811, pp_check(elerate_all_tbayesp, type = "dens_overlay"))

# Residuals
elerate_all_bayes_res <- tibble(
  fitted = fitted(elerate_all_tbayesp)[, "Estimate"],
  residuals = residuals(elerate_all_tbayesp)[, "Estimate"],
  period = elerate_all$period,
  summit = elerate_all$summit,
  species = elerate_all$species
)
ggplot(elerate_all_bayes_res, aes(x = fitted, y = residuals, colour = period)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    x = "Fitted values",
    y = "Residuals",
    title = "Residuals vs Fitted Values"
  ) +
  theme_minimal()

# It seems there is heteroskedasticity, with period 2 having greater variance
pp_check(elerate_all_tbayesp, type = "dens_overlay_grouped", group = "period") # Heteroskedasticity also displayed here



### Model 2

elerate_all_bayesh <- brm(
  bf(rate ~ 
    period + (1|summit) + (1|species),
    sigma ~period),
  family = student(), 
  prior = c(
    prior(normal(0, 0.5), class = "b"),
    prior(normal(0, 0.5), class = "Intercept"),
    prior(gamma(55, 1), class = "nu")
  ),
  data = elerate_all,
  seed = 811,
  control = list(adapt_delta = 0.99))


## Diagnosis

# Model convergence and sample quality
elerate_all_bayesh |> summary()   # Rhat = 1, no divergent transitions, large ESS (effective sample size)
elerate_all_bayesh |> plot() # hairy caterpillars

# Posterior predictive check
withr::with_seed(811, pp_check(elerate_all_bayesh, type = "dens_overlay")) # The range is slightly too wide. I continue, and see afterwards what to adjust

# Residuals
elerate_all_bayesh_res <- tibble(
  fitted = fitted(elerate_all_bayesh)[, "Estimate"],
  residuals = residuals(elerate_all_bayesh)[, "Estimate"],
  period = elerate_all$period,
  summit = elerate_all$summit,
  species = elerate_all$species
)
ggplot(elerate_all_bayesh_res, aes(x = fitted, y = residuals, colour = period)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    x = "Fitted values",
    y = "Residuals",
    title = "Residuals vs Fitted Values"
  ) +
  theme_minimal()
# Maybe more variance in period 2, but it looks quite good
pp_check(elerate_all_bayesh, type = "dens_overlay_grouped", group = "period") # The distributions fit relatively well (though both miss the bump on the right)

loo(elerate_all_tbayesp, elerate_all_bayesh) # Large difference

# Random effect structure
elerate_all_bayesh |> ranef()
elerate_all_bayesh |> VarCorr()
elerate_all_bayesh |> bayes_R2()

# Outliers
elerate_all_bayesh |> loo() # No influential points

# Summary
elerate_all_bayesh |> summary()

elerate_all_results <- elerate_all_bayesh |> 
  emmeans( ~ period) |> 
  tidy(conf.int = TRUE) |> 
  clean_names() |> 
  rename(conf_low = lower_hpd,
         conf_high = upper_hpd) |> 
  mutate(model = "elevation") |> 
  relocate(model)




# Elevation change Including new species----

### Model

elerate_new |> 
  ggplot() +
  geom_histogram(aes(x = rate))

elerate_new_mod <- glmmTMB(
  rate ~ 
    period + (1 | summit) + (1 | species), 
  family = t_family,
  data = elerate_new)

elerate_new_mod |> model_diagnosis()
elerate_new_mod |> model_homoscedasticity()
elerate_new_mod |> summary()

# No distributions I try get close to fitting. I try bayesian

elerate_new_bayes <- brm(
  rate ~ 
    period + (1|summit) + (1|species),
  family = student(), 
  data = elerate_new,
  seed = 811)


## Diagnosis

# Model convergence and sample quality
elerate_new_bayes |> summary()   # Rhat = 1, no divergent transitions, large ESS (effective sample size)
elerate_new_bayes |> plot() # hairy caterpillars

# Posterior predictive check
withr::with_seed(811, pp_check(elerate_new_bayes, type = "dens_overlay")) # Correct range, but misses the slight bump on the positive side

# Residuals
elerate_new_bayes_res <- tibble(
  fitted = fitted(elerate_new_bayes)[, "Estimate"],
  residuals = residuals(elerate_new_bayes)[, "Estimate"],
  period = elerate_new$period,
  summit = elerate_new$summit,
  species = elerate_new$species
)
ggplot(elerate_new_bayes_res, aes(x = fitted, y = residuals, colour = period)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    x = "Fitted values",
    y = "Residuals",
    title = "Residuals vs Fitted Values"
  ) +
  theme_minimal()
# It seems there is heteroskedasticity, with period 1 having greater variance
pp_check(elerate_new_bayes, type = "dens_overlay_grouped", group = "period") # Heteroskedasticity also displayed here



### Model 2

elerate_new_bayesh <- brm(
  bf(rate ~ 
    period + (1|summit) + (1|species),
    sigma ~ period),
  family = student(), 
  data = elerate_new,
  seed = 811)
loo(elerate_new_bayes, elerate_new_bayesh) # The new model is more complex, but better

## Diagnosis

# Model convergence and sample quality
elerate_new_bayesh |> summary()   # Rhat = 1, no divergent transitions, large ESS (effective sample size)
elerate_new_bayesh |> plot() # hairy caterpillars

# Posterior predictive check
withr::with_seed(811, pp_check(elerate_new_bayesh, type = "dens_overlay")) # Correct range, but misses the slight bump on the positive side

# Residuals
elerate_new_bayesh_res <- tibble(
  fitted = fitted(elerate_new_bayesh)[, "Estimate"],
  residuals = residuals(elerate_new_bayesh)[, "Estimate"],
  period = elerate_new$period,
  summit = elerate_new$summit,
  species = elerate_new$species
)
ggplot(elerate_new_bayesh_res, aes(x = fitted, y = residuals, colour = period)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    x = "Fitted values",
    y = "Residuals",
    title = "Residuals vs Fitted Values"
  ) +
  theme_minimal()
# It seems there is still heteroskedasticity, with period 1 having greater variance
withr::with_seed(811, pp_check(elerate_new_bayesh, type = "dens_overlay_grouped", group = "period")) # Still some heteroskedasticity, but the distributions fit better

# Random effect structure
elerate_new_bayesh |> ranef()
elerate_new_bayesh |> VarCorr()
elerate_new_bayesh |> bayes_R2()

# Outliers
elerate_new_bayesh |> loo() # No influential points

# Summary
elerate_new_bayesh |> summary()

elerate_new_results <- elerate_new_bayesh |> 
  emmeans( ~ period) |> 
  tidy(conf.int = TRUE) |> 
  clean_names() |> 
  rename(conf_low = lower_hpd,
         conf_high = upper_hpd) |> 
  mutate(model = "elevation") |> 
  relocate(model)


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
#   mutate(period = factor(period, level = c("period2", "period1")))
# test_elevation_two_mod <- glmmTMB(
#   elevation_change_rate ~ 
#     period + (1 | summit) + (1 | species), 
#   family = gaussian, 
#   data = test_elevation_two)
# test_elevation_two_mod |> summary()




# Richness change----

## Exploratoy graphs

richness_rate |>
  ggplot(aes(x = period, y = rate)) +
  geom_violin() +
  labs(title = " elevations by Year",
       x = "Year",
       y = "Vertical elevation to Top (meters)") +
  theme_minimal()

richness_rate |>
  ggplot(aes(x = summit:period, y = rate, color = summit)) +
  geom_point() +
  labs(title = "Plot of richness change per year",
       x = "Year",
       y = "Richness change (species/year)") +
  theme_minimal() +
  theme(legend.position = "none")
# Some summits gained species at a faster rate, some at a slower, and some lost species in the second period


## Modelling

richness_rate |> 
  ggplot() +
  geom_histogram(aes(x = rate))

richrate_mod <- glmmTMB(
  rate ~
    period,
  family = gaussian,
  data = richness_rate)

richrate_mod |> model_diagnosis() # No problems
richrate_mod |> model_homoscedasticity() # No problems
richrate_mod |> summary()

richrate_results <- richrate_mod |>
  emmeans( ~ period) |>
  tidy(conf.int = TRUE) |> 
  clean_names() |> 
  select(period, estimate, conf_low, conf_high) |>
  mutate(model = "richness") |> 
  relocate(model)




# Turnover----

turnover_summit |>
  summarise(.by = period,
            new_mean = mean(new),
            nochange_mean = mean(nochange),
            lost_mean = mean(lost))


# New species

turnover_summit |>
  ggplot(aes(x = period, y = new)) +
  geom_violin()

turnover_summit |> 
  ggplot() +
  geom_histogram(aes(x = new))

turnew_mod <- glmmTMB(
  new ~
    period,
  family = gaussian,
  data = turnover_summit)

turnew_mod |> model_diagnosis() # No problems
turnew_mod |> model_homoscedasticity() # No problems
turnew_mod |> summary()

turnew_results <- turnew_mod |>
  emmeans( ~ period) |>
  tidy(conf.int = TRUE) |> 
  clean_names() |> 
  select(period, estimate, conf_low, conf_high) |>
  mutate(model = "new") |> 
  relocate(model)
# More new species in the second period, but not significantly


# Lost species

turnover_summit |>
  ggplot(aes(x = period, y = lost)) +
  geom_violin()

turnover_summit |> 
  ggplot() +
  geom_histogram(aes(x = lost))

turlost_mod <- glmmTMB(
  lost ~
    period,
  family = gaussian,
  data = turnover_summit)

turlost_mod |> model_diagnosis()
turlost_mod |> model_homoscedasticity()
turlost_mod |> summary()

turlost_modh <- glmmTMB(
  lost ~
    period,
  dispformula = ~period,
  family = gaussian,
  data = turnover_summit)

turlost_modh |> model_diagnosis() # no problems
turlost_modh |> model_homoscedasticity() # No problems
turlost_modh |> summary()
# Greater species loss in the second period

turlost_results <- turlost_mod |>
  emmeans( ~ period) |>
  tidy(conf.int = TRUE) |> 
  clean_names() |> 
  select(period, estimate, conf_low, conf_high) |>
  mutate(model = "lost") |> 
  relocate(model)



# Turnover original species----

turnover_status <- elevation_species |> 
  mutate(presence1 = ifelse(!is.na(distance1), 1, 0), 
         presence2 = ifelse(!is.na(distance2), 1, 0), 
         presence3 = ifelse(!is.na(distance3), 1, 0),
         turnover1 = presence2 - presence1,
         turnover2 = presence3 - presence2,
         development = case_when(turnover1 == 1 & turnover2 == 1 ~ "Error",
                                 turnover1 == 1 & turnover2 == 0 ~ "Appeared1",
                                 turnover1 == 1 & turnover2 == -1 ~ "Forth_back",
                                 turnover1 == 0 & turnover2 == 1 ~ "Appeared2",
                                 turnover1 == 0 & turnover2 == 0 ~ "Remained",
                                 turnover1 == 0 & turnover2 == -1 ~ "Disappeared2",
                                 turnover1 == -1 & turnover2 == 1 ~ "Back_forth",
                                 turnover1 == -1 & turnover2 == 0 ~ "Disappeared1",
                                 turnover1 == -1 & turnover2 == -1 ~ "Error")) |> 
  summarise(.by = "development", total = n())

# 341 Remained
# 7 Disappeared1 | 14 Disappeared2
# 14 Back_forth | 57 Forth_back
# 225 Appeared1 | 118 Appeared2




# Results----

results_table <- elerate_new_results |>
  rbind(richrate_results) |>
  rbind(turnew_results) |>
  rbind(turlost_results) |>
  mutate(model = factor(model, levels = c("elevation", "richness", "new", "lost")))
# 
# facet_labels <- data.frame(model = unique(results_table$model), label = letters[1:4], x = -0.2, y = 1.5)
# 
results_graph <- results_table |>
  mutate(period = factor(period, levels = c("period2", "period1"))) |>
  ggplot(aes(x = estimate, y = period, colour = period)) +
  theme_minimal() +
  facet_grid(rows = vars(model), switch = "y", labeller = as_labeller(adj_label)) +
  geom_vline(xintercept = 0, colour = "black") +
  # geom_vline(xintercept = c(-0.2, 0.2, 0.4, 0.6, 0.8), colour = "lightgrey") +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf_low, xmax = conf_high), height = 0.3) +
  # scale_x_continuous(limits = c(-0.2, 0.92), breaks = c(0.0, 0.4, 0.8)) +
  scale_colour_manual("period", values = colour_mapping$period, labels = adj_label) +
  guides(colour = guide_legend(reverse = TRUE)) +
  labs(x = "Rate") +
  theme(panel.spacing.y = unit(2, "lines"),
        text = element_text(size = 16, family = "serif"),
        axis.title.x = element_text(hjust = 0.535),
        axis.text.x = element_text(margin = margin(t = 10, b = 10)),
        strip.text.y.left = ggtext::element_markdown(angle = 0, hjust = 0),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "top",
        legend.box.margin = margin(l = 85),
        legend.title = element_text(margin = margin(b = 5, r = 40)),
        legend.text = element_text(margin = margin(l = 9, r = 20, b = 4)))
results_graph
results_graph |> ggsave(file = "Results/Rate_of_change.png", width = 20, height = 15, units = "cm")

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