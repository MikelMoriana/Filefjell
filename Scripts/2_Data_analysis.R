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

elevation_species <- filefjell_data_clean |> 
  select(!c(date, recorder)) |> 
  pivot_wider(names_from = year, names_prefix = "y", values_from = distance) |> 
  left_join(filefjell_visit_dates_wide, by = "summit") |> 
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


# # Considering the two periods separately (i.e., all species found in both samplings of a period are considered)
# 
# elevation_change_two_data <- filefjell_data_clean |> 
#   pivot_wider(names_from = year, names_prefix = "y", values_from = distance) |> 
#   mutate(period1 = case_when(is.na(y1972) ~ NA, 
#                           is.na(y2009) ~ NA, 
#                           !is.na(y2009) & !is.na(y1972) ~ y2009 - y1972), 
#          period2 = case_when(is.na(y2009) ~ NA, 
#                           is.na(y2024) ~ NA, 
#                           !is.na(y2024) & !is.na(y2009) ~ y2024 - y2009)) |> 
#   select(-c(y1972, y2009, y2024)) |> 
#   pivot_longer(cols =c("period1", "period2"), names_to = "period", values_to = "distance_change") |> 
#   mutate(elevation_change = distance_change * -1) |> 
#   select(-distance_change) |> 
#   mutate(elevation_change_rate = case_when(period == "period1" ~ elevation_change / 37, 
#                                           period == "period2" ~ elevation_change / 15)) |> 
#   filter(!is.na(elevation_change)) |> 
#   mutate(period = as.factor(period)) |> 
#   arrange(summit, species, period)


## Richness

richness_change <- filefjell_data_clean |>
  summarise(.by = c(year, summit, elevation),
            richness = n()) |>
  pivot_wider(names_from = year, names_prefix = "y", values_from = richness) |>
  mutate(year1 = case_when(!is.na(y1972) ~ 1972),
         year2 = case_when(!is.na(y2008) ~ 2008,
                           !is.na(y2009) ~ 2009),
         year3 = case_when(!is.na(y2024) ~ 2024,
                           !is.na(y2025) ~ 2025)) |>
  mutate(richness1 = y1972,
         richness2 = coalesce(y2008, y2009),
         richness3 = coalesce(y2024, y2025)) |>
  mutate(period1 = year2 - year1,
         change1 = richness2 - richness1,
         period2 = year3 - year2,
         change2 = richness3 - richness2) |>
  select(-c(y1972, y2008, y2009, y2024, y2025)) |>
  pivot_longer(cols = c(period1, period2), names_to = "period", values_to = "years") |>
  mutate(period = as.factor(period)) |>
  mutate(change = case_when(period == "period1" ~ change1,
                            period == "period2" ~ change2)) |>
  mutate(rate = change / years) |>
  select(!c(change1, change2))


# Turnover
# I HAVE TO FIGURE OUT HOW I DO TO KEEP THE YEAR, SO THAT I CAN CALCULATE THE RATE
# mAYBE CREATE AN OBJECT WITH SPECIES AND YEAR FOUND? AND JOIN IT AFTERWARDS
turnover_species <- filefjell_data_clean |>
  select(!c(date, recorder)) |>
  mutate(presence = ifelse(!is.na(distance), 1, 0)) |>
  select(-distance) |>
  pivot_wider(names_from = "year", names_prefix = "y", values_from = "presence", values_fill = 0) |>
  arrange(summit, species) |>
  mutate(year1 = case_when(!is.na(y1972) ~ 1972),
         year2 = case_when(!is.na(y2008) ~ 2008,
                           !is.na(y2009) ~ 2009),
         year3 = case_when(!is.na(y2024) ~ 2024,
                           !is.na(y2025) ~ 2025)) |>
  mutate(presence1 = y1972,
         presence2 = y2008 + y2009,
         presence3 = y2024 + y2025) |>
  select(!c(y1972, y2008, y2009, y2024, y2025)) |>
  mutate(period1 = year2 - year1,
         change1 = presence2 - presence1,
         period2 = year3 - year2,
         change2 = presence3 - presence2) |>
  pivot_longer(cols = c(period1, period2), names_to = "period", values_to = "years") |>
  mutate(period = as.factor(period)) |>
  mutate(change = case_when(period == "period1" ~ change1,
                            period == "period2" ~ change2)) |>
  mutate(rate = change / years) |>
  select(!c(change1, change2))

turnover_summit <- turnover_species |>
  summarise(.by = c(summit, period),
            lost = sum(rate < 0),
            nochange = sum(rate == 0),
            new = sum(rate > 1)) |>
  pivot_longer(cols = starts_with("per"),
               names_to = c("period", ".value"),
               names_sep = "_")


turnover_data <- species_turnover_data |>
  
  mutate(number_years = ifelse(period == "period1", 37, 15)) |>
  mutate(lost_rate = lost / number_years,
         new_rate = new / number_years) |>
  mutate(period = as.factor(period))

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



# Elevation change NOT FINISHED----

## Only species found all three years TO FINISH

# Model

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

elerate_all_bayes <- brm(
  rate ~ 
    period + (1|summit) + (1|species),
  family = student(), 
  prior(gamma(55, 0.1), class = "nu"),
  data = elerate_all,
  seed = 811)

# Diagnosis

elerate_all_bayes |> summary() # Converged
elerate_all_bayes |> plot() # "hairy caterpillars"

withr::with_seed(811, pp_check(elerate_all_bayes, type = "dens_overlay"))
withr::with_seed(811, pp_check(elerate_all_bayes, type = "hist"))
withr::with_seed(811, pp_check(elerate_all_bayes, type = "scatter_avg"))
withr::with_seed(811, pp_check(elerate_all_bayes, type = "stat"))

elerate_all_df <- data.frame(fitted = fitted(elerate_all_bayes)[,1],
                            resid = residuals(elerate_all_bayes)[,1],
                            predictor = elerate_all_bayes$data$period)
plot(elerate_all_df$fitted, elerate_all_df$resid, 
     xlab = "Fitted values", ylab = "Residuals")
ggplot(elerate_all_df, aes(x = predictor, y = resid)) +
  geom_point() +
  geom_smooth(method = "loess") +
  theme_minimal()
    
bayes_R2(elerate_all_bayes)
loo(elerate_all_bayes)

elerate_all_results <- elerate_all_mod |> 
  ggeffects::ggpredict(terms = "period") |> 
  rename(period = x) |> 
  as.data.frame() |> 
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
  fixef() |> 
  as_tibble(rownames = "term") |> 
  clean_names() |> 
  filter(!grepl("sigma", term)) |> 
  select(term, estimate, est_error, q2_5, q97_5) |> 
  rename(std_error = est_error) |> 
  rename(conf_low = q2_5) |> 
  rename(conf_high = q97_5) |> 
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

richness_change |>
  ggplot(aes(x = period, y = rate)) +
  geom_violin() +
  labs(title = " elevations by Year",
       x = "Year",
       y = "Vertical elevation to Top (meters)") +
  theme_minimal()

richness_change |>
  ggplot(aes(x = summit:period, y = rate, color = summit)) +
  geom_point() +
  labs(title = "Plot of richness change per year",
       x = "Year",
       y = "Richness change (species/year)") +
  theme_minimal() +
  theme(legend.position = "none")
# Some summits gained species at a faster rate, some at a slower, and some lost species in the second period


## Modelling

richness_change |> 
  ggplot() +
  geom_histogram(aes(x = rate))

riccha_rate_mod <- glmmTMB(
  rate ~
    period + (1 | summit),
  family = gaussian,
  data = richness_change)

riccha_rate_mod |> model_diagnosis() # No problems
riccha_rate_mod |> model_homoscedasticity() # No problems
riccha_rate_mod |> summary()

riccha_rate_results <- riccha_rate_mod |>
  tidy(effects = "fixed", conf.int = TRUE) |> 
  clean_names() |> 
  mutate(term = ifelse(term == "(Intercept)", "Intercept", term)) |> 
  select(term, estimate, std_error, conf_low, conf_high) |> 
  mutate(model = "richness") |> 
  relocate(model)
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
#   scale_fill_manual("period", values = c("period1" = "#859395", "period2" = "#f58800"), labels = c("1972-2009", "2009-2024")) + 
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
#   mutate(period = factor(period, levels = c("period2", "period1"))) |> 
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