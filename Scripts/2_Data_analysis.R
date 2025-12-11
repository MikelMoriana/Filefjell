# Libraries----

library(vegan)

source("Scripts/0_setup.R")


# Data----

elevation_data_clean <- tar_read(elevation_data_clean)
visit_years <- elevation_data_clean |>
  select(year, summit) |>
  distinct() |>
  pivot_wider(names_from = year, names_prefix = "y", values_from = year) |>
  mutate(first = 1972,
         second = coalesce(y2008, y2009),
         third = coalesce(y2024, y2025)) |>
  select(summit, first, second, third)

elevation_wide <- elevation_data_clean |>
  select(!c(date:recorder, rareness)) |>
  pivot_wider(names_from = year, names_prefix = "y", values_from = distance) |>
  left_join(visit_years, by = "summit") |>
  mutate(third = ifelse(!is.na(y2025), 2025, third),
         distance1 = y1972,
         distance2 = coalesce(y2008, y2009),
         distance3 = coalesce(y2024, y2025)) |>
  select(!c(y1972, y2008, y2009, y2024, y2025)) |>
  mutate(period1 = second - first,
         period2 = third - second)


## Turnover----

turnover_species <- elevation_wide |>
  select(!first:third) |>
  mutate(presence1 = ifelse(is.na(distance1), 0, 1),
         presence2 = ifelse(is.na(distance2), 0, 1),
         presence3 = ifelse(is.na(distance3), 0, 1),
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
  select(!distance1:distance3) |> 
  relocate(development, .after = species) |>
  mutate(rate1 = turnover1 / period1,
         rate2 = turnover2 / period2) |>
  pivot_longer(cols = c(period1, period2), names_to = "period", values_to = "years") |>
  mutate(rate = ifelse(period == "period1", rate1, rate2)) |>
  relocate(period:rate, .after = development) |>
  mutate(period = as.factor(period))

turnover_grouped <- turnover_species |> 
  summarise(.by = c("specialization", "development"), total = n() / 2) |> # Divide by two since for each species we have two rows, one per period
  arrange(development, specialization)


# By summit

turnover_summit <- turnover_species |> 
  summarise(.by = c(summit, period, specialization), 
            new = sum(case_when(rate > 0 ~ rate), na.rm = TRUE), 
            nochange = sum(case_when(rate == 0 ~ rate), na.rm = TRUE), 
            lost = sum(case_when(rate < 0 ~ rate), na.rm = TRUE))

turnover_summit_tourist <- turnover_species |> 
  filter(development %in% c("Forth_back", "Back_forth")) |> 
  summarise(.by = c(summit, period), 
            new = sum(case_when(rate > 0 ~ rate), na.rm = TRUE), 
            nochange = sum(case_when(rate == 0 ~ rate), na.rm = TRUE), 
            lost = sum(case_when(rate < 0 ~ rate), na.rm = TRUE))

turnover_summit_nontourist <- turnover_species |> 
  filter(!(development %in% c("Forth_back", "Back_forth"))) |> 
  summarise(.by = c(summit, period), 
            new = sum(case_when(rate > 0 ~ rate), na.rm = TRUE), 
            nochange = sum(case_when(rate == 0 ~ rate), na.rm = TRUE), 
            lost = sum(case_when(rate < 0 ~ rate), na.rm = TRUE))


## Richness----

richness_rate <- turnover_species |>
  summarise(.by = c(summit, elevation, period, specialization), rate = sum(rate))


# New species by area

summit_data <- tar_read(summit_data_tidy)

turnover_area <- turnover_species |> 
  left_join(summit_data, by = c("summit", "elevation"))
turnover_area_grouped <- turnover_area |> 
  summarise(.by = c(elevation, area, bedrock, development), 
            total = n())


## Distance----

elevation_wide_new <- elevation_wide |> 
  mutate(adj_dist1 = ifelse(is.na(distance1) & !is.na(distance2), 33, distance1),
         adj_dist2 = ifelse(is.na(distance2) & !is.na(distance3), 33, distance2),
         adj_dist3 = ifelse(is.na(distance3) & is.na(distance1) & !is.na(distance2), 33, distance3))
# If a species appeared we assume it was right below the limit the previous survey. If a species disappeared, and it was new the previous survey, we assume it has gone back to right below the limit

# We have distance to the top as variable, which lets us compare mountains of different elevations. But since we are using change, we decide to use change in altitude instead of change in distance, since it is more intuitive: a positive value means the species grows higher up the summit (we subtract the value the first year from the value the second year)

# Considering only species for which we have data (present two sampling times in a row)
elerate_all <- elevation_wide |>
  mutate(change1 = distance1 - distance2, 
         change2 = distance2 - distance3) |> 
  pivot_longer(cols = c(period1, period2), names_to = "period", values_to = "years") |>
  mutate(period = as.factor(period)) |>
  mutate(change = case_when(period == "period1" ~ change1,
                            period == "period2" ~ change2)) |>
  filter(!is.na(change)) |> 
  mutate(rate = change / years)


# Considering only species found all years at a summit
elerate_remained <- elevation_wide |>
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
elerate_new <- elevation_wide_new |>
  mutate(change1 = adj_dist1 - adj_dist2, 
         change2 = adj_dist2 - distance3) |> 
  filter(!is.na(change1) & !is.na(change2)) |>
  pivot_longer(cols = c(period1, period2), names_to = "period", values_to = "years") |>
  mutate(period = as.factor(period)) |>
  mutate(change = case_when(period == "period1" ~ change1,
                            period == "period2" ~ change2)) |>
  mutate(rate = change / years) |>
  select(!c(change1, change2))


## Nature type----

filefjell_2024_2025_clean <- tar_read(filefjell_2024_2025_clean)

# nature_types

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




# Turnover overview----

# Overview tables

turnover_grouped |> 
  pivot_wider(names_from = specialization, values_from = total)
# development   alpine  generalist
# Remained         220         121
# Appeared1        114         111
# Appeared2         48          70
# Disappeared1       1           6
# Disappeared2      10           4
# Forth_back        21          36
# Back_forth        10           4

observations <- elevation_wide |> 
  select(!c(first:third, period1, period2)) |> 
  pivot_longer(cols = c(distance1:distance3), names_to = "year", values_to = "distance") |> 
  mutate(year = case_when(year == "distance1" ~ 1972,
                          year == "distance2" ~ 2009,
                          year == "distance3" ~ 2024)) |> 
  filter(!is.na(distance)) |> 
  summarise(.by = c(year, specialization), observations = n()) |> 
  arrange(year) |> 
  pivot_wider(names_from = year, values_from = observations) |> 
  mutate(specialization = ifelse(specialization == "alpine", "Alpine", "Generalist")) |> 
  rbind(c("extra", "extra1", "extra2", "extra3"), 
        c("Specialization", "1972", "2009", "2024")) |> 
  row_to_names(row_number = 3, remove_rows_above = FALSE) |> 
  mutate(extra = factor(extra, levels = c("Specialization", "Alpine", "Generalist"))) |> 
  arrange(extra)

turnover_development <- turnover_grouped |> 
  pivot_wider(names_from = specialization, values_from = total) |> 
  mutate(status1 = case_when(development %in% c("Remained", "Disappeared2") ~ "Remained",
                             development %in% c("Appeared1", "Forth_back") ~ "New",
                             development %in% c("Disappeared1", "Back_forth") ~ "Lost",
                             development == "Appeared2" ~ NA),
         status2 = case_when(development %in% c("Remained", "Appeared1") ~ "Remained",
                             development %in% c("Appeared2", "Back_forth") ~ "New",
                             development %in% c("Forth_back", "Disappeared2") ~ "Lost",
                             development == "Disappeared1" ~ NA)) |> 
  relocate(status1, status2) |> 
  rbind(c("extra", "extra1", "extras", "extra2", "extra3"), 
        c("Status 2009", "Status 2024", "development", "Alpine", "Generalist")) |> 
  row_to_names(row_number = 8, remove_rows_above = FALSE) |> 
  mutate(extras = factor(extras, levels = c("development", "Remained", "Appeared1", "Forth_back", "Disappeared1", "Back_forth", "Appeared2", "Disappeared2"))) |>
  arrange(extras) |> 
  select(!extras)

observations_turnover_ft <- observations |> 
  rbind(turnover_development) |> 
  mutate(across(where(is.factor), as.character)) |> 
  row_to_names(row_number = 1) |> 
  flextable() |> 
  bg(part = "header", bg = "black") |> 
  color(part = "header", color = "white") |> 
  bold(part = "header") |> 
  bg(part = "body", bg = "white") |> 
  color(part = "body", color = "black") |> 
  bg(part = "body", i = 3, bg = "black") |> 
  color(part = "body", i = 3, color = "white") |> 
  bold(part = "body", i = 3) |> 
  align(part = "all", j = 2:4, align = "center") |> 
  align(part = "body", i = 3:10, j = 2, align = "left") |> 
  flextable::font(part = "all", fontname = "Times New Roman") |> 
  autofit()

observations_turnover_ft |> save_as_image(path = "Results/Observations_turnover.png")


# Turnover by distance to top

turnover_status <- elevation_wide |> 
  select(!c(first:third, period1, period2)) |> 
  mutate(development = case_when(!is.na(distance1) & !is.na(distance2) & !is.na(distance3) ~ "Remained",
                                 !is.na(distance1) & !is.na(distance2) & is.na(distance3) ~ "Disappeared2",
                                 !is.na(distance1) & is.na(distance2) & !is.na(distance3) ~ "Back_forth",
                                 !is.na(distance1) & is.na(distance2) & is.na(distance3) ~ "Disappeared1",
                                 is.na(distance1) & !is.na(distance2) & !is.na(distance3) ~ "Appeared1",
                                 is.na(distance1) & !is.na(distance2) & is.na(distance3) ~ "Forth_back",
                                 is.na(distance1) & is.na(distance2) & !is.na(distance3) ~ "Appeared2")) |>
  pivot_longer(cols = c(distance1:distance3), names_to = "measurement", values_to = "distance") |> 
  filter(!is.na(distance)) |> 
  mutate(altitude = (-1)*distance + 32,
         development = factor(development, levels = c("Remained", "Disappeared2", "Back_forth", "Disappeared1", "Appeared1", "Forth_back", "Appeared2")))

turnover_status |> 
  ggplot() + 
  geom_histogram(aes(x = (altitude+1)/34))

turdev_mod <- glmmTMB(
  (altitude+1)/34 ~ 
    development,
  family = beta_family(),
  dispformula = ~development,
  data = turnover_status)
turdev_mod |> model_diagnosis()
turdev_mod |> model_homoscedasticity()
turdev_mod |> summary()

turdev_emmeans <- turdev_mod |> emmeans(~development) |> contrast(method = "pairwise")
turdev_emmeans
turdev_letters <- turnover_status |> 
  select(development) |> 
  mutate(levels = case_when(development == "Remained" ~ "A",
                            development == "Disappeared2" ~ "BC",
                            development == "Back_forth" ~ "BC",
                            development == "Disappeared1" ~ "BC",
                            development == "Appeared1" ~ "B",
                            development == "Forth_back" ~ "C",
                            development == "Appeared2" ~ "B"))

turnover_status |> 
  ggplot() +
  geom_boxplot(aes(x = development, y = altitude)) +
  geom_text(data = turdev_letters, aes(x = development, y = 33, label = levels))




# Turnover----

turnover_summit |>
  summarise(.by = c(period, specialization),
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
    period * specialization + (1 | summit),
  family = gaussian,
  data = turnover_summit)
# turnew_mod <- tar_read(turnew_mod) # To double-check targets

turnew_mod |> model_diagnosis() # No problems
turnew_mod |> model_homoscedasticity() # No problems
turnew_mod |> summary()
# Slightly greater rate in the second period, but not by much. No difference between specialization levels

turnew_results <- turnew_mod |>
  mod_summary()


# Lost species

turnover_summit |>
  ggplot(aes(x = period, y = lost)) +
  geom_violin()

turnover_summit |>
  ggplot() +
  geom_histogram(aes(x = lost))

turlost_mod <- glmmTMB(
  lost ~
    period * specialization + (1 | summit),
  family = gaussian,
  data = turnover_summit)

turlost_mod |> model_diagnosis()
turlost_mod |> model_homoscedasticity()
turlost_mod |> summary()

turlost_modh <- glmmTMB(
  lost ~
    period * specialization + (1 | summit),
  dispformula = ~period*specialization, # complex, but helps with uniformity and both heteroskedasticities
  family = gaussian,
  data = turnover_summit)
# turlost_modh <- tar_read(turlost_mod) # To double-check targets

turlost_modh |> model_diagnosis() # no problems
turlost_modh |> model_homoscedasticity() # No problems
turlost_modh |> summary()
# Greater species loss in the second period

turlost_results <- turlost_modh |>
  mod_summary()




# Richness----

## Exploratory graphs

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
    period * specialization + (1 | summit),
  family = gaussian,
  data = richness_rate)

richrate_mod |> model_diagnosis() # No problems
richrate_mod |> model_homoscedasticity() # period
richrate_mod |> summary()

richrate_modh <- glmmTMB(
  rate ~
    period * specialization + (1 | summit),
  dispformula = ~period,
  family = gaussian,
  data = richness_rate)
# richrate_modh <- tar_read(richrate_mod) # To double-check targets

richrate_modh |> model_diagnosis() # No problems
richrate_modh |> model_homoscedasticity() # No problems
richrate_modh |> summary()

richrate_results <- richrate_modh |> 
  mod_summary()




# Elevation change - Only species we have data for----

## Model

elerate_all |> 
  ggplot() +
  geom_histogram(aes(x = rate))

elerate_all_mod <- glmmTMB(
  rate ~ 
    period * specialization + (1 | summit) + (1 | species), 
  family = gaussian, 
  data = elerate_all)

elerate_all_mod |> model_diagnosis()
elerate_all_mod |> model_homoscedasticity()
elerate_all_mod |> summary()


# No distributions I try get closely to fitting. I try bayesian. THIS IS NOT FINISHED, I FOCUS ON NEW

elerate_all_gbayes <- brm(
  rate ~ 
    period * specialization + (1|summit) + (1|species),
  family = gaussian(), 
  data = elerate_all,
  seed = 811)
withr::with_seed(811, pp_check(elerate_all_gbayes, type = "dens_overlay")) # The shape does not really fit, though the range is similar

elerate_all_tbayes <- brm(
  rate ~ 
    period * specialization + (1|summit) + (1|species),
  family = student(), 
  data = elerate_all,
  seed = 811)
withr::with_seed(811, pp_check(elerate_all_tbayes, type = "dens_overlay")) # Some extreme values 

loo(elerate_all_gbayes, elerate_all_tbayes) # It seems student t is better, we have to fix the priors to our expectations
# I have also tried beta (using 32 as maximum possible change), but doesn't really fit (not good for a peak)

elerate_all_tbayesp <- brm(
  rate ~ 
    period * specialization + (1 | summit) + (1 | species), 
  family = student(), 
  prior = c(
    prior(normal(0, 0.5), class = "b"),
    prior(normal(0, 0.5), class = "Intercept"),
    prior(student_t(3, 0, 0.3), class = "sigma"),
    prior(gamma(80, 1), class = "nu")
    ),
  control = list(adapt_delta = 0.999),
  data = elerate_all,
  seed = 811
) # 65, 70, 75, 80, 85
withr::with_seed(811, pp_check(elerate_all_tbayesp, type = "dens_overlay", size = 2))

loo(elerate_all_tbayes, elerate_all_tbayesp) # The one without priors seems slightly better, but the posterior distribution doesn't match, we keep use priors


## Diagnosis

# Model convergence and sample quality
elerate_all_tbayesp |> summary()   # Rhat = 1, no divergent transitions, large ESS (effective sample size)
elerate_all_tbayesp |> plot() # hairy caterpillars

# Posterior predictive check
withr::with_seed(811, pp_check(elerate_all_tbayesp, type = "dens_overlay"))

# Residuals
elerate_bayes_res <- tibble(
  fitted = fitted(elerate_all_tbayesp)[, "Estimate"],
  residuals = residuals(elerate_all_tbayesp)[, "Estimate"],
  period = elerate_all$period,
  specialization = elerate_all$specialization,
  summit = elerate_all$summit,
  species = elerate_all$species
)
ggplot(elerate_bayes_res, aes(x = fitted, y = residuals, colour = period)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    x = "Fitted values",
    y = "Residuals",
    title = "Residuals vs Fitted Values"
  ) +
  theme_minimal()
ggplot(elerate_bayes_res, aes(x = fitted, y = residuals, colour = specialization)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    x = "Fitted values",
    y = "Residuals",
    title = "Residuals vs Fitted Values"
  ) +
  theme_minimal()

# It seems there might be some heteroskedasticity, with period 2 having greater variance
pp_check(elerate_all_tbayesp, type = "dens_overlay_grouped", group = "period") # Heteroskedasticity also displayed here
pp_check(elerate_all_tbayesp, type = "dens_overlay_grouped", group = "specialization") # Quite similar



### Model 2

# elerate_all_bayesh <- brm(
#   bf(rate ~ 
#     period * specialization + (1|summit) + (1|species),
#     sigma ~period),
#   family = student(), 
#   prior = c(
#     prior(normal(0, 0.5), class = "b"),
#     prior(normal(0, 0.5), class = "Intercept"),
#     prior(gamma(46, 1), class = "nu")
#   ),
#   data = elerate_all,
#   control = list(adapt_delta = 0.999),
#   seed = 811
#   )
elerate_all_bayesh <- tar_read(elerate_all_bayes)
withr::with_seed(811, pp_check(elerate_all_bayesh, type = "dens_overlay", size = 1))
loo(elerate_all_tbayesp, elerate_all_bayesh)

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
  specialization = elerate_all$specialization,
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
withr::with_seed(811, pp_check(elerate_all_bayesh, type = "dens_overlay_grouped", group = "period")) # The distributions fit relatively well (though both miss the bump on the right)

ggplot(elerate_all_bayesh_res, aes(x = fitted, y = residuals, colour = specialization)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    x = "Fitted values",
    y = "Residuals",
    title = "Residuals vs Fitted Values"
  ) +
  theme_minimal()
withr::with_seed(811, pp_check(elerate_all_bayesh, type = "dens_overlay_grouped", group = "specialization"))

# Random effect structure
elerate_all_bayesh |> ranef()
elerate_all_bayesh |> VarCorr()
elerate_all_bayesh |> bayes_R2()

# Outliers
elerate_all_bayesh |> loo() # No influential points

# Summary
elerate_all_bayesh |> summary()

elerate_all_results <- elerate_all_bayesh |>
  mod_summary()
  



## Elevation change. Only species present all years----

# elerate_rem_bayesh <- brm(
#   bf(rate ~ 
#        period * specialization + (1|summit) + (1|species),
#      sigma ~period),
#   family = student(), 
#   prior = c(
#     prior(normal(0, 0.5), class = "b"),
#     prior(normal(0, 0.5), class = "Intercept"),
#     prior(gamma(45, 1), class = "nu")
#   ),
#   data = elerate_remained,
#   control = list(adapt_delta = 0.999),
#   seed = 811
# )
elerate_rem_bayesh <- tar_read(elerate_rem_bayes)

## Diagnosis

# Model convergence and sample quality
elerate_rem_bayesh |> summary()   # Rhat = 1, no divergent transitions, large ESS (effective sample size)
elerate_rem_bayesh |> plot() # hairy caterpillars

# Posterior predictive check
withr::with_seed(811, pp_check(elerate_rem_bayesh, type = "dens_overlay", size = 2)) # The range is slightly too wide. I continue, and see afterwards what to adjust

# Residuals
elerate_rem_bayesh_res <- tibble(
  fitted = fitted(elerate_rem_bayesh)[, "Estimate"],
  residuals = residuals(elerate_rem_bayesh)[, "Estimate"],
  period = elerate_remained$period,
  specialization = elerate_remained$specialization,
  summit = elerate_remained$summit,
  species = elerate_remained$species
)
ggplot(elerate_rem_bayesh_res, aes(x = fitted, y = residuals, colour = period)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    x = "Fitted values",
    y = "Residuals",
    title = "Residuals vs Fitted Values"
  ) +
  theme_minimal()
# Maybe more variance in period 2, but it looks quite good
withr::with_seed(811, pp_check(elerate_rem_bayesh, type = "dens_overlay_grouped", group = "period"))

ggplot(elerate_rem_bayesh_res, aes(x = fitted, y = residuals, colour = specialization)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    x = "Fitted values",
    y = "Residuals",
    title = "Residuals vs Fitted Values"
  ) +
  theme_minimal()
# Maybe more variance in period 2, but it looks quite good
withr::with_seed(811, pp_check(elerate_rem_bayesh, type = "dens_overlay_grouped", group = "specialization"))

# Random effect structure
elerate_rem_bayesh |> ranef()
elerate_rem_bayesh |> VarCorr()
elerate_rem_bayesh |> bayes_R2()

# Outliers
elerate_rem_bayesh |> loo() # No influential points

# Summary
elerate_rem_bayesh |> summary()

elerate_rem_results <- elerate_rem_bayesh |> 
  mod_summary()




## Elevation change Including new species----

# elerate_new_bayesh <- brm(
#   bf(rate ~ 
#        period * specialization + (1|summit) + (1|species),
#      sigma ~period),
#   family = student(),
#   data = elerate_new,
#   control = list(adapt_delta = 0.999),
#   seed = 811
# )
elerate_new_bayesh <- tar_read(elerate_new_bayes)
withr::with_seed(811, pp_check(elerate_new_bayesh, type = "dens_overlay")) 


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
  specialization = elerate_new$specialization,
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
withr::with_seed(811, pp_check(elerate_new_bayesh, type = "dens_overlay_grouped", group = "period")) # Some heteroskedasticity, but doesn't look that bad


ggplot(elerate_new_bayesh_res, aes(x = fitted, y = residuals, colour = specialization)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    x = "Fitted values",
    y = "Residuals",
    title = "Residuals vs Fitted Values"
  ) +
  theme_minimal()
withr::with_seed(811, pp_check(elerate_new_bayesh, type = "dens_overlay_grouped", group = "specialization"))

# Random effect structure
elerate_new_bayesh |> ranef()
elerate_new_bayesh |> VarCorr()
elerate_new_bayesh |> bayes_R2()

# Outliers
elerate_new_bayesh |> loo() # No influential points

# Summary
elerate_new_bayesh |> summary()

elerate_new_results <- elerate_new_bayesh |> 
  mod_summary()




# Results----

emmeans_overview <- turnew_results$emmeans_df |> 
  mutate(Model = "new") |>
  rbind(turlost_results$emmeans_df |> 
          mutate(Model = "lost")) |> 
  rbind(richrate_results$emmeans_df |>
          mutate(Model = "richness")) |> 
  rbind(elerate_all_results$emmeans_df |> 
          mutate(df = NA_real_,
                 statistic = NA_real_,
                 Model = "elevation") |> 
          relocate(df, .after = Estimate) |> 
          relocate(statistic, .after = CI_upper)) |> 
  relocate(Model) |> 
  mutate(Model = factor(Model, levels = c("new", "lost", "richness", "elevation")))

contrasts_overview <- turnew_results$contrast_df |> 
  mutate(Model = "New species") |>
  rbind(turlost_results$contrast_df |> 
          mutate(Model = "Lost species")) |> 
  rbind(richrate_results$contrast_df |>
          mutate(Model = "Richness")) |> 
  rbind(elerate_all_results$contrast_df |> 
          mutate(df = NA_real_,
                 statistic = NA_real_,
                 Model = "Elevation") |> 
          relocate(df, .after = Estimate) |> 
          relocate(statistic, .after = CI_upper)) |> 
  relocate(Model) |> 
  mutate(Model = factor(Model, levels = c("New species", "Lost species", "Richness", "Elevation")))

contrasts_table <- contrasts_overview |>
  select(!c(df, statistic)) |> 
  mutate(Contrast = case_when(Contrast == "1A-2A" ~ "Alpine. Period 1 - Period 2",
                              Contrast == "1G-2G" ~ "Generalist. Period 1 - Period 2",
                              Contrast == "1A-1G" ~ "Period 1. Alpine - Generalist",
                              Contrast == "2A-2G" ~ "Period 2. Alpine - Generalist")) |> 
  flextable() |> 
  bg(part = "header", bg = "black") |> 
  color(part = "header", color = "white") |> 
  bold(part = "header") |>
  bg(part = "body", bg = "white") |> 
  color(part = "body", color = "black") |> 
  merge_v(j = "Model") |>
  hline(i = c(4, 8, 12)) |> 
  hline(i = c(2, 6, 10, 14), border = officer::fp_border(style = "dotted")) |>
  bold(i = ~ ((CI_lower * CI_upper) > 0), j = -1) |>
  align(part = "all", j = -c(1, 2), align = "center") |> 
  flextable::font(part = "all", fontname = "Times New Roman") |> 
  fontsize(size = 12) |> 
  autofit()
contrasts_table |> save_as_image(path = "Results/Rate_changes.png")


# One ggplot

emmeans_figure <- emmeans_overview |> gg_results() +
  facet_grid(rows = vars(Model), switch = "y", labeller = as_labeller(adj_label)) +
  scale_y_discrete(position = "right", labels = adj_label) +
  labs(x = "Rate of change") +
  theme(panel.spacing.y = unit(1, "lines"),
        text = element_text(size = 16),
        strip.text.y.left = element_markdown(angle = 0, hjust = 0),
        axis.title.y = element_blank())
emmeans_figure
emmeans_figure |> ggsave(file = "Results/Rate_of_change.png", width = 20, height = 15, units = "cm")


# Several ggplots

turnew_figure <- turnew_results$emmeans_df |> 
  gg_results() +
  scale_x_continuous(limits = c(-0.4, 0.5),
                     labels = NULL) +
  labs(x = NULL, y = adj_label["new"])

turlost_figure <- turlost_results$emmeans_df |> 
  gg_results() +
  scale_x_continuous(limits = c(-0.4, 0.5),
                     labels = NULL) +
  labs(x = NULL, y = adj_label["lost"])

richrate_figure <- richrate_results$emmeans_df |> 
  gg_results() +
  scale_x_continuous(limits = c(-0.4, 0.5)) +
  labs(x = "Rate of change (number of species / year)", y = adj_label["richness"])

elerate_all_figure <- elerate_all_results$emmeans_df |> 
  gg_results() +
  scale_x_continuous(limits = c(-0.06, 0.10)) +
  labs(x = "Rate of change (metres / year)", y = adj_label["elevation"])

results_figure_stack <- ggarrange(
  plotlist = list(turnew_figure, turlost_figure, richrate_figure, elerate_all_figure),
  ncol = 1,
  nrow = 4,
  align = "v",
  common.legend = TRUE,
  heights = c(1, 1, 1.68, 1.68)
)
results_figure_stack
results_figure_stack |> ggsave(file = "Results/Rate_of_change_stack.png", width = 20, height = 15, units = "cm")




# New_species ~ ...----

summit_area_new |> ggplot() + geom_point(aes(x = area, y = total)) + facet_wrap(~development)
summit_area_new |> ggplot() + geom_point(aes(x = elevation, y = total)) + facet_wrap(~development)

app1_area_mod <- glmmTMB(
  total ~ area,
  data = summit_area_new |> filter(development == "Appeared1")
)
app1_area_mod |> summary()

app2_area_mod <- glmmTMB(
  total ~ area,
  data = summit_area_new |> filter(development == "Appeared2")
)
app2_area_mod |> summary()

baf_area_mod <- glmmTMB(
  total ~ area,
  data = summit_area_new |> filter(development == "Back_forth")
)
baf_area_mod |> summary()

fab_area_mod <- glmmTMB(
  total ~ area,
  data = summit_area_new |> filter(development == "Forth_back")
)
fab_area_mod |> summary()


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