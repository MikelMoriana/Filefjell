# Libraries----

source("Scripts/0_setup.R")


# Data----

filefjell_data_clean <- tar_read(filefjell_data_clean)

summit_periods <- filefjell_data_clean |>
  select(year, summit) |>
  mutate(year = ifelse(year == 2025 & summit == "Storeknippa", 2024, year)) |> # For simplicity's sake, we assume the five species recorded in Storeknippa in 2025 were also there in 2024
  distinct() |>
  pivot_wider(names_from = year, names_prefix = "y", values_from = year) |>
  mutate(first = y1972,
         second = coalesce(y2008, y2009),
         third = coalesce(y2024, y2025)) |>
  mutate(period1 = second - first,
         period2 = third - second) |>
  select(summit, period1, period2) |>
  pivot_longer(cols = c(period1, period2), names_to = "period", values_to = "time")

filefjell_simplified <- filefjell_data_clean |>
  select(!c(date:recorder, rareness)) |>
  mutate(year = case_when(year == 1972 ~ "first",
                          year %in% c(2008, 2009) ~ "second",
                          year %in% c(2024, 2025) ~ "third"))



## Richness----

richness <- filefjell_simplified |>
  summarise(.by = c(year, summit, specialisation), richness = n())

richness_rate <- richness |>
  pivot_wider(names_from = year, values_from = richness, values_fill = 0) |>
  mutate(period1 = second - first,
         period2 = third - second) |>
  select(!c(first:third)) |>
  pivot_longer(cols = c(period1, period2), names_to = "period", values_to = "change") |>
  left_join(summit_periods, by = c("summit", "period")) |>
  mutate(rate = change / time)


# Only top 10 metres

richness10 <- filefjell_simplified |>
  filter(distance <= 10) |>
  summarise(.by = c(year, summit, specialisation), richness = n())

richness10_rate <- richness10 |>
  pivot_wider(names_from = year, values_from = richness, values_fill = 0) |>
  mutate(period1 = second - first,
         period2 = third - second) |>
  select(!c(first:third)) |>
  pivot_longer(cols = c(period1, period2), names_to = "period", values_to = "change") |>
  left_join(summit_periods, by = c("summit", "period")) |>
  mutate(rate = change / time)


## Turnover----

turnover <- filefjell_simplified |>
  pivot_wider(names_from = year, values_from = distance) |>
  mutate(new1 = ifelse(is.na(first) & !is.na(second), 1, 0),
         new2 = ifelse(is.na(second) & !is.na(third), 1, 0),
         lost1 = ifelse(!is.na(first) & is.na(second), 1, 0),
         lost2 = ifelse(!is.na(second) & is.na(third), 1, 0)) |>
  select(!c(first:third)) |>
  summarise(.by = c(summit, specialisation),
            newperiod1 = sum(new1),
            newperiod2 = sum(new2),
            lostperiod1 = sum(lost1),
            lostperiod2 = sum(lost2)) |>
  pivot_longer(cols = c(newperiod1, newperiod2, lostperiod1, lostperiod2),
               names_to = c("type", "period"),
               names_pattern = "^(new|lost)(period\\d+)$",
               values_to = "value")

new_rate <- turnover |>
  filter(type == "new") |>
  left_join(summit_periods, by = c("summit", "period")) |>
  mutate(rate = value / time)

lost_rate <- turnover |>
  filter(type == "lost") |>
  left_join(summit_periods, by = c("summit", "period")) |>
  mutate(rate = value / time)


## 10 metres

turnover10 <- filefjell_simplified |>
  filter(distance <= 10) |>
  pivot_wider(names_from = year, values_from = distance) |>
  mutate(new1 = ifelse(is.na(first) & !is.na(second), 1, 0),
         new2 = ifelse(is.na(second) & !is.na(third), 1, 0),
         lost1 = ifelse(!is.na(first) & is.na(second), 1, 0),
         lost2 = ifelse(!is.na(second) & is.na(third), 1, 0)) |>
  select(!c(first:third)) |>
  summarise(.by = c(summit, specialisation),
            newperiod1 = sum(new1),
            newperiod2 = sum(new2),
            lostperiod1 = sum(lost1),
            lostperiod2 = sum(lost2)) |>
  pivot_longer(cols = c(newperiod1, newperiod2, lostperiod1, lostperiod2),
               names_to = c("type", "period"),
               names_pattern = "^(new|lost)(period\\d+)$",
               values_to = "value")

new10_rate <- turnover10 |>
  filter(type == "new") |>
  left_join(summit_periods, by = c("summit", "period")) |>
  mutate(rate = value / time)

lost10_rate <- turnover10 |>
  filter(type == "lost") |>
  left_join(summit_periods, by = c("summit", "period")) |>
  mutate(rate = value / time)



## New species by area----

summit_data <- tar_read(summit_data_tidy)

turnover_area <- turnover_species |> 
  left_join(summit_data, by = c("summit", "elevation"))
turnover_area_grouped <- turnover_area |> 
  summarise(.by = c(elevation, summit_decare, bedrock, development), 
            total = n())



## Altitude change----

# Considering only species for which we have data (present two sampling times in a row)

altitude_rate <- filefjell_simplified |> 
  pivot_wider(names_from = year, values_from = distance) |> 
  mutate(period1 = (second - first) * (-1),
         period2 = (third - second) * (-1)) |> # Change the sign so that a positive value indicates upwards movement
  select(!c(first:third)) |> 
  pivot_longer(cols = c(period1, period2), names_to = "period", values_to = "change") |> 
  filter(!is.na(change)) |> 
  left_join(summit_periods, by = c("summit", "period")) |> 
  mutate(rate = change / time)

altitude10_rate <- filefjell_simplified |> 
  filter(distance <= 10) |> 
  pivot_wider(names_from = year, values_from = distance) |> 
  mutate(period1 = (second - first) * (-1),
         period2 = (third - second) * (-1)) |> # Change the sign so that a positive value indicates upwards movement
  select(!c(first:third)) |> 
  pivot_longer(cols = c(period1, period2), names_to = "period", values_to = "change") |> 
  filter(!is.na(change)) |> 
  left_join(summit_periods, by = c("summit", "period")) |> 
  mutate(rate = change / time)


## Nature type----

type_species <- tar_read(type_species_clean)

all_species_types <- type_species |> 
  mutate(habitat_decare = ifelse(is.na(habitat_decare), 0.25, habitat_decare)) |> 
  mutate(main_type = ifelse(grepl("V", main_type), "V", main_type)) |> 
  summarise(.by = c(summit, main_type, specialisation, habitat_decare), total = n()) |> 
  pivot_wider(names_from = specialisation, values_from = total, values_fill = 0) |> 
  pivot_longer(cols = c("alpine", "generalist"), names_to = "specialisation", values_to = "total") |> 
  mutate(specialisation = as.factor(specialisation))

new_species_2024_2025 <- turnover_species |> 
  filter(period == "period2", turnover2 == 1) |> 
  select(summit:species)

new_species_types <- type_species |> 
  right_join(new_species_2024_2025, by = c("summit", "elevation", "specialisation", "species")) |> 
  mutate(habitat_decare = ifelse(is.na(habitat_decare), 0.25, habitat_decare)) |> 
  summarise(.by = c(summit, elevation, summit_decare, specialisation, main_type, habitat_decare), total = n()) |> 
  mutate(main_type = factor(main_type, levels = c("T1", "T27", "T14", "T3", "T22", "T7")))


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




# Rate Analyses----
## Turnover overview----

### Overview tables

# General

status_year <- turnover_species |> 
  filter(period == "period1") |> 
  select(!c(elevation, specialisation, development:rate, turnover1:rate2)) |> 
  mutate(first = case_when(presence1 == 1 ~ "1972Present",
                           presence1 == 0 ~ "1972Absent")) |> 
  mutate(second = case_when(presence1 == 1 & presence2 == 1 ~ "2009Remained",
                            presence1 == 1 & presence2 == 0 ~ "2009Lost",
                            presence1 == 0 & presence2 == 1 ~ "2009New",
                            presence1 == 0 & presence2 == 0 ~ "2009Absent")) |> 
  mutate(third = case_when(presence1 == 1 & presence2 == 1 & presence3 == 1 ~ "2024Remained",
                           presence1 == 1 & presence2 == 1 & presence3 == 0 ~ "2024Lost",
                           presence1 == 1 & presence2 == 0 & presence3 == 1 ~ "2024Reappeared",
                           presence1 == 1 & presence2 == 0 & presence3 == 0 ~ "2024Stayed_lost",
                           presence1 == 0 & presence2 == 1 & presence3 == 1 ~ "2024Stayed",
                           presence1 == 0 & presence2 == 1 & presence3 == 0 ~ "2024Disappeared",
                           presence1 == 0 & presence2 == 0 & presence3 == 1 ~ "2024New")) |> 
  select(first:third) |> 
  pivot_longer(cols = first:third, names_to = "survey", values_to = "status") |> 
  summarise(.by = "status", total = n()) |> 
  arrange(status)

header_map <- tibble(
  col_keys = c("1972status", "1972total", "2008/09status", "2008/09total", "2024/25status", "2024/25total"),
  level1   = c("1972", "1972", "2008/09", "2008/09", "2024/25", "2024/25")
)

status_year_ft <- tibble(
  "1972status" = c("Present", 
                rep("", 6),
                "Total present"),
  "1972total" = c(status_year |> filter(status == "1972Present") |> pull(total),
                rep("", 6),
                status_year |> filter(status == "1972Present") |> pull(total)),
  "2008/09status" = c("Remained", "", "Lost", "", "New", "", "", "Total present"),
  "2008/09total" = c(status_year |> filter(status == "2009Remained") |> pull(total), "",
                status_year |> filter(status == "2009Lost") |> pull(total), "",
                status_year |> filter(status == "2009New") |> pull(total), "", "",
                (status_year |> filter(status == "2009Remained") |> pull(total)) + (status_year |> filter(status == "2009New") |> pull(total))),
  "2024/25status" = c("Remained", "Lost", "Reappeared", "Did not reappear", "Remained", "Lost", "New", "Total present"),
  "2024/25total" = c(status_year |> filter(status == "2024Remained") |> pull(total),
                status_year |> filter(status == "2024Lost") |> pull(total),
                status_year |> filter(status == "2024Reappeared") |> pull(total),
                status_year |> filter(status == "2024Stayed_lost") |> pull(total),
                status_year |> filter(status == "2024Stayed") |> pull(total),
                status_year |> filter(status == "2024Disappeared") |> pull(total),
                status_year |> filter(status == "2024New") |> pull(total),
                (status_year |> filter(status == "2024Remained") |> pull(total)) + (status_year |> filter(status == "2024Reappeared") |> pull(total)) + (status_year |> filter(status == "2024Stayed") |> pull(total)) + (status_year |> filter(status == "2024New") |> pull(total)))
) |> 
  flextable() |>
  set_header_df(mapping = header_map, key = "col_keys") |> 
  merge_h(part = "header") |> 
  bg(part = "header", bg = "black") |> 
  color(part = "header", color = "white") |> 
  bold(part = "header") |> 
  align(part = "header", align = "center") |> 
  bg(part = "body", bg = "white") |> 
  color(part = "body", color = "black") |> 
  bg(part = "body", i = 8, bg = "grey") |> 
  hline(i = 4) |>
  hline(i = 2, j = 3:6) |> 
  hline(i = 6, j = 3:6) |> 
  hline(i = 7) |> 
  border(i = c(1:6, 8), j = 3, border.left = officer::fp_border(color = "black")) |> 
  border(i = 1:8, j = 5, border.left = officer::fp_border(color = "black")) |>
  border(part = "header", border.left = officer::fp_border(color = "white")) |> 
  vline_left() |> 
  vline_right() |> 
  align(part = "body", align = "left") |> 
  align(part = "body", j = c(2, 4, 6), align = "center") |> 
  flextable::font(part = "all", fontname = "Times New Roman") |> 
  autofit()
status_year_ft

status_year_ft |> save_as_image(path = "Results/Status_year.png")


# By specialisation

turnover_grouped |> 
  pivot_wider(names_from = specialisation, values_from = total)
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
  summarise(.by = c(year, specialisation), observations = n()) |> 
  arrange(year) |> 
  pivot_wider(names_from = year, values_from = observations) |> 
  mutate(specialisation = ifelse(specialisation == "alpine", "Alpine", "Generalist")) |> 
  rbind(c("extra", "extra1", "extra2", "extra3"), 
        c("specialisation", "1972", "2009", "2024")) |> 
  row_to_names(row_number = 3, remove_rows_above = FALSE) |> 
  mutate(extra = factor(extra, levels = c("specialisation", "Alpine", "Generalist"))) |> 
  arrange(extra)

turnover_development <- turnover_grouped |> 
  pivot_wider(names_from = specialisation, values_from = total) |> 
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




## Richness----

## Whole summit

richness_rate |>
  mutate(per_spe = paste0(period, specialisation)) |>
  ggplot(aes(x = per_spe, y = rate)) +
  geom_violin() +
  labs(title = " elevations by Year",
       x = "Year",
       y = "Vertical elevation to Top (meters)") +
  theme_minimal()

richness_rate |>
  ggplot() +
  geom_histogram(aes(x = rate))

richrate_mod <- glmmTMB(
  rate ~
    period * specialisation + (1 | summit),
  family = gaussian,
  data = richness_rate)

richrate_mod |> model_diagnosis() # No problems
richrate_mod |> model_homoscedasticity() # period
richrate_mod |> summary()

richrate_modh <- glmmTMB(
  rate ~
    period * specialisation + (1 | summit),
  dispformula = ~period,
  family = gaussian,
  data = richness_rate)

richrate_modh |> model_diagnosis() # No problems
richrate_modh |> model_homoscedasticity() # No problems
richrate_modh |> summary()

richrate_results <- richrate_modh |>
  mod_summary()
richrate_results


## 10 metres

richrate10_mod <- glmmTMB(
  rate ~
    period * specialisation + (1 | summit),
  family = gaussian,
  data = richness10_rate)

richrate10_mod |> model_diagnosis() # No problems
richrate10_mod |> model_homoscedasticity() # period
richrate10_mod |> summary()

richrate10_modh <- glmmTMB(
  rate ~
    period * specialisation + (1 | summit),
  dispformula = ~period,
  family = gaussian,
  data = richness10_rate)

richrate10_modh |> model_diagnosis() # No problems
richrate10_modh |> model_homoscedasticity() # No problems
richrate10_modh |> summary()

richrate10_results <- richrate10_mod |>
  mod_summary()
richrate10_results


# ## Functional
#
# richfun_rate |>
#   mutate(per_spe = paste0(period, functional)) |>
#   ggplot(aes(x = per_spe, y = rate)) +
#   geom_violin() +
#   labs(title = " elevations by Year",
#        x = "Year",
#        y = "Vertical elevation to Top (meters)") +
#   theme_minimal()
#
# richfun_rate |>
#   ggplot() +
#   geom_histogram(aes(x = rate))
#
# richfun_rate_mod <- glmmTMB(
#   rate ~
#     period * functional + (1 | summit),
#   family = gaussian,
#   data = richfun_rate)
#
# richfun_rate_mod |> model_diagnosis() # Uniformity, outliers and quantiles
# richfun_rate_mod |> model_homoscedasticity() # period and functional
# richfun_rate_mod |> summary()
#
# richfun_rate_modh <- glmmTMB(
#   rate ~
#     period * functional + (1 | summit),
#   dispformula = ~period+functional,
#   family = gaussian,
#   data = richfun_rate)
#
# richfun_rate_modh |> model_diagnosis() # No problems
# richfun_rate_modh |> model_homoscedasticity() # No problems
# richfun_rate_modh |> summary()
#
# richfun_rate_results <- richfun_rate_modh |>
#   modfun_summary()
# richfun_rate_results



## New species----

new_rate |>
  ggplot(aes(x = period, y = rate)) +
  geom_violin()

new_rate |>
  ggplot() +
  geom_histogram(aes(x = rate))

new_mod <- glmmTMB(
  rate ~
    period * specialisation + (1 | summit),
  family = gaussian,
  data = new_rate)

new_mod |> model_diagnosis() # No problems
new_mod |> model_homoscedasticity() # No problems
new_mod |> summary()
# Slightly greater rate in the second period, but not by much. No difference between specialisation levels

new_results <- new_mod |>
  mod_summary()
new_results


## Top 10 metres

new10_rate |>
  ggplot(aes(x = period, y = rate)) +
  geom_violin()

new10_rate |>
  ggplot() +
  geom_histogram(aes(x = rate))

new10_mod <- glmmTMB(
  rate ~
    period * specialisation + (1 | summit),
  family = gaussian,
  data = new10_rate)

new10_mod |> model_diagnosis() # Quantiles
new10_mod |> model_homoscedasticity() # period
new10_mod |> summary()


new10_modh <- glmmTMB(
  rate ~
    period * specialisation + (1 | summit),
  dispformula = ~period,
  family = gaussian,
  data = new10_rate)

new10_modh |> model_diagnosis() # No problems
new10_modh |> model_homoscedasticity() # No problems
new10_modh |> summary()

# Slightly greater rate in the second period, but not by much. No difference between specialisation levels

new10_results <- new10_modh |>
  mod_summary()
new10_results




## Lost species----

lost_rate |>
  ggplot(aes(x = period, y = rate)) +
  geom_violin()

lost_rate |>
  ggplot() +
  geom_histogram(aes(x = rate))

lost_mod <- glmmTMB(
  rate ~
    period * specialisation + (1 | summit),
  family = gaussian,
  data = lost_rate)

lost_mod |> model_diagnosis() # Uniformity and quantiles
lost_mod |> model_homoscedasticity() # period
lost_mod |> summary()

lost_modh <- glmmTMB(
  rate ~
    period * specialisation + (1 | summit),
  dispformula = ~period,
  family = gaussian,
  data = lost_rate)

lost_modh |> model_diagnosis() # No problems
lost_modh |> model_homoscedasticity() # No problems
lost_modh |> summary()
# Greater species loss in the second period

lost_results <- lost_modh |>
  mod_summary()
lost_results


## Top 10 metres

lost10_rate |>
  ggplot(aes(x = period, y = rate)) +
  geom_violin()

lost10_rate |>
  ggplot() +
  geom_histogram(aes(x = rate))

lost10_mod <- glmmTMB(
  rate ~
    period * specialisation + (1 | summit),
  family = gaussian,
  data = lost10_rate)

lost10_mod |> model_diagnosis() # Quantiles
lost10_mod |> model_homoscedasticity() # period
lost10_mod |> summary()

lost10_modh <- glmmTMB(
  rate ~
    period * specialisation + (1 | summit),
  dispformula = ~period,
  family = gaussian,
  data = lost10_rate)

lost_modh |> model_diagnosis() # No problems
lost_modh |> model_homoscedasticity() # No problems
lost_modh |> summary()
# Greater species loss in the second period

lost10_results <- lost10_modh |>
  mod_summary()
lost10_results




## Altitude change----

#### Frequentist analysis

altitude_rate |> 
  ggplot() +
  geom_histogram(aes(x = rate))

altrate_mod <- glmmTMB(
  rate ~ 
    period * specialisation + (1 | summit) + (1 | species), 
  family = gaussian, 
  data = altitude_rate)

altrate_mod |> model_diagnosis()
altrate_mod |> model_homoscedasticity()
altrate_mod |> summary()



#### No distributions I try get closely to fitting. I try bayesian

### 1. Choosing weakly informative priors
# Response scale. The rate of elevation change will not go beyond -2.133 to 2.133 (32 metres in 15 years, highest possible change in shortest time span between surveys)
# Fixed effects

priors_g1 <- c(
  prior(normal(0, 0.5), class = "Intercept"), # small positive mean for response variable
  prior(normal(0, 0.5), class = "b"),            # fixed effects
  prior(exponential(3), class = "sigma"),        # residual SD
  prior(exponential(3), class = "sd")           # all RE SDs (summit, species)
)

## Gaussian

altrate_gbay1 <- brm(
  bf(rate ~
       period * specialisation + (1|summit) + (1|species)),
  family = gaussian(),
  prior = priors_g1,
  sample_prior = "only",
  data = altitude_rate,
  chains = 4, iter = 2000, seed = 811
)

altrate_expla1 <- crossing(
  period = unique(altitude_rate$period),
  specialisation = unique(altitude_rate$specialisation),
  summit = NA,
  species = NA
)

altrate_gpred1 <- altrate_gbay1 %>%
  add_predicted_draws(newdata = altrate_expla1, re_formula = NA) %>%
  mutate(in_range = between(.prediction, -2, 2))

altrate_gsumm1 <- altrate_gpred1 %>%
  group_by(period, specialisation) %>%
  summarise(
    p_in_range = mean(in_range),
    q05 = quantile(.prediction, 0.05),
    q50 = quantile(.prediction, 0.50),
    q95 = quantile(.prediction, 0.95),
    .groups = "drop"
  )

altrate_gsumm1
# Between 97 and 99% of prior predictions fall within [-2, 2]. Good priors


## Student t

priors_t1 <- c(
  priors_g1,
  prior(gamma(2, 0.4), class = "nu")  # mean ~5, moderate heavy tails
)

altrate_tmod1 <- brm(
  bf(rate ~
       period * specialisation + (1|summit) + (1|species)),
  family = student(),
  prior = priors_t1,
  sample_prior = "only",
  data = altitude_rate,
  chains = 4, iter = 2000,  seed = 811
)

altrate_tpred1 <- altrate_tmod1 %>%
  add_predicted_draws(newdata = altrate_expla1, re_formula = NA) %>%
  mutate(in_range = between(.prediction, -2, 2))

altrate_tsumm1 <- altrate_tpred1 %>%
  group_by(period, specialisation) %>%
  summarise(
    p_in_range = mean(in_range),
    q05 = quantile(.prediction, 0.05),
    q50 = quantile(.prediction, 0.50),
    q95 = quantile(.prediction, 0.95),
    .groups = "drop"
  )

altrate_tsumm1


### 2. Comparing gaussian and student t

altrate_gmod2 <- brm(
  bf(rate ~ 
       period * specialisation + (1|summit) + (1|species)),
  family = gaussian(),
  prior = priors_g1,
  data = altitude_rate,
  chains = 4, iter = 4000, seed = 811,
  control = list(adapt_delta = 0.95)
)

# Student-t, sigma constant
altrate_tmod2 <- brm(
  bf(rate ~ 
       period * specialisation + (1|summit) + (1|species)),
  family = student(),
  prior = priors_t1,
  data = elerate_all,
  chains = 4, iter = 4000, seed = 811,
  control = list(adapt_delta = 0.95)
)


altrate_gloo2 <- loo(altrate_gmod2, save_psis = TRUE)  # PSIS-LOO
altrate_tloo2 <- loo(altrate_tmod2, save_psis = TRUE)

# Side-by-side comparison
loo_compare(altrate_gloo2, altrate_tloo2)

table(cut(altrate_gloo2$diagnostics$pareto_k, c(-Inf, 0.5, 0.7, 1, Inf)))
table(cut(altrate_tloo2$diagnostics$pareto_k, c(-Inf, 0.5, 0.7, 1, Inf)))

pp_check(altrate_gmod2, type = "ecdf_overlay_grouped", group = "period")
pp_check(altrate_tmod2, type = "ecdf_overlay_grouped", group = "period")
pp_check(altrate_gmod2, type = "dens_overlay_grouped", group = "period")
pp_check(altrate_tmod2, type = "dens_overlay_grouped", group = "period")

altrate_rates <- altitude_rate$rate
altrate_grates2 <- posterior_predict(altrate_gmod2, draws = 1000)
altrate_trates2 <- posterior_predict(altrate_tmod2, draws = 1000)

ppc_loo_pit_qq(altrate_rates, altrate_grates2, psis_object = altrate_gloo2$psis_object)
ppc_loo_pit_qq(altrate_rates, altrate_trates2, psis_object = altrate_tloo2$psis_object)

# nu

altrate_draws2 <- as_draws_df(altrate_tmod2)
altrate_nu2 <- altrate_draws2 %>%
  summarise(
    nu_mean = mean(nu),
    nu_median = median(nu),
    nu_q05 = quantile(nu, 0.05),
    nu_q95 = quantile(nu, 0.95)
  )
altrate_nu2

# We have an extremely low nu. We explore what this means
# i.e.: are there actually such heavy tails, or is it indicative of missing variance structure?

### 3.Seeing whether there's heteroskedasticity, and if the extreme nu can be caused by it

log(1/3)
priors_t3 <- c(
  # Mean model (as before)
  prior(normal(0, 0.5), class = "Intercept"),
  prior(normal(0, 0.5), class = "b"),
  prior(exponential(3), class = "sd"),
  prior(gamma(2, 0.4), class = "nu"),
  # Sigma model (log-scale)
  prior(normal(-1.098612, 0.5), class = "Intercept", dpar = "sigma"), 
  prior(normal(0, 0.3), class = "b", dpar = "sigma")
)
# For sigma, I tried 0.5 for intercept and 0.3 for b (instead of 0.3 and 0.2), but it resulted in one -inf value, and the brm function stopped

altrate_tmod3per <- brm(
  bf(rate ~ 
       period * specialisation + (1|summit) + (1|species),
     sigma ~ period),
  family = student(),
  prior = priors_t3,
  data = altitude_rate,
  chains = 4, iter = 4000, seed = 811,
  control = list(adapt_delta = 0.95)
)

altrate_tmod3spe <- brm(
  bf(rate ~ 
       period * specialisation + (1|summit) + (1|species),
     sigma ~ specialisation),
  family = student(),
  prior = priors_t3,
  data = altitude_rate,
  chains = 4, iter = 4000, seed = 811,
  control = list(adapt_delta = 0.95)
)

altrate_tmod3perspe <- brm(
  bf(rate ~ 
       period * specialisation + (1|summit) + (1|species),
     sigma ~ period + specialisation),
  family = student(),
  prior = priors_t3,
  data = altitude_rate,
  chains = 4, iter = 4000, seed = 811,
  control = list(adapt_delta = 0.95)
)

altrate_tmod3perxspe <- brm(
  bf(rate ~ 
       period * specialisation + (1|summit) + (1|species),
     sigma ~ period * specialisation),
  family = student(),
  prior = priors_t3,
  data = altitude_rate,
  chains = 4, iter = 4000, seed = 811,
  control = list(adapt_delta = 0.95)
)

loo_compare(loo(altrate_tmod2), 
            loo(altrate_tmod3per), 
            loo(altrate_tmod3spe), 
            loo(altrate_tmod3perspe),
            loo(altrate_tmod3perxspe))
posterior_summary(altrate_tmod2, variable = "nu")
posterior_summary(altrate_tmod3per, variable = "nu")
posterior_summary(altrate_tmod3spe, variable = "nu")
posterior_summary(altrate_tmod3perspe, variable = "nu")
posterior_summary(altrate_tmod3perxspe, variable = "nu")
# We keep the per model. period improves drastically the fit, and increases nu. specialisation does not do it. The additive and interactive models are slightly better, but the improvement is within 1 SD. So they add complexity without improving the results notably



### 4. Model validation

altrate_tmod3per |> summary()
# Rhat = 1.00 for all parameters
# Bulk and Tail Effective sample size > 1000 for all parameters
# No divergences

pp_check(altrate_tmod3per, type="dens_overlay_grouped", group="period")
pp_check(altrate_tmod3per, type="ecdf_overlay_grouped", group="period")


altrate_t3per_rates <- altitude_rate$rate
altrate_t3per_pred <- posterior_predict(altrate_tmod3per, draws = 1000)
altrate_t3per_loo <- loo(altrate_tmod3per, save_psis = TRUE)

ppc_loo_pit_qq(altrate_t3per_rates, 
               altrate_t3per_pred, 
               psis_object = altrate_t3per_loo$psis_object)
# The model slightly overestimates tail heaviness, but is well-calibrated for the central mass


### 5. Interpret model parameters

# Alpine species did not move systematically in any of the periods. Generalists moved slightly more than specialists in the first period, but not in the second
# There has been more heterogeneity in alitudinal movement in the second period than in the first - Period explains an increase in variability

# While mean elevational movement remains near zero for both sampling periods, residual variation increased more than threefold in the second period, indicating that species responses have become substantially more heterogeneous under recent climatic conditions.

# Little difference among species or summits

# Most species show modest movement, but a few exhibit surprisingly large shifts

altrate_t3per_r2 <- r2_bayes(altrate_tmod3per)
altrate_t3per_r2 # Neither the fixed effects or the random effects explain much


### 6. Post hoc

altrate_results <- altrate_tmod3per |> 
  mod_summary()
altrate_results
# Post-hoc analyses of mean elevational change




# Results----

emmeans_overview <- richrate_results$emmeans_df |>
  mutate(Model = "richness") |>
  rbind(turnew_results$emmeans_df |>
          mutate(Model = "new")) |>
  rbind(turlost_results$emmeans_df |>
          mutate(Model = "lost")) |>
  rbind(elerate_all_results$emmeans_df |>
          mutate(df = NA_real_,
                 statistic = NA_real_,
                 Model = "elevation") |>
          relocate(df, .after = Estimate) |>
          relocate(statistic, .after = CI_upper)) |>
  relocate(Model) |>
  mutate(Model = factor(Model, levels = c("richness", "new", "lost", "elevation")))


contrasts_overview <- richrate_results$contrast_df |>
  mutate(Model = "Species richness") |>
  rbind(turnew_results$contrast_df |>
          mutate(Model = "New species")) |>
  rbind(turlost_results$contrast_df |>
          mutate(Model = "Lost species")) |>
  rbind(elerate_all_results$contrast_df |>
          mutate(df = NA_real_,
                 statistic = NA_real_,
                 Model = "Uppermost occurrence") |>
          relocate(df, .after = Estimate) |>
          relocate(statistic, .after = CI_upper)) |>
  relocate(Model) |>
  mutate(Model = factor(Model, levels = c("Species richness", "New species", "Lost species", "Uppermost occurrence"))) |>
  mutate(Model = recode(Model, "Species richness" = "Species\nrichness", "Uppermost occurrence" = "Uppermost\noccurrence"))

contrasts_table <- contrasts_overview |>
  select(!c(df, statistic)) |> 
  mutate(Contrast = case_when(Contrast == "1A-2A" ~ "Specialists. Period 1 – Period 2",
                              Contrast == "1G-2G" ~ "Generalists. Period 1 - Period 2",
                              Contrast == "1A-1G" ~ "Period 1. Specialists - Generalists",
                              Contrast == "2A-2G" ~ "Period 2. Specialists - Generalists")) |> 
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
  set_header_labels(CI_lower = "CI Lower", CI_upper = "CI Upper", p_value = "p value") |> 
  align(part = "all", j = -c(1, 2), align = "center") |> 
  flextable::font(part = "all", fontname = "Times New Roman") |> 
  fontsize(size = 12) |> 
  autofit()
contrasts_table
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

richrate_figure <- richrate_results$emmeans_df |> 
  gg_results() +
  scale_x_continuous(limits = c(-0.4, 0.5),
                     labels = NULL) +
  labs(x = NULL, y = adj_label["richness"])

turnew_figure <- turnew_results$emmeans_df |> 
  gg_results() +
  scale_x_continuous(limits = c(-0.4, 0.5),
                     labels = NULL) +
  labs(x = NULL, y = adj_label["new"])

turlost_figure <- turlost_results$emmeans_df |> 
  gg_results() +
  scale_x_continuous(limits = c(-0.4, 0.5)) +
  labs(x = "Rate (species&nbsp;year<sup>-1</sup>&nbsp;summit<sup>-1</sup>)", y = adj_label["lost"]) +
  theme(axis.title.x = element_markdown())

elerate_all_figure <- elerate_all_results$emmeans_df |> 
  gg_results() +
  scale_x_continuous(limits = c(-0.06, 0.10)) +
  labs(x = "Rate (metres&nbsp;year<sup>-1</sup>&nbsp;summit<sup>-1</sup>)", y = adj_label["elevation"]) +
  theme(axis.title.x = element_markdown())

results_figure_stack <- ggarrange(
  plotlist = list(richrate_figure, turnew_figure, turlost_figure, elerate_all_figure),
  ncol = 1,
  nrow = 4,
  align = "v",
  common.legend = TRUE,
  heights = c(1, 1, 1.68, 1.68)
)
results_figure_stack
results_figure_stack |> ggsave(file = "Results/Rate_of_change_period.png", width = 20, height = 15, units = "cm")




# Variance----

# We noticed that variance seems to be greater in period 2 for most models. We address it here

## Richness

richrate_disp <- richrate_modh |> 
  emmeans(~ period, component = "disp") |>
  as.data.frame() |>
  transmute(period,
            logvar = emmean,
            logvar_lwr = lower.CL,
            logvar_upr = upper.CL,
            variance = exp(logvar), # Back-transform to variance with 95% CIs
            var_lwr = exp(logvar_lwr),
            var_upr = exp(logvar_upr))
richrate_disp

## Lost species

turlost_disp <- turlost_modh |> 
  emmeans(~ period * specialisation, component = "disp") |>
  as.data.frame() |> 
  transmute(period,
            specialisation,
            logvar = emmean,
            logvar_lwr = lower.CL,
            logvar_upr = upper.CL,
            variance = exp(logvar), # Back-transform to variance with 95% CIs
            var_lwr = exp(logvar_lwr),
            var_upr = exp(logvar_upr))
turlost_disp


## Elevational change

elerate_all_disp <- elerate_all_tmod3per |> 
  spread_draws(b_sigma_Intercept, b_sigma_periodperiod2) |>
  mutate(logvar_period1 = b_sigma_Intercept,
         logvar_period2 = b_sigma_Intercept + b_sigma_periodperiod2,
         var_period1 = exp(b_sigma_Intercept),
         var_period2 = exp(b_sigma_Intercept + b_sigma_periodperiod2)) |>
  pivot_longer(cols = c(logvar_period1, logvar_period2, var_period1, var_period2), 
               names_to = c("scale", "period"), 
               names_pattern = "(logvar|var)_period(\\d)") |> 
  summarise(.by = c("period", "scale"),
            estimate = mean(value),
            lwr = quantile(value, 0.025),
            upr = quantile(value, 0.975)) |> 
  pivot_longer(cols = c(estimate, lwr, upr)) |> 
  mutate(period = paste0("period", period),
         names = paste0(scale, "_", name)) |> 
  select((!c("scale", "name"))) |> 
  pivot_wider(names_from = names, values_from = value) |> 
  rename(logvar = logvar_estimate,
         variance = var_estimate)
elerate_all_disp


## Table

resvar_ft <- richrate_disp |> 
  mutate(model = "Richness", 
         specialisation = NA_real_,
         joined = glue::glue("{round(variance, 3)} ({round(var_lwr, 3)}–{round(var_upr, 3)})")) |> 
  select(model, specialisation, period, joined) |> 
  pivot_wider(names_from = period, values_from = joined) |> 
  rbind(turlost_disp |> 
          mutate(model = "Lost species", 
                 joined = glue::glue("{round(variance, 3)} ({round(var_lwr, 3)}–{round(var_upr, 3)})")) |> 
          select(model, specialisation, period, joined) |> 
          pivot_wider(names_from = period, values_from = joined)) |> 
  rbind(elerate_all_disp |> 
          mutate(model = "Uppermost occurrence", 
                 specialisation = NA_real_,
                 joined = glue::glue("{round(variance, 3)} ({round(var_lwr, 3)}–{round(var_upr, 3)})")) |> 
          select(model, specialisation, period, joined) |> 
          pivot_wider(names_from = period, values_from = joined)) |> 
  mutate(specialisation = ifelse(specialisation == "alpine", "Specialist", "Generalist")) |> 
  flextable() |> 
  set_header_labels(model = "Model", 
                    specialisation = "Specialisation group", 
                    period1 = "Period 1", 
                    period2 = "Period 2") |> 
  bg(part = "header", bg = "black") |> 
  color(part = "header", color = "white") |> 
  bold(part = "header", bold = TRUE) |>
  bg(part = "body", bg = "white") |> 
  color(part = "body", color = "black") |>
  align(part = "all", j = 3:4, align = "center") |> 
  hline(i = c(1, 3)) |> 
  flextable::font(part = "all", fontname = "Times New Roman") |> 
  autofit()
resvar_ft
resvar_ft |> save_as_image(path = "Results/Variance_differences.png")




# Nature type Analysis ~ ...----
## All observations----

all_species_types |> 
  ggplot() +
  geom_histogram(aes(x = total))

alltypes_mod <- glmmTMB(
  total ~
    main_type * specialisation + log(habitat_decare) + (1 | summit),
  family = nbinom2(),
  data = all_species_types
)

alltypes_mod |> model_diagnosis() # No problems
alltypes_mod |> model_homoscedasticity() # No problems
alltypes_mod |> summary()

alltypes_results <- alltypes_mod |> mod_types()

alltypes_results$emmeans_df |> 
  # mutate(specialisation = factor(specialisation, levels = c("generalist", "alpine"))) |>
  # mutate(letters = c("A", "A", "B", "C", "C", "BC")) |> 
  ggplot(aes(x = Estimate, y = Habitat, colour = specialisation, group = specialisation)) +
  theme_minimal() +
  geom_vline(xintercept = 0, colour = "black") +
  geom_pointrangeh(aes(xmin = CI_lower, xmax = CI_upper), size = 0.8, fatten = 4, position = position_dodge2v(height = 0.4, reverse = TRUE)) +
  # geom_point(size = 3, position = test25) +
  # geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.4, position = test25) +
  # geom_text(aes(x = CI_upper + 0.4, label = letters)) +
  scale_x_continuous(name = "Number of species / 1000m2") +
  scale_colour_manual("Specialisation", values = colour_mapping$specialisation, labels = adj_label) +
  scale_y_discrete(labels = adj_label, limits = rev) +
  theme(text = element_text(size = 14, family = "serif"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(hjust = 0.35),
        axis.text.x = element_text(margin = margin(t = 10, b = 10)),
        panel.grid.major.y = element_blank(),
        legend.position = "top",
        legend.box.margin = margin(l = -10),
        legend.title = element_text(margin = margin(b = 5, r = 40)),
        legend.text = element_text(margin = margin(l = 9, r = 20, b = 4)))
newtypes_figure
newtypes_figure |> ggsave(file = "Results/Habitat_types.png", width = 20, height = 15, units = "cm")
test25 <- 
  ggstance::position_dodge2v(
    height   = 0.6,     # <- this controls vertical separation between the two groups
    preserve = "total", # keeps total stack height similar even if a group is missing
    reverse  = TRUE     # draws the first level above; also affects legend if reversed
  )




## New observations----

new_species_types |> 
  ggplot() +
  geom_histogram(aes(x = total))

newtypes_mod <- glmmTMB(
  total ~
    main_type * specialisation + offset(log(habitat_decare)) + (1 | summit),
  family = nbinom2(),
  data = new_species_types
)
# I've tried including specialisation and there is no difference

newtypes_mod |> model_diagnosis() # Quantiles
newtypes_mod |> model_homoscedasticity() # No problems
newtypes_mod |> summary()

newtypes_results <- newtypes_mod |> 
  mod_types()

newtypes_figure <- newtypes_results$emmeans_df |> 
  mutate(letters = c("A", "A", "B", "C", "C", "BC")) |> 
  ggplot(aes(x = Estimate, y = Habitat)) +
  theme_minimal() +
  geom_vline(xintercept = 0, colour = "black") +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.4) +
  geom_text(aes(x = CI_upper + 0.4, label = letters)) +
  scale_x_continuous(name = "Number of new species / Decare") +
  scale_y_discrete(labels = adj_label, limits = rev) +
  theme(text = element_text(size = 14, family = "serif"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(hjust = 0.35),
        axis.text.x = element_text(margin = margin(t = 10, b = 10)),
        panel.grid.major.y = element_blank(),
        legend.position = "top",
        legend.box.margin = margin(l = -10),
        legend.title = element_text(margin = margin(b = 5, r = 40)),
        legend.text = element_text(margin = margin(l = 9, r = 20, b = 4)))
newtypes_figure
newtypes_figure |> ggsave(file = "Results/Habitat_types.png", width = 20, height = 15, units = "cm")






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
elerate_all_gbayes <- brm(
  rate ~ 
    period * specialisation + (1|summit) + (1|species),
  family = gaussian(), 
  data = elerate_all,
  seed = 811)
withr::with_seed(811, pp_check(elerate_all_gbayes, type = "dens_overlay")) # The shape does not really fit, though the range is similar

elerate_all_tbayes <- brm(
  rate ~ 
    period * specialisation + (1|summit) + (1|species),
  family = student(), 
  data = elerate_all,
  seed = 811)
withr::with_seed(811, pp_check(elerate_all_tbayes, type = "dens_overlay")) # Some extreme values 

loo(elerate_all_gbayes, elerate_all_tbayes) # It seems student t is better, we have to fix the priors to our expectations
# I have also tried beta (using 32 as maximum possible change), but doesn't really fit (not good for a peak)

elerate_all_tbayesp <- brm(
  rate ~ 
    period * specialisation + (1 | summit) + (1 | species), 
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
  specialisation = elerate_all$specialisation,
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
ggplot(elerate_bayes_res, aes(x = fitted, y = residuals, colour = specialisation)) +
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
pp_check(elerate_all_tbayesp, type = "dens_overlay_grouped", group = "specialisation") # Quite similar



### Model 2

elerate_all_bayesh <- brm(
  bf(rate ~
       period * specialisation + (1|summit) + (1|species),
     sigma ~period),
  family = student(),
  prior = c(
    prior(normal(0, 0.5), class = "b"),
    prior(normal(0, 0.5), class = "Intercept"),
    prior(gamma(46, 1), class = "nu")
  ),
  data = elerate_all,
  control = list(adapt_delta = 0.999),
  seed = 811
)
# elerate_all_bayesh <- tar_read(elerate_all_bayes) # To double-check targets
withr::with_seed(811, pp_check(elerate_all_bayesh, type = "dens_overlay", size = 1))
loo(elerate_all_tbayesp, elerate_all_bayesh)

## Diagnosis

# Model convergence and sample quality
elerate_all_bayesh |> summary()   # Rhat = 1, no divergent transitions, large ESS (effective sample size)
elerate_all_bayesh |> plot() # hairy caterpillars

# Posterior predictive check
withr::with_seed(811, pp_check(elerate_all_bayesh, type = "dens_overlay")) # Quite good

# Residuals
elerate_all_bayesh_res <- tibble(
  fitted = fitted(elerate_all_bayesh)[, "Estimate"],
  residuals = residuals(elerate_all_bayesh)[, "Estimate"],
  period = elerate_all$period,
  specialisation = elerate_all$specialisation,
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

ggplot(elerate_all_bayesh_res, aes(x = fitted, y = residuals, colour = specialisation)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    x = "Fitted values",
    y = "Residuals",
    title = "Residuals vs Fitted Values"
  ) +
  theme_minimal()
withr::with_seed(811, pp_check(elerate_all_bayesh, type = "dens_overlay_grouped", group = "specialisation"))
# Quite similar distributions, no need to adjust for it

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

elerate_rem_bayesh <- brm(
  bf(rate ~
       period * specialisation + (1|summit) + (1|species),
     sigma ~period),
  family = student(),
  prior = c(
    prior(normal(0, 0.5), class = "b"),
    prior(normal(0, 0.5), class = "Intercept"),
    prior(gamma(45, 1), class = "nu")
  ),
  data = elerate_remained,
  control = list(adapt_delta = 0.999),
  seed = 811
)
# elerate_rem_bayesh <- tar_read(elerate_rem_bayes) # To double check targets

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
  specialisation = elerate_remained$specialisation,
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

ggplot(elerate_rem_bayesh_res, aes(x = fitted, y = residuals, colour = specialisation)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    x = "Fitted values",
    y = "Residuals",
    title = "Residuals vs Fitted Values"
  ) +
  theme_minimal()
withr::with_seed(811, pp_check(elerate_rem_bayesh, type = "dens_overlay_grouped", group = "specialisation"))
# Very similar distributions

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

elerate_new_bayesh <- brm(
  bf(rate ~
       period * specialisation + (1|summit) + (1|species),
     sigma ~period),
  family = student(),
  data = elerate_new,
  control = list(adapt_delta = 0.999),
  seed = 811
)
# elerate_new_bayesh <- tar_read(elerate_new_bayes) # To double-check targets
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
  specialisation = elerate_new$specialisation,
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


ggplot(elerate_new_bayesh_res, aes(x = fitted, y = residuals, colour = specialisation)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    x = "Fitted values",
    y = "Residuals",
    title = "Residuals vs Fitted Values"
  ) +
  theme_minimal()
withr::with_seed(811, pp_check(elerate_new_bayesh, type = "dens_overlay_grouped", group = "specialisation"))

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



### For the appendix----

eleextra_emmeans <- elerate_all_results$emmeans_df |> 
  select(!c(SE, p_value)) |> 
  mutate(Model = "all") |> 
  rbind(elerate_rem_results$emmeans_df |> 
          select(!c(SE, p_value)) |> 
          mutate(Model = "rem")) |> 
  rbind(elerate_new_results$emmeans_df |> 
          select(!c(SE, p_value)) |> 
          mutate(Model = "newer")) |> 
  relocate(Model) |> 
  mutate(Model = factor(Model, levels = c("all", "rem", "newer")))

eleextra_figure <- eleextra_emmeans |> gg_results() +
  facet_grid(rows = vars(Model), switch = "y", labeller = as_labeller(adj_label)) +
  scale_y_discrete(position = "right", labels = adj_label) +
  labs(x = "Rate of change") +
  theme(panel.spacing.y = unit(1, "lines"),
        text = element_text(size = 16),
        strip.text.y.left = element_markdown(angle = 0, hjust = 0),
        axis.title.y = element_blank())
eleextra_figure |> ggsave(file = "Results/Extra/Altitudinal_change.png", width = 20, height = 15, units = "cm")

# turnover_species <- elevation_wide |>
#   select(!first:third) |>
#   mutate(presence1 = ifelse(is.na(distance1), 0, 1),
#          presence2 = ifelse(is.na(distance2), 0, 1),
#          presence3 = ifelse(is.na(distance3), 0, 1),
#          turnover1 = presence2 - presence1,
#          turnover2 = presence3 - presence2,
#          development = case_when(turnover1 == 1 & turnover2 == 1 ~ "Error",
#                                  turnover1 == 1 & turnover2 == 0 ~ "Appeared1",
#                                  turnover1 == 1 & turnover2 == -1 ~ "Forth_back",
#                                  turnover1 == 0 & turnover2 == 1 ~ "Appeared2",
#                                  turnover1 == 0 & turnover2 == 0 ~ "Remained",
#                                  turnover1 == 0 & turnover2 == -1 ~ "Disappeared2",
#                                  turnover1 == -1 & turnover2 == 1 ~ "Back_forth",
#                                  turnover1 == -1 & turnover2 == 0 ~ "Disappeared1",
#                                  turnover1 == -1 & turnover2 == -1 ~ "Error")) |>
#   select(!distance1:distance3) |>
#   relocate(development, .after = species) |>
#   mutate(rate1 = turnover1 / period1,
#          rate2 = turnover2 / period2) |>
#   pivot_longer(cols = c(period1, period2), names_to = "period", values_to = "years") |>
#   mutate(rate = ifelse(period == "period1", rate1, rate2)) |>
#   relocate(period:rate, .after = development) |>
#   mutate(period = as.factor(period))
#
# turnover_grouped <- turnover_species |>
#   summarise(.by = c("specialisation", "development"), total = n() / 2) |> # Divide by two since for each species we have two rows, one per period
#   arrange(development, specialisation)
#
# turnover_bydevelopment <- turnover_species |>
#   summarise(.by = c("development"), total = n() / 2) # Divide by two since for each species we have two rows, one per period
#
#
# # By summit
#
# turnover_summit <- turnover_species |>
#   summarise(.by = c(summit, period, specialisation),
#             new = sum(case_when(rate > 0 ~ rate), na.rm = TRUE),
#             nochange = sum(case_when(rate == 0 ~ rate), na.rm = TRUE),
#             lost = sum(case_when(rate < 0 ~ rate), na.rm = TRUE))
#
# turnover_summit_tourist <- turnover_species |>
#   filter(development %in% c("Forth_back", "Back_forth")) |>
#   summarise(.by = c(summit, period),
#             new = sum(case_when(rate > 0 ~ rate), na.rm = TRUE),
#             nochange = sum(case_when(rate == 0 ~ rate), na.rm = TRUE),
#             lost = sum(case_when(rate < 0 ~ rate), na.rm = TRUE))
#
# turnover_summit_nontourist <- turnover_species |>
#   filter(!(development %in% c("Forth_back", "Back_forth"))) |>
#   summarise(.by = c(summit, period),
#             new = sum(case_when(rate > 0 ~ rate), na.rm = TRUE),
#             nochange = sum(case_when(rate == 0 ~ rate), na.rm = TRUE),
#             lost = sum(case_when(rate < 0 ~ rate), na.rm = TRUE))
