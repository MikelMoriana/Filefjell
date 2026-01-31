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



## Turnover overview----

# General

status_survey <- filefjell_simplified |>
  pivot_wider(names_from = year, values_from = distance) |>
  mutate(status1 = case_when(!is.na(first) ~ "1972Present",
                             is.na(first) ~ "1972Absent")) |>
  mutate(status2 = case_when(!is.na(first) & !is.na(second) ~ "2009Remained",
                             !is.na(first) & is.na(second) ~ "2009Lost",
                             is.na(first) & !is.na(second) ~ "2009New",
                             is.na(first) & is.na(second) ~ "2009Absent")) |>
  mutate(status3 = case_when(!is.na(first) & !is.na(second) & !is.na(third) ~ "2024Remained",
                             !is.na(first) & !is.na(second) & is.na(third) ~ "2024Lost",
                             !is.na(first) & is.na(second) & !is.na(third) ~ "2024Reappeared",
                             !is.na(first) & is.na(second) & is.na(third) ~ "2024Stayed_lost",
                             is.na(first) & !is.na(second) & !is.na(third) ~ "2024Stayed",
                             is.na(first) & !is.na(second) == 1 & is.na(third) ~ "2024Disappeared",
                             is.na(first) & is.na(second) & !is.na(third) ~ "2024New")) |>
  select(status1:status3) |>
  pivot_longer(cols = status1:status3, names_to = "survey", values_to = "status") |>
  summarise(.by = "status", total = n()) |>
  arrange(status)

header_map <- tibble(
  col_keys = c("1972status", "1972total", "2008/09status", "2008/09total", "2024/25status", "2024/25total"),
  level1   = c("1972", "1972", "2008/09", "2008/09", "2024/25", "2024/25")
)

status_survey_ft <- tibble(
  "1972status" = c("Present",
                   rep("", 6),
                   "Total present"),
  "1972total" = c(status_survey |> filter(status == "1972Present") |> pull(total),
                  rep("", 6),
                  status_survey |> filter(status == "1972Present") |> pull(total)),
  "2008/09status" = c("Remained", "", "Lost", "", "New", "", "", "Total present"),
  "2008/09total" = c(status_survey |> filter(status == "2009Remained") |> pull(total), "",
                     status_survey |> filter(status == "2009Lost") |> pull(total), "",
                     status_survey |> filter(status == "2009New") |> pull(total), "", "",
                     (status_survey |> filter(status == "2009Remained") |> pull(total)) + (status_survey |> filter(status == "2009New") |> pull(total))),
  "2024/25status" = c("Remained", "Lost", "Reappeared", "Did not reappear", "Remained", "Lost", "New", "Total present"),
  "2024/25total" = c(status_survey |> filter(status == "2024Remained") |> pull(total),
                     status_survey |> filter(status == "2024Lost") |> pull(total),
                     status_survey |> filter(status == "2024Reappeared") |> pull(total),
                     status_survey |> filter(status == "2024Stayed_lost") |> pull(total),
                     status_survey |> filter(status == "2024Stayed") |> pull(total),
                     status_survey |> filter(status == "2024Disappeared") |> pull(total),
                     status_survey |> filter(status == "2024New") |> pull(total),
                     (status_survey |> filter(status == "2024Remained") |> pull(total)) + (status_survey |> filter(status == "2024Reappeared") |> pull(total)) + (status_survey |> filter(status == "2024Stayed") |> pull(total)) + (status_survey |> filter(status == "2024New") |> pull(total)))
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
status_survey_ft

status_survey_ft |> save_as_image(path = "Results/Status_survey.png")


# By specialisation

status_spe_survey <- filefjell_simplified |>
  pivot_wider(names_from = year, values_from = distance) |>
  mutate(status1 = case_when(!is.na(first) ~ "1972Present",
                             is.na(first) ~ "1972Absent")) |>
  mutate(status2 = case_when(!is.na(first) & !is.na(second) ~ "2009Remained",
                             !is.na(first) & is.na(second) ~ "2009Lost",
                             is.na(first) & !is.na(second) ~ "2009New",
                             is.na(first) & is.na(second) ~ "2009Absent")) |>
  mutate(status3 = case_when(!is.na(first) & !is.na(second) & !is.na(third) ~ "2024Remained",
                             !is.na(first) & !is.na(second) & is.na(third) ~ "2024Lost",
                             !is.na(first) & is.na(second) & !is.na(third) ~ "2024Reappeared",
                             !is.na(first) & is.na(second) & is.na(third) ~ "2024Stayed_lost",
                             is.na(first) & !is.na(second) & !is.na(third) ~ "2024Stayed",
                             is.na(first) & !is.na(second) == 1 & is.na(third) ~ "2024Disappeared",
                             is.na(first) & is.na(second) & !is.na(third) ~ "2024New")) |>
  select(specialisation, status1:status3) |>
  pivot_longer(cols = status1:status3, names_to = "survey", values_to = "status") |>
  summarise(.by = c("specialisation", "status"), total = n()) |>
  arrange(status)

header_spe_map <- tibble(
  col_keys = c("specialisation", "1972status", "1972total", "2008/09status", "2008/09total", "2024/25status", "2024/25total"),
  level1   = c("Specialisation", "1972", "1972", "2008/09", "2008/09", "2024/25", "2024/25")
)

status_spe_survey_ft <- tibble(
  "specialisation" = c("Specialists",
                       rep("", 7),
                       "Generalists",
                       rep("", 7)),
  "1972status" = c("Present",
                   rep("", 6),
                   "Total specialists",
                   "Present",
                   rep("", 6),
                   "Total generalists"),
  "1972total" = c(status_spe_survey |> filter(specialisation == "alpine" & status == "1972Present") |> pull(total),
                  rep("", 6),
                  status_spe_survey |> filter(specialisation == "alpine" & status == "1972Present") |> pull(total),
                  status_spe_survey |> filter(specialisation == "generalist" & status == "1972Present") |> pull(total),
                  rep("", 6),
                  status_spe_survey |> filter(specialisation == "generalist" & status == "1972Present") |> pull(total)),
  "2008/09status" = c("Remained", "", "Lost", "", "New", "", "", "Total present",
                      "Remained", "", "Lost", "", "New", "", "", "Total present"),
  "2008/09total" = c(status_spe_survey |> filter(specialisation == "alpine" & status == "2009Remained") |> pull(total),
                     "",
                     status_spe_survey |> filter(specialisation == "alpine" & status == "2009Lost") |> pull(total),
                     "",
                     status_spe_survey |> filter(specialisation == "alpine" & status == "2009New") |> pull(total),
                     "",
                     "",
                     (status_spe_survey |> filter(specialisation == "alpine" & status == "2009Remained") |> pull(total)) +
                       (status_spe_survey |> filter(specialisation == "alpine" & status == "2009New") |> pull(total)),
                     status_spe_survey |> filter(specialisation == "generalist" & status == "2009Remained") |> pull(total),
                     "",
                     status_spe_survey |> filter(specialisation == "generalist" & status == "2009Lost") |> pull(total),"
                     ",
                     status_spe_survey |> filter(specialisation == "generalist" & status == "2009New") |> pull(total),
                     "",
                     "",
                     (status_spe_survey |> filter(specialisation == "generalist" & status == "2009Remained") |> pull(total)) +
                       (status_spe_survey |> filter(specialisation == "generalist" & status == "2009New") |> pull(total))),
  "2024/25status" = c("Remained", "Lost", "Reappeared", "Did not reappear", "Remained", "Lost", "New", "Total present",
                      "Remained", "Lost", "Reappeared", "Did not reappear", "Remained", "Lost", "New", "Total present"),
  "2024/25total" = c(status_spe_survey |> filter(specialisation == "alpine" & status == "2024Remained") |> pull(total),
                     status_spe_survey |> filter(specialisation == "alpine" & status == "2024Lost") |> pull(total),
                     status_spe_survey |> filter(specialisation == "alpine" & status == "2024Reappeared") |> pull(total),
                     status_spe_survey |> filter(specialisation == "alpine" & status == "2024Stayed_lost") |> pull(total),
                     status_spe_survey |> filter(specialisation == "alpine" & status == "2024Stayed") |> pull(total),
                     status_spe_survey |> filter(specialisation == "alpine" & status == "2024Disappeared") |> pull(total),
                     status_spe_survey |> filter(specialisation == "alpine" & status == "2024New") |> pull(total),
                     (status_spe_survey |> filter(specialisation == "alpine" & status == "2024Remained") |> pull(total)) +
                       (status_spe_survey |> filter(specialisation == "alpine" & status == "2024Reappeared") |> pull(total)) +
                       (status_spe_survey |> filter(specialisation == "alpine" & status == "2024Stayed") |> pull(total)) +
                       (status_spe_survey |> filter(specialisation == "alpine" & status == "2024New") |> pull(total)),
                     status_spe_survey |> filter(specialisation == "generalist" & status == "2024Remained") |> pull(total),
                     status_spe_survey |> filter(specialisation == "generalist" & status == "2024Lost") |> pull(total),
                     status_spe_survey |> filter(specialisation == "generalist" & status == "2024Reappeared") |> pull(total),
                     status_spe_survey |> filter(specialisation == "generalist" & status == "2024Stayed_lost") |> pull(total),
                     status_spe_survey |> filter(specialisation == "generalist" & status == "2024Stayed") |> pull(total),
                     status_spe_survey |> filter(specialisation == "generalist" & status == "2024Disappeared") |> pull(total),
                     status_spe_survey |> filter(specialisation == "generalist" & status == "2024New") |> pull(total),
                     (status_spe_survey |> filter(specialisation == "generalist" & status == "2024Remained") |> pull(total)) +
                       (status_spe_survey |> filter(specialisation == "generalist" & status == "2024Reappeared") |> pull(total)) +
                       (status_spe_survey |> filter(specialisation == "generalist" & status == "2024Stayed") |> pull(total)) +
                       (status_spe_survey |> filter(specialisation == "generalist" & status == "2024New") |> pull(total)))
) |>
  flextable() |>
  set_header_df(mapping = header_spe_map, key = "col_keys") |>
  merge_h(part = "header") |>
  bg(part = "header", bg = "black") |>
  color(part = "header", color = "white") |>
  bold(part = "header") |>
  align(part = "header", j = 2:7, align = "center") |>
  bg(part = "body", bg = "white") |>
  color(part = "body", color = "black") |>
  bg(part = "body", i = c(8, 16), bg = "grey") |>
  hline(i = c(4, 12), j = 2:7) |>
  hline(i = c(2, 10), j = 4:7) |>
  hline(i = c(6, 14), j = 4:7) |>
  border(i = 8, border.bottom = officer::fp_border(width = 2)) |>
  border(i = c(1:6, 8, 9:14, 16), j = 4, border.left = officer::fp_border(color = "black")) |>
  border(i = 1:16, j = c(2, 6), border.left = officer::fp_border(color = "black")) |>
  border(part = "header", border.left = officer::fp_border(color = "white")) |>
  vline_left() |>
  vline_right() |>
  align(part = "body", align = "left") |>
  align(part = "body", j = c(3, 5, 7), align = "center") |>
  flextable::font(part = "all", fontname = "Times New Roman") |>
  autofit()
status_spe_survey_ft

status_spe_survey_ft |> save_as_image(path = "Results/Status_spe_survey.png")


# # Turnover by distance to top
#
# turnover_status <- elevation_wide |>
#   select(!c(first:third, period1, period2)) |>
#   mutate(development = case_when(!is.na(distance1) & !is.na(distance2) & !is.na(distance3) ~ "Remained",
#                                  !is.na(distance1) & !is.na(distance2) & is.na(distance3) ~ "Disappeared2",
#                                  !is.na(distance1) & is.na(distance2) & !is.na(distance3) ~ "Back_forth",
#                                  !is.na(distance1) & is.na(distance2) & is.na(distance3) ~ "Disappeared1",
#                                  is.na(distance1) & !is.na(distance2) & !is.na(distance3) ~ "Appeared1",
#                                  is.na(distance1) & !is.na(distance2) & is.na(distance3) ~ "Forth_back",
#                                  is.na(distance1) & is.na(distance2) & !is.na(distance3) ~ "Appeared2")) |>
#   pivot_longer(cols = c(distance1:distance3), names_to = "measurement", values_to = "distance") |>
#   filter(!is.na(distance)) |>
#   mutate(altitude = (-1)*distance + 32,
#          development = factor(development, levels = c("Remained", "Disappeared2", "Back_forth", "Disappeared1", "Appeared1", "Forth_back", "Appeared2")))
#
# turnover_status |>
#   ggplot() +
#   geom_histogram(aes(x = (altitude+1)/34))
#
# turdev_mod <- glmmTMB(
#   (altitude+1)/34 ~
#     development,
#   family = beta_family(),
#   dispformula = ~development,
#   data = turnover_status)
# turdev_mod |> model_diagnosis()
# turdev_mod |> model_homoscedasticity()
# turdev_mod |> summary()
#
# turdev_emmeans <- turdev_mod |> emmeans(~development) |> contrast(method = "pairwise")
# turdev_emmeans
# turdev_letters <- turnover_status |>
#   select(development) |>
#   mutate(levels = case_when(development == "Remained" ~ "A",
#                             development == "Disappeared2" ~ "BC",
#                             development == "Back_forth" ~ "BC",
#                             development == "Disappeared1" ~ "BC",
#                             development == "Appeared1" ~ "B",
#                             development == "Forth_back" ~ "C",
#                             development == "Appeared2" ~ "B"))
#
# turnover_status |>
#   ggplot() +
#   geom_boxplot(aes(x = development, y = altitude)) +
#   geom_text(data = turdev_letters, aes(x = development, y = 33, label = levels))




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
  data = altitude_rate,
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



## Top 10 metres

altrate10_tmod <- brm(
  bf(rate ~
       period * specialisation + (1|summit) + (1|species),
     sigma ~ period),
  family = student(),
  prior = priors_t3,
  data = altitude10_rate,
  chains = 4, iter = 4000, seed = 811,
  control = list(adapt_delta = 0.95)
)

altrate10_tmod |> summary()
# Rhat = 1.00 for all parameters
# Bulk and Tail Effective sample size > 1000 for all parameters
# No divergences

pp_check(altrate10_tmod, type="dens_overlay_grouped", group="period")
pp_check(altrate10_tmod, type="ecdf_overlay_grouped", group="period")


altrate10_t_rates <- altitude10_rate$rate
altrate10_t_pred <- posterior_predict(altrate10_tmod, draws = 1000)
altrate10_t_loo <- loo(altrate10_tmod, save_psis = TRUE)

ppc_loo_pit_qq(altrate10_t_rates,
               altrate10_t_pred,
               psis_object = altrate10_t_loo$psis_object)

altrate10_results <- altrate10_tmod |>
  mod_summary()
altrate10_results




# Rate Results----

emmeans_overview <- richrate_results$emmeans_df |>
  mutate(Model = "Species richness") |>
  rbind(new_results$emmeans_df |>
          mutate(Model = "New species")) |>
  rbind(lost_results$emmeans_df |>
          mutate(Model = "Lost species")) |>
  rbind(altrate_results$emmeans_df |>
          mutate(df = NA_real_,
                 statistic = NA_real_,
                 Model = "Uppermost occurrence") |>
          relocate(df, .after = Estimate) |>
          relocate(statistic, .after = CI_upper)) |>
  relocate(Model) |>
  mutate(Model = factor(Model, levels = c("Species richness", "New species", "Lost species", "Uppermost occurrence"))) |>
  mutate(Model = recode(Model, "Species richness" = "Species\nrichness", "Uppermost occurrence" = "Uppermost\noccurrence")) |>
  rename(Specialisation = specialisation)

emmeans_table <- emmeans_overview |>
  select(!c(df, SE, statistic)) |>
  arrange(Model, Period, Specialisation) |>
  mutate(Period = case_when(Period == "period1" ~ "1972–2008/09",
                            Period == "period2" ~ "2008/09–2024/25"),
         Specialisation = case_when(Specialisation == "alpine" ~ "Specialist",
                                    Specialisation == "generalist" ~ "Generalist")) |>
  flextable() |>
  bg(part = "header", bg = "black") |>
  color(part = "header", color = "white") |>
  bold(part = "header") |>
  bg(part = "body", bg = "white") |>
  color(part = "body", color = "black") |>
  merge_v(j = "Model") |>
  merge_v(j = "Period") |>
  hline(i = c(4, 8, 12)) |>
  hline(i = c(2, 6, 10, 14), border = officer::fp_border(style = "dotted")) |>
  bold(i = ~ ((CI_lower * CI_upper) > 0), j = 3:7) |>
  set_header_labels(CI_lower = "CI Lower", CI_upper = "CI Upper", p_value = "p value") |>
  align(part = "all", j = 4:7, align = "center") |>
  flextable::font(part = "all", fontname = "Times New Roman") |>
  fontsize(size = 12) |>
  autofit()
emmeans_table
emmeans_table |> save_as_image(path = "Results/Rates_emmeans_table.png")


contrasts_overview <- richrate_results$contrast_df |>
  mutate(Model = "Species richness") |>
  rbind(new_results$contrast_df |>
          mutate(Model = "New species")) |>
  rbind(lost_results$contrast_df |>
          mutate(Model = "Lost species")) |>
  rbind(altrate_results$contrast_df |>
          mutate(df = NA_real_,
                 statistic = NA_real_,
                 Model = "Uppermost occurrence") |>
          relocate(df, .after = Estimate) |>
          relocate(statistic, .after = CI_upper)) |>
  relocate(Model) |>
  mutate(Model = factor(Model, levels = c("Species richness", "New species", "Lost species", "Uppermost occurrence"))) |>
  mutate(Model = recode(Model, "Species richness" = "Species\nrichness", "Uppermost occurrence" = "Uppermost\noccurrence"))

contrasts_table <- contrasts_overview |>
  select(!c(df, SE, statistic)) |>
  mutate(Contrast = case_when(Contrast == "1A-2A" ~ "Period 1 – Period 2. Specialists",
                              Contrast == "1G-2G" ~ "Period 1 – Period 2. Generalists",
                              Contrast == "1A-1G" ~ "Specialists – Generalists. Period 1",
                              Contrast == "2A-2G" ~ "Specialists – Generalists. Period 2")) |>
  separate_wider_delim(cols = Contrast, delim = ".", names = c("Contrast", "Group")) |>
  flextable() |>
  bg(part = "header", bg = "black") |>
  color(part = "header", color = "white") |>
  bold(part = "header") |>
  bg(part = "body", bg = "white") |>
  color(part = "body", color = "black") |>
  merge_v(j = "Model") |>
  merge_v(j = "Contrast") |>
  hline(i = c(4, 8, 12)) |>
  hline(i = c(2, 6, 10, 14), border = officer::fp_border(style = "dotted")) |>
  bold(i = ~ ((CI_lower * CI_upper) > 0), j = -1) |>
  set_header_labels(CI_lower = "CI Lower", CI_upper = "CI Upper", p_value = "p value") |>
  align(part = "all", j = -c(1:3), align = "center") |>
  flextable::font(part = "all", fontname = "Times New Roman") |>
  fontsize(size = 12) |>
  autofit()
contrasts_table
contrasts_table |> save_as_image(path = "Results/Rates_contrasts_table.png")


# Figure

richrate_figure <- richrate_results$emmeans_df |>
  gg_results() +
  scale_x_continuous(limits = c(-0.3, 0.5),
                     labels = NULL) +
  labs(x = NULL, y = adj_label["richness"])

new_figure <- new_results$emmeans_df |>
  gg_results() +
  scale_x_continuous(limits = c(-0.3, 0.5),
                     labels = NULL) +
  labs(x = NULL, y = adj_label["new"])

lost_figure <- lost_results$emmeans_df |>
  gg_results() +
  scale_x_continuous(limits = c(-0.3, 0.5)) +
  labs(x = "Rate (species&nbsp;year<sup>-1</sup>&nbsp;summit<sup>-1</sup>)", y = adj_label["lost"]) +
  theme(axis.title.x = element_markdown())

altrate_figure <- altrate_results$emmeans_df |>
  gg_results() +
  scale_x_continuous(limits = c(-0.06, 0.1)) +
  labs(x = "Rate (metres&nbsp;year<sup>-1</sup>&nbsp;summit<sup>-1</sup>)", y = adj_label["altitude"]) +
  theme(axis.title.x = element_markdown())

rates_figure <- ggarrange(
  plotlist = list(richrate_figure, new_figure, lost_figure, altrate_figure),
  ncol = 1,
  nrow = 4,
  align = "v",
  common.legend = TRUE,
  heights = c(1, 1, 1.68, 1.68)
)
rates_figure
rates_figure |> ggsave(file = "Results/Rates_emmeans_figure.png", width = 20, height = 15, units = "cm")




# Rate Variance----

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

lost_disp <- lost_modh |>
  emmeans(~ period, component = "disp") |>
  as.data.frame() |>
  transmute(period,
            logvar = emmean,
            logvar_lwr = lower.CL,
            logvar_upr = upper.CL,
            variance = exp(logvar), # Back-transform to variance with 95% CIs
            var_lwr = exp(logvar_lwr),
            var_upr = exp(logvar_upr))
lost_disp


## Altitudinal change

altrate_disp <- altrate_tmod3per |>
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
altrate_disp


## Table

variance_ft <- richrate_disp |>
  mutate(model = "Species richness",
         joined = glue::glue("{round(variance, 2)} ({round(var_lwr, 2)}–{round(var_upr, 2)})")) |>
  select(model, period, joined) |>
  rbind(lost_disp |>
          mutate(model = "Lost species",
                 joined = glue::glue("{round(variance, 2)} ({round(var_lwr, 2)}–{round(var_upr, 2)})")) |>
          select(model, period, joined)) |>
  rbind(altrate_disp |>
          mutate(model = "Uppermost occurrence",
                 joined = glue::glue("{round(variance, 2)} ({round(var_lwr, 2)}–{round(var_upr, 2)})")) |>
          select(model, period, joined)) |>
          pivot_wider(names_from = period, values_from = joined) |>
  flextable() |>
  set_header_labels(model = "Model",
                    period1 = "Period 1",
                    period2 = "Period 2") |>
  bg(part = "header", bg = "black") |>
  color(part = "header", color = "white") |>
  bold(part = "header", bold = TRUE) |>
  bg(part = "body", bg = "white") |>
  color(part = "body", color = "black") |>
  align(part = "all", j = 2:3, align = "center") |>
  flextable::font(part = "all", fontname = "Times New Roman") |>
  autofit()
variance_ft
variance_ft |> save_as_image(path = "Results/Rates_Variance_table.png")




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
