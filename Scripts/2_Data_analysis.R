# Libraries----

source("Scripts/0_setup.R")



# General data----

filefjell_data_clean <- tar_read(filefjell_data_clean)
summit_data <- tar_read(summit_data_tidy)

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
  select(!c(height:date)) |>
  mutate(year = case_when(year == 1972 ~ "first",
                          year %in% c(2008, 2009) ~ "second",
                          year %in% c(2024, 2025) ~ "third"))



# Summit general info----

summit_ft <- summit_data |>
  left_join(filefjell_data_clean |>
              select(summit, year) |>
              distinct() |>
              mutate(survey = case_when(year == 1972 ~ "first",
                                        year %in% c(2008, 2009) ~ "second",
                                        year %in% c(2024, 2025) ~ "third")) |>
              pivot_wider(names_from = survey, values_from = year),
            by = "summit") |>
  mutate(across(c(correct_height, first:third), ~ as.character(.x))) |>
  rename("Summit" = summit,
         "Elevation" = correct_height,
         "Area (ha)" = summit_hectare,
         "Bedrock" = bedrock,
         "First survey" = first,
         "Second survey" = second,
         "Third survey" = third) |>
  clean_ft() |>
  align(part = "body", j = c(2, 3, 5, 6, 7), align = "center") |>
  autofit()
summit_ft
summit_ft |> save_as_image(path = "Results/Summit_overview.png", res = 300)




# Turnover. Alluvial figure----

status <- filefjell_simplified |>
    select(year, summit, species) |>
    mutate(presence = 1) |>
    pivot_wider(names_from = year, values_from = presence, values_fill = 0)

flows_all <- status |>
  mutate(type12 = case_when(first  == 1 & second == 1 ~ "Persisted",
                            first  == 1 & second == 0 ~ "Lost",
                            first  == 0 & second == 1 ~ "New",
                            first  == 0 & second == 0 ~ "Absent"),
         type23 = case_when(second == 1 & third  == 1 ~ "Persisted",
                            second == 1 & third  == 0L ~ "Lost",
                            second == 0 & third  == 1 ~ "New",
                            second == 0 & third == 0 ~ "Absent")) |>
  count(first, second, third, type12, type23, name = "n") |>
  mutate(flow_id = row_number(),
         type12 = factor(type12, levels = c("Persisted","Lost","New","Absent")),
         type23 = factor(type23, levels = c("Persisted","Lost","New","Absent")))

lodes_12 <- flows_all |>
  select(flow_id, n, first, second, type12) |>
  ggalluvial::to_lodes_form(axes    = c("first","second"),
                            key     = "x",
                            value   = "stratum",
                            id      = "flow_id",
                            weight  = "n",
                            discern = TRUE) |>
  rename(survey = x) |>
  mutate(x = recode(survey, first = 1, second = 2.38),
         stratum = case_when(flow_id == 7 & survey == "first" ~ 7,
                             flow_id == 6 & survey == "first" ~ 6,
                             flow_id == 5 & survey == "first" ~ 5,
                             flow_id == 4 & survey == "first" ~ 4,
                             flow_id == 3 & survey == "first" ~ 3,
                             flow_id == 2 & survey == "first" ~ 2,
                             flow_id == 1 & survey == "first" ~ 1,
                             flow_id == 7 & survey == "second" ~ 7, # 7
                             flow_id == 6 & survey == "second" ~ 6, # 6
                             flow_id == 5 & survey == "second" ~ 3, # 3
                             flow_id == 4 & survey == "second" ~ 2, # 2
                             flow_id == 3 & survey == "second" ~ 5, # 5
                             flow_id == 2 & survey == "second" ~ 4, # 4
                             flow_id == 1 & survey == "second" ~ 1), # 1
         stratum = factor(stratum, levels = c(1, 2, 3, 4, 5, 6, 7)),
         type    = type12,
         n = case_when(type == "New" & survey == "first" ~ 0,
                       type == "Absent" ~ 0,
                       TRUE ~ n),
         alpha_group = case_when(flow_id %in% c(2, 4, 6) ~ "light",
                                 TRUE ~ "dark")) |>
  select(flow_id, x, stratum, n, type, alpha_group)

lodes_23 <- flows_all |>
  select(flow_id, n, second, third, type23) |>
  ggalluvial::to_lodes_form(axes = c("second","third"),
                            key = "x",
                            value = "stratum",
                            id = "flow_id",
                            weight = "n",
                            discern = TRUE) |>
  rename(survey = x) |>
  mutate(x = recode(survey, second = 2.38, third = 3),
         stratum = case_when(flow_id == 7 & survey == "second" ~ 7, # 7
                             flow_id == 6 & survey == "second" ~ 6, # 6
                             flow_id == 5 & survey == "second" ~ 3, # 3
                             flow_id == 4 & survey == "second" ~ 2, # 2
                             flow_id == 3 & survey == "second" ~ 5, # 5
                             flow_id == 2 & survey == "second" ~ 4, # 4
                             flow_id == 1 & survey == "second" ~ 1, # 1
                             flow_id == 7 & survey == "third" ~ 7, # 7
                             flow_id == 6 & survey == "third" ~ 3, # 3
                             flow_id == 5 & survey == "third" ~ 5, # 5
                             flow_id == 4 & survey == "third" ~ 1, # 1
                             flow_id == 3 & survey == "third" ~ 6, # 6
                             flow_id == 2 & survey == "third" ~ 2, # 2
                             flow_id == 1 & survey == "third" ~ 4), # 4
         stratum = factor(stratum, levels = c(1, 2, 3, 4, 5, 6, 7)),
         type = type23,
         n = case_when(flow_id == 1 & survey == "second" ~ 0,
                       TRUE ~ n),
         alpha_group = case_when(flow_id %in% c(3, 5) ~ "light",
                                 TRUE ~ "dark")) |>
  select(flow_id, x, stratum, n, type, alpha_group)

strata <- bind_rows(
  status |>
    mutate(x = 1,
           stratum = factor(first, levels = c(0, 1))) |>
    count(x, stratum, name = "n"),
  status |>
    mutate(x = 2.38,
           stratum = factor(second, levels = c(0, 1))) |>
    count(x, stratum, name = "n"),
  status |>
    mutate(x = 3,
           stratum = factor(third, levels = c(0, 1))) |>
    count(x, stratum, name = "n")) |>
  mutate(strata_fill = ifelse(stratum == 1, "#4DAF4A", "white"))

species_records_manually <- tribble(
    ~x,     ~y,      ~label,
  1.690,  820.0, "Period 1",
  2.690,  820.0, "Period 2",
  0.930,  188.0,      "376",
  2.310,  177.5,      "355",
  2.310,  496.0,      "282",
  2.325,  680.0,       "21",
  3.070,  170.5,      "341",
  3.070,  453.5,      "225",
  3.055,  573.0,       "14",
  3.070,  639.0,      "118",
  3.055,  705.0,       "14",
  3.055,  740.5,       "57",
  3.040,  776.0,        "7"
)

species_records_plot <- ggplot() +
  geom_flow(data = lodes_12,
            aes(x = x, alluvium = flow_id, y = n, stratum = stratum, fill = type),
            alpha = 0.9,
            width = 0,
            knot.pos = 0.35,
            curve_type = "cubic") +
  geom_flow(data = lodes_23,
            aes(x = x, alluvium = flow_id, y = n, stratum = stratum, fill = type, alpha = alpha_group),
            width = 0,
            knot.pos = 0.35,
            curve_type = "cubic") +
  geom_stratum(data  = strata |> filter(x %in% c(1, 2.38)),
               aes(x = x, y = n, stratum = stratum),
               alpha = 1,
               width = 0) +
  geom_stratum(data  = strata |> filter(x %in% c(2.38, 3)),
               aes(x = x, y = n, stratum = stratum),
               alpha = 1,
               width = 0) +
  geom_text(data = species_records_manually,
            aes(x = x, y = y, label = label),
            family = "serif", size = 4) +
  scale_fill_manual(values = alluvial_palette,
                    name   = "Between surveys") +
  scale_alpha_manual(values = c("light" = 0.6, "dark" = 0.9)) +
  guides(alpha = "none") +
  scale_x_continuous(breaks = c(1, 2.38, 3),
                     labels = c("1972", "2008/09", "2024/25"),
                     expand = c(0.08, 0.05)) +
  labs(x = NULL,
       y = "Number of species records") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.y = element_markdown(margin = margin(r = 10)),
        legend.position = "top",
        text = element_text(size = 14, family = "serif"))
species_records_plot
species_records_plot |> ggsave(filename = "Species_records.png", path = "Results", width = 20, height = 15, units = "cm")




# Richness----

## Data

richness <- filefjell_simplified |>
  summarise(.by = c(year, summit, specialisation), richness = n())


## Overview increases and decreases

richness_overview_specialisation <- richness |>
  pivot_wider(names_from = year, values_from = richness, values_fill = 0) |>
  mutate(change1 = case_when(second > first ~ "Increase",
                             second == first ~ "No change",
                             second < first ~ "Decrease"),
         change2 = case_when(third > second ~ "Increase",
                             third == second ~ "No change",
                             third < second ~ "Decrease")) |>
  arrange(summit)

rich_spe_ft <- richness_overview_specialisation |>
  select(!first:third) |>
  pivot_wider(names_from = specialisation, values_from = change1:change2) |>
  mutate(period1 = case_when(change1_alpine == "Increase" & change1_generalist == "Increase" ~ "++",
                             change1_alpine == "Increase" & change1_generalist == "Decrease" ~ "+-",
                             change1_alpine == "Decrease" & change1_generalist == "Increase" ~ "-+",
                             change1_alpine == "Decrease" & change1_generalist == "Decrease" ~ "--"),
         period2 = case_when(change2_alpine == "Increase" & change2_generalist == "Increase" ~ "++",
                             change2_alpine == "Increase" & change2_generalist == "Decrease" ~ "+-",
                             change2_alpine == "No change" & change2_generalist == "Increase" ~ "0+",
                             change2_alpine == "Decrease" & change2_generalist == "Increase" ~ "-+",
                             change2_alpine == "Decrease" & change2_generalist == "Decrease" ~ "--")) |>
  select(period1, period2) |>
  pivot_longer(cols = period1:period2, names_to = "period", values_to = "change") |>
  summarise(.by = c(period, change), total = n()) |>
  pivot_wider(names_from = period, values_from = total, values_fill = 0) |>
  mutate(alpine = str_sub(change, end = 1L),
         generalist = str_sub(change, start = 2L, end = 2L),
         alpine = case_when(alpine == "+" ~ "Increase",
                            alpine == "0" ~ "No change",
                            alpine == "-" ~ "Decrease"),
         alpine = factor(alpine, levels = c("Increase", "No change", "Decrease")),
         generalist = case_when(generalist == "+" ~ "Increase",
                                generalist == "0" ~ "No change",
                                generalist == "-" ~ "Decrease"),
         generalist = factor(generalist, levels = c("Increase", "Decrease"))) |>
  select(!change) |>
  relocate(alpine, generalist) |>
  arrange(alpine, generalist) |>
  rename("Specialists" = alpine,
         "Generalists" = generalist,
         "1972-2008/09" = period1,
         "2008/09-2024/25" = period2) |>
  clean_ft() |>
  hline(i = c(2, 3), border = officer::fp_border(color = "grey")) |>
  merge_v(j = 1) |>
  align(j = 3:4, align = "center") |>
  fontsize(part = "header", size = 12) |>
  fontsize(part = "body", size = 11) |>
  autofit()
rich_spe_ft
rich_spe_ft |> save_as_image(path = "Results/Richness_specialisation_number_summits.png", res = 300)


## Rate

richness_rate <- richness |>
  pivot_wider(names_from = year, values_from = richness, values_fill = 0) |>
  mutate(period1 = second - first,
         period2 = third - second) |>
  select(!c(first:third)) |>
  pivot_longer(cols = c(period1, period2), names_to = "period", values_to = "change") |>
  left_join(summit_periods, by = c("summit", "period")) |>
  mutate(dec_rate = 10 * change / time)

richness_rate |>
  mutate(per_spe = paste0(period, specialisation)) |>
  ggplot(aes(x = per_spe, y = dec_rate)) +
  geom_violin() +
  labs(title = " elevations by Year",
       x = "Year",
       y = "Vertical elevation to Top (meters)") +
  theme_minimal()

richness_rate |>
  ggplot() +
  geom_histogram(aes(x = dec_rate), binwidth = 1)


## Analysis

richrate_mod <- glmmTMB(
  dec_rate ~
    period * specialisation + (1 | summit),
  family = gaussian,
  data = richness_rate)

richrate_mod |> model_diagnosis() # No problems
richrate_mod |> model_homoscedasticity() # No problems. But period is in the limit. We compare models
richrate_mod |> summary()


# We check for greater variance in the second period

richrate_modh <- glmmTMB(
  dec_rate ~
    period * specialisation + (1 | summit),
  dispformula = ~period,
  family = gaussian,
  data = richness_rate)
richrate_modh |> model_diagnosis() # No problems
richrate_modh |> model_homoscedasticity() # No problems
richrate_modh |> summary() # Periods differ in their variances
anova(richrate_mod, richrate_modh) # Better with dispformula

richrate_results <- richrate_modh |>
  mod_summary()
richrate_results


# Across groups

richrate_period1 <- richrate_modh |>
  emmeans(~ period * specialisation) |>
  contrast(method = list(total_p1 = c(1, 0, 1, 0))) |>
  tidy(conf.int = TRUE)
richrate_period1 # 5.48 Total rate in period 1

richrate_period2 <- richrate_modh |>
  emmeans(~ period * specialisation) |>
  contrast(method = list(total_p2 = c(0, 1, 0, 1))) |>
  tidy(conf.int = TRUE)
richrate_period2 # 3.11 total rate in period 2

richrate_periods <- richrate_modh |>
  emmeans(~ period * specialisation) |>
  contrast(method = list(total_p2 = c(-1, 1, -1, 1))) |>
  tidy(conf.int = TRUE)
richrate_periods # In the second period richness rate was slower, -2.37. p = 0.085


richrate_specialists <- richrate_modh |>
  emmeans(~ period * specialisation) |>
  contrast(method = list(total_alp = c(1, 1, 0, 0))) |>
  tidy(conf.int = TRUE)
richrate_specialists # 3.95

richrate_generalists <- richrate_modh |>
  emmeans(~ period * specialisation) |>
  contrast(method = list(total_gen = c(0, 0, 1, 1))) |>
  tidy(conf.int = TRUE)
richrate_generalists # 4.64

richrate_class <- richrate_modh |>
  emmeans(~ period * specialisation) |>
  contrast(method = list(total_gen = c(-1, -1, 1, 1))) |>
  tidy(conf.int = TRUE)
richrate_class # Generalists increased at a higher rate (0.69). p value = 0.61




# Turnover----

## Data

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
  mutate(dec_rate = 10 * value / time)

lost_rate <- turnover |>
  filter(type == "lost") |>
  left_join(summit_periods, by = c("summit", "period")) |>
  mutate(dec_rate = 10 * value / time)


## New species

new_rate |>
  ggplot(aes(x = period, y = dec_rate)) +
  geom_violin()

new_rate |>
  ggplot() +
  geom_histogram(aes(x = dec_rate), binwidth = 0.5)

new_mod <- glmmTMB(
  dec_rate ~
    period * specialisation + (1 | summit),
  family = gaussian,
  data = new_rate)

new_mod |> model_diagnosis() # No problems
new_mod |> model_homoscedasticity() # No problems
new_mod |> summary()


# We check for greater variance in the second period

new_modh <- glmmTMB(
  dec_rate ~
    period * specialisation + (1 | summit),
  dispformula = ~period,
  family = gaussian,
  data = new_rate)
new_modh |> model_diagnosis() # No problems
new_modh |> model_homoscedasticity() # No problems
new_modh |> summary()
anova(new_mod, new_modh) # Better with dispformula

new_results <- new_modh |>
  mod_summary()
new_results



## Lost species

lost_rate |>
  ggplot(aes(x = period, y = dec_rate)) +
  geom_violin()

lost_rate |>
  ggplot() +
  geom_histogram(aes(x = dec_rate))

lost_mod <- glmmTMB(
  dec_rate ~
    period * specialisation + (1 | summit),
  family = gaussian,
  data = lost_rate)

lost_mod |> model_diagnosis() # Uniformity and quantiles
lost_mod |> model_homoscedasticity() # period
lost_mod |> summary()


# We check for greater variance in the second period

lost_modh <- glmmTMB(
  dec_rate ~
    period * specialisation + (1 | summit),
  dispformula = ~period,
  family = gaussian,
  data = lost_rate)
lost_modh |> model_diagnosis() # Outliers
lost_modh |> model_homoscedasticity() # No problems
lost_modh |> summary()
anova(lost_mod, lost_modh) # Better with dispformula

lost_results <- lost_modh |>
  mod_summary()
lost_results



# Across groups

lostrate_period1 <- lost_modh |>
  emmeans(~ period * specialisation) |>
  contrast(method = list(total_p1 = c(1, 0, 1, 0))) |>
  tidy(conf.int = TRUE)
lostrate_period2 <- lost_modh |>
  emmeans(~ period * specialisation) |>
  contrast(method = list(total_p2 = c(0, 1, 0, 1))) |>
  tidy(conf.int = TRUE)
lostrate_period1
lostrate_period2


lostrate_specialists <- lost_modh |>
  emmeans(~ period * specialisation) |>
  contrast(method = list(total_alp = c(1, 1, 0, 0))) |>
  tidy(conf.int = TRUE)
lostrate_generalists <- lost_modh |>
  emmeans(~ period * specialisation) |>
  contrast(method = list(total_gen = c(0, 0, 1, 1))) |>
  tidy(conf.int = TRUE)
lostrate_specialists
lostrate_generalists



## Loss of original species

original_lost <- filefjell_simplified |>
  mutate(presence = ifelse(!is.na(distance), 1, 0)) |>
  select(!distance) |>
  pivot_wider(names_from = year, values_from = presence) |>
  filter(!is.na(first)) |>
  mutate(lost1 = ifelse(!is.na(first) & is.na(second), 1, 0),
         lost2 = ifelse(!is.na(second) & is.na(third), 1, 0)) |>
  select(!c(first:third)) |>
  summarise(.by = c(summit, specialisation),
            period1 = sum(lost1),
            period2 = sum(lost2)) |>
  pivot_longer(cols = c(period1, period2),
               names_to = c("period"),
               values_to = "value") |>
  left_join(summit_periods, by = c("summit", "period")) |>
  mutate(dec_rate = 10 * value / time)

original_lost |>
  ggplot(aes(x = period, y = dec_rate)) +
  geom_violin()

original_lost |>
  ggplot() +
  geom_histogram(aes(x = dec_rate), binwidth = 0.1)

orilost_mod <- glmmTMB(
  dec_rate ~
    period * specialisation + (1 | summit),
  family = gaussian,
  data = original_lost)

orilost_mod |> model_diagnosis() # No problems
orilost_mod |> model_homoscedasticity() # No problems (though period on the edge)
orilost_mod |> summary()

orilost_modh <- glmmTMB(
  dec_rate ~
    period * specialisation + (1 | summit),
  dispformula = ~period,
  family = gaussian,
  data = original_lost)
orilost_modh |> model_diagnosis() # No problems
orilost_modh |> model_homoscedasticity() # No problems
orilost_modh |> summary()
anova(orilost_mod, orilost_modh) # Better with dispformula

orilost_results <- orilost_modh |>
  mod_summary()
orilost_results

orilost_model_ft <- orilost_results$model_df |>
  select(!c(SE, Statistic)) |>
  mutate(p_value1 = ifelse(p_value > 0.001, round(p_value, 3), NA) |> as.character(),
         p_value2 = ifelse(p_value < 0.001, "< 0.001", NA) |> as.character(),
         across(where(is.numeric), ~ round(., 2))) |>
  select(!p_value) |>
  mutate(p_value = coalesce(p_value1, p_value2)) |>
  select(!c(p_value1, p_value2)) |>
  clean_ft() |>
  set_header_labels(p_value = "p value",
                    CI_lower = "CI Lower",
                    CI_upper = "CI Upper") |>
  compose(part = "body", j = 1, 
          value = as_paragraph(c("Intercept", "Period 2", "Generalist", "Period 2 : Generalist"))) |>
  bold(i = ~ ((CI_lower * CI_upper) > 0)) |>
  hline(i = c(1, 3)) |>
  align(part = "all", j = 2:5, align = "center") |>
  fontsize(part = "header", size = 12) |>
  fontsize(part = "body", size = 11) |>
  autofit()
orilost_model_ft
orilost_model_ft |> save_as_image(path = "Results/Loss_original_species.png", res = 300)

orilost_results$emmeans_ft |> delete_columns(j = c(4, 5, 8))
orilost_results$contrast_ft |> delete_columns(j = c(3, 4, 7))


# Across groups

orilost_period1 <- orilost_modh |>
  emmeans(~ period * specialisation) |>
  contrast(method = list(total_p1 = c(1, 0, 1, 0))) |>
  tidy(conf.int = TRUE)
orilost_period2 <- orilost_modh |>
  emmeans(~ period * specialisation) |>
  contrast(method = list(total_p2 = c(0, 1, 0, 1))) |>
  tidy(conf.int = TRUE)
orilost_period1
orilost_period2


orilost_specialists <- orilost_modh |>
  emmeans(~ period * specialisation) |>
  contrast(method = list(total_alp = c(1, 1, 0, 0))) |>
  tidy(conf.int = TRUE)
orilost_generalists <- orilost_modh |>
  emmeans(~ period * specialisation) |>
  contrast(method = list(total_gen = c(0, 0, 1, 1))) |>
  tidy(conf.int = TRUE)
orilost_specialists
orilost_generalists




# New and lost species----

new_lost <- filefjell_simplified |>
  pivot_wider(names_from = year, values_from = distance) |>
  mutate(status = case_when(!is.na(first) & !is.na(second) & !is.na(third) ~ "persisted",
                            !is.na(first) & !is.na(second) & is.na(third) ~ "lost_2",
                            !is.na(first) & is.na(second) & !is.na(third) ~ "reappeared",
                            !is.na(first) & is.na(second) & is.na(third) ~ "lost_1",
                            is.na(first) & !is.na(second) & !is.na(third) ~ "new_1",
                            is.na(first) & !is.na(second) & is.na(third) ~ "redisappeared",
                            is.na(first) & is.na(second) & !is.na(third) ~ "new_2")) |>
  summarise(.by = c("species", "specialisation", "functional", "status"), total = n()) |>
  mutate(status = factor(status, levels = c("persisted", "new_1", "new_2", "reappeared", "redisappeared", "lost_1", "lost_2"))) |>
  arrange(status) |>
  pivot_wider(names_from = status, values_from = total, values_fill = 0) |>
  arrange(species)

winners <- new_lost |>
  filter((new_1 + new_2) > 6 & (lost_1 + lost_2) < 2)

winners_period2 <- new_lost |>
  filter(new_2 > 2)

losers <- new_lost |>
  filter((lost_1 + lost_2) > 1)

ambivalents <- new_lost |>
  filter((reappeared + redisappeared) > 2)

stagnants <- new_lost |>
  filter((new_1 + new_2) < 2)

disappeared <-  new_lost |>
  filter((lost_1 + lost_2) > 0)


## Tables

winners_ft <- winners |>
  select(species, specialisation, persisted:new_2) |>
  arrange(specialisation) |>
  mutate(dispersal = case_when(species == "Ant_dio" ~ "Wind",
                               species == "Ath_dis" ~ "Wind",
                               species == "Ave_fle" ~ "Wind",
                               species == "Cry_cri" ~ "Wind",
                               species == "Eri_ang" ~ "Wind",
                               species == "Eri_sch" ~ "Wind",
                               species == "Mic_ste" ~ "Water / Wind",
                               species == "Oma_nor" ~ "Wind",
                               species == "Ran_pyg" ~ "Water / Wind",
                               species == "Vac_myr" ~ "Birds",
                               species == "Vac_uli" ~ "Birds"),
         species = case_when(species == "Ant_dio" ~ "Antennaria dioica",
                             species == "Ath_dis" ~ "Athyrium distentifolium",
                             species == "Ave_fle" ~ "Avenella flexuosa",
                             species == "Cry_cri" ~ "Cryptogramma crispa",
                             species == "Eri_ang" ~ "Eriophorum angustifolium",
                             species == "Eri_sch" ~ "Eriophorum scheuchzeri",
                             species == "Mic_ste" ~ "Micranthes stellaris",
                             species == "Oma_nor" ~ "Omalotheca norvegica",
                             species == "Ran_pyg" ~ "Ranunculus pygmaeus",
                             species == "Vac_myr" ~ "Vaccinium myrtillus",
                             species == "Vac_uli" ~ "Vaccinium uliginosum"),
         specialisation = ifelse(specialisation == "alpine", "Specialist", "Generalist")) |>
  relocate(c(dispersal, new_1, new_2), .after = specialisation) |>
  clean_ft() |>
  set_header_labels(species = "Species",
                    specialisation = "Specialisation\nclass",
                    dispersal = "Primary dispersal\nmechanism",
                    new_1 = "# Summits new\n1972\u20132008/09",
                    new_2 = "# Summits new\n2008/09\u20132024/25",
                    persisted = "# Summits persisted\n1972\u20132024/25") |>
  fontsize(part = "body", size = 14) |>
  italic(part = "body", j = 1) |>
  hline(i = 4) |>
  align(part = "all", j = 4:6, align = "center") |>
  autofit() |>
  height(part = "header", height = 1) |>
  hrule(part = "header", rule = "exact") |>
  height(part = "body", height = 0.4) |>
  hrule(part = "body", rule = "exact") |>
  line_spacing(part = "header", space = 2)
winners_ft
winners_ft |> save_as_image(path = "Results/Winners.png", res = 300)


disappeared_ft<- disappeared |>
  mutate(species = case_when(species == "Alc_glo" ~ "Alchemilla glomerulans",
                             species == "Ant_odo" ~ "Anthoxantum odoratum",
                             species == "Arc_uva" ~ "Arctostaphylos uva-ursi",
                             species == "Cer_alp" ~ "Cerastium alpinum",
                             species == "Cer_cer" ~ "Cerastium cerastoides",
                             species == "Cha_ang" ~ "Chamaenerion angustifolium",
                             species == "Che_bif" ~ "Cherleria biflora",
                             species == "Dac_vir" ~ "Dactylorhiza viridis",
                             species == "Dip_alp" ~ "Diphasiastrum alpinum",
                             species == "Epi_ana" ~ "Epilobium anagallidifolium",
                             species == "Eri_vag" ~ "Eriophorum vaginatum",
                             species == "Poa_arc" ~ "Poa arctica",
                             species == "Poa_jem" ~ "Poa x jemtlandica",
                             species == "Pot_cra" ~ "Potentilla crantzii",
                             species == "Rum_ace" ~ "Rumex acetosa",
                             species == "Sil_wah" ~ "Silene wahlbergella",
                             species == "Vis_alp" ~ "Viscaria alpina"),
         specialisation = ifelse(specialisation == "generalist", "Generalist", "Specialist"),
         functional = case_when(functional == "forb" ~ "Forb",
                                functional == "graminoid" ~ "Graminoid",
                                functional == "dwarf_shrub" ~ "Dwarf shrub",
                                functional == "pteridophyte" ~ "Pteridophyte")) |>
  relocate(c(lost_1, lost_2), .after = functional) |>
  rename(Species = species, Specialisation = specialisation, Functional = functional, 'Lost 2008/09' = lost_1, 'Lost 2024/25' = lost_2, "Persisted" = persisted, "New 2008/09" = new_1, "New 2024/25" = new_2, "Reappeared" = reappeared, "Re-disappeared" = redisappeared) |>
  clean_ft() |>
  italic(part = "body", j = 1) |>
  align(part = "body", j = -(1:3), align = "center") |>
  autofit()
disappeared_ft
disappeared_ft |> save_as_image(path = "Results/Species_disappearing.png")




# Uppermost occurrence----

## Data

# Considering only species for which we have data (present two sampling times in a row)

altitude_rate <- filefjell_simplified |>
  pivot_wider(names_from = year, values_from = distance) |>
  mutate(period1 = (second - first) * (-1),
         period2 = (third - second) * (-1)) |> # Change the sign so that a positive value indicates upwards movement
  select(!c(first:third)) |>
  pivot_longer(cols = c(period1, period2), names_to = "period", values_to = "change") |>
  filter(!is.na(change)) |>
  left_join(summit_periods, by = c("summit", "period")) |>
  mutate(dec_rate = 10 * change / time)


## Frequentist analysis

altitude_rate |>
  ggplot() +
  geom_histogram(aes(x = dec_rate))

altrate_mod <- glmmTMB(
  dec_rate ~
    period * specialisation + (1 | summit) + (1 | species),
  dispformula = ~period,
  family = t_family(),
  data = altitude_rate)

altrate_mod |> model_diagnosis()
altrate_mod |> model_homoscedasticity()
altrate_mod |> summary()
# No distributions I try get closely to fitting. I try bayesian


## Bayesian analysis

# 1. Choosing weakly informative priors

# Response scale. The rate of elevation change will not go beyond -2.133 to 2.133 (32 metres in 1.5 decades, highest possible change in shortest time span between surveys)

## Gaussian

priors_g1 <- c(
  prior(normal(0, 5), class = "Intercept"), # Since there was no change in 1972–2008/09 (Odland article) we start from 0
  prior(normal(0, 5), class = "b"), # fixed effects
  prior(exponential(0.3), class = "sigma"), # residual SD
  prior(exponential(0.3), class = "sd") # all RE SDs (summit, species)
)

altrate_gbay1 <- brm(
  bf(dec_rate ~
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
  mutate(in_range = between(.prediction, -21.33, 21.33))

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
# Between 97 and 99% of prior predictions fall within [-21.33, 21.33]. Good priors


## Student t

priors_t1 <- c(
  priors_g1,
  prior(gamma(2, 0.1), class = "nu")  # mean ~20, weak tails
)

altrate_tbay1 <- brm(
  bf(dec_rate ~
       period * specialisation + (1|summit) + (1|species)),
  family = student(),
  prior = priors_t1,
  sample_prior = "only",
  data = altitude_rate,
  chains = 4, iter = 2000,  seed = 811
)

altrate_tpred1 <- altrate_tbay1 %>%
  add_predicted_draws(newdata = altrate_expla1, re_formula = NA) %>%
  mutate(in_range = between(.prediction, -21.33, 21.33))

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
# Between 97 and 99% of prior predictions fall within [-21.33, 21.33]. Good priors


# 2. Comparing gaussian and student t

altrate_gbay2 <- brm(
  bf(dec_rate ~
       period * specialisation + (1|summit) + (1|species)),
  family = gaussian(),
  prior = priors_g1,
  data = altitude_rate,
  chains = 4, iter = 4000, seed = 811,
  control = list(adapt_delta = 0.95)
)
altrate_gloo2 <- loo(altrate_gbay2, save_psis = TRUE)  # PSIS-LOO

# Student-t, sigma constant
altrate_tbay2 <- brm(
  bf(dec_rate ~
       period * specialisation + (1|summit) + (1|species)),
  family = student(),
  prior = priors_t1,
  data = altitude_rate,
  chains = 4, iter = 4000, seed = 811,
  control = list(adapt_delta = 0.95)
)
altrate_tloo2 <- loo(altrate_tbay2, save_psis = TRUE)

# Side-by-side comparison
loo_compare(altrate_gloo2, altrate_tloo2)

table(cut(altrate_gloo2$diagnostics$pareto_k, c(-Inf, 0.5, 0.7, 1, Inf)))
table(cut(altrate_tloo2$diagnostics$pareto_k, c(-Inf, 0.5, 0.7, 1, Inf)))

pp_check(altrate_gbay2, type = "ecdf_overlay_grouped", group = "period") + xlim(-22, 22)
pp_check(altrate_tbay2, type = "ecdf_overlay_grouped", group = "period") + xlim(-22, 22)
pp_check(altrate_gbay2, type = "dens_overlay_grouped", group = "period") + xlim(-22, 22)
pp_check(altrate_tbay2, type = "dens_overlay_grouped", group = "period") + xlim(-22, 22)

altrate_rates <- altitude_rate$dec_rate
altrate_grates2 <- posterior_predict(altrate_gbay2, draws = 1000)
altrate_trates2 <- posterior_predict(altrate_tbay2, draws = 1000)

ppc_loo_pit_qq(altrate_rates, altrate_grates2, psis_object = altrate_gloo2$psis_object)
ppc_loo_pit_qq(altrate_rates, altrate_trates2, psis_object = altrate_tloo2$psis_object)

# We choose student-t
altrate_tbay2 |> summary()
# We have an extremely low nu. We explore what this means
# i.e.: are there actually such heavy tails, or is it indicative of missing variance structure?


# 3.Seeing whether there's heteroskedasticity, and if the extreme nu can be caused by it

# I create the priors, now without a prior for sigma ad we'll evaluate the effect of the fixed effects on it
priors_t3 <- c(
  prior(normal(0, 5), class = "Intercept"),
  prior(normal(0, 5), class = "b"),
  prior(exponential(0.3), class = "sd"),
  prior(gamma(2, 0.1), class = "nu")
)

altrate_tbay3per <- brm(
  bf(dec_rate ~
       period * specialisation + (1|summit) + (1|species),
     sigma ~ period),
  family = student(),
  prior = priors_t3,
  data = altitude_rate,
  chains = 4, iter = 4000, seed = 811,
  control = list(adapt_delta = 0.95)
)

altrate_tbay3spe <- brm(
  bf(dec_rate ~
       period * specialisation + (1|summit) + (1|species),
     sigma ~ specialisation),
  family = student(),
  prior = priors_t3,
  data = altitude_rate,
  chains = 4, iter = 4000, seed = 811,
  control = list(adapt_delta = 0.95)
)

altrate_tbay3perspe <- brm(
  bf(dec_rate ~
       period * specialisation + (1|summit) + (1|species),
     sigma ~ period + specialisation),
  family = student(),
  prior = priors_t3,
  data = altitude_rate,
  chains = 4, iter = 4000, seed = 811,
  control = list(adapt_delta = 0.95)
)

altrate_tbay3perxspe <- brm(
  bf(dec_rate ~
       period * specialisation + (1|summit) + (1|species),
     sigma ~ period * specialisation),
  family = student(),
  prior = priors_t3,
  data = altitude_rate,
  chains = 4, iter = 4000, seed = 811,
  control = list(adapt_delta = 0.95)
)

loo_compare(loo(altrate_tbay2),
            loo(altrate_tbay3per),
            loo(altrate_tbay3spe),
            loo(altrate_tbay3perspe),
            loo(altrate_tbay3perxspe))
posterior_summary(altrate_tbay2, variable = "nu")
posterior_summary(altrate_tbay3per, variable = "nu")
posterior_summary(altrate_tbay3spe, variable = "nu")
posterior_summary(altrate_tbay3perspe, variable = "nu")
posterior_summary(altrate_tbay3perxspe, variable = "nu")
# We keep the per model. period improves drastically the fit, and increases nu. specialisation does not do it. The interaction model is slightly better, but the improvement is tiny. It adds complexity without improving the results notably


# 4. Model validation

# altrate_tbay <- tar_read(altitude_bay)
altrate_tbay <- altrate_tbay3per

altrate_tper_loo <- loo(altrate_tbay, save_psis = TRUE)
table(cut(altrate_tper_loo$diagnostics$pareto_k, c(-Inf, 0.5, 0.7, 1, Inf))) # All good

altrate_tbay |> summary()
# Rhat = 1.00 for all parameters
# Bulk and Tail Effective sample size > 1000 for all parameters
# No divergences
altrate_tbay |> plot()

pp_check(altrate_tbay, type="dens_overlay_grouped", group="period") + xlim(-21.33, 21.33) # Looks good
pp_check(altrate_tbay, type="ecdf_overlay_grouped", group="period") + xlim(-21.33, 21.33) # Looks good


altrate_tbay_rates <- altitude_rate$dec_rate
altrate_tbay_pred <- posterior_predict(altrate_tbay, draws = 1000)
ppc_loo_pit_qq(altrate_tbay_rates,
               altrate_tbay_pred,
               psis_object = altrate_tper_loo$psis_object)
# The model slightly overestimates tail heaviness, but is well-calibrated for the central mass

altrate_tbay_r2 <- r2_bayes(altrate_tbay)
altrate_tbay_r2 # Neither the fixed effects nor the random effects explain much


# 5. Post hoc

altrate_results <- altrate_tbay |>
  mod_summary()
altrate_results

altrate_period1 <- altrate_tbay |>
  emmeans(~ period * specialisation) |>
  contrast(method = list(total_p1 = c(1, 0, 1, 0))) |>
  tidy(conf.int = TRUE)
altrate_period1

altrate_period2 <- altrate_tbay |>
  emmeans(~ period * specialisation) |>
  contrast(method = list(total_p2 = c(0, 1, 0, 1))) |>
  tidy(conf.int = TRUE)
altrate_period2

altrate_specialists <- altrate_tbay |>
  emmeans(~ period * specialisation) |>
  contrast(method = list(total_alp = c(1, 1, 0, 0))) |>
  tidy(conf.int = TRUE)
altrate_generalists <- altrate_tbay |>
  emmeans(~ period * specialisation) |>
  contrast(method = list(total_gen = c(0, 0, 1, 1))) |>
  tidy(conf.int = TRUE)
altrate_specialists
altrate_generalists




# Rate Results----

rate_emmeans <- richrate_results$emmeans_df |>
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
  mutate(Model = recode(Model, "Species richness" = "Species\nrichness", "Uppermost occurrence" = "Uppermost\noccurrence"))

rate_emmeans_ft <- rate_emmeans |>
  select(!c(df, SE, statistic)) |>
  arrange(Model, Period, Specialisation) |>
  mutate(p_value1 = ifelse(p_value > 0.001, round(p_value, 3), NA) |> as.character(),
         p_value2 = ifelse(p_value < 0.001, "< 0.001", NA) |> as.character(),
         across(where(is.numeric), ~ round(., 2))) |>
  select(!p_value) |>
  mutate(p_value = coalesce(p_value1, p_value2)) |>
  select(!c(p_value1, p_value2)) |>
  mutate(Period = case_when(Period == "period1" ~ "1972–2008/09",
                            Period == "period2" ~ "2008/09–2024/25"),
         Specialisation = case_when(Specialisation == "alpine" ~ "Specialist",
                                    Specialisation == "generalist" ~ "Generalist")) |>
  clean_ft() |>
  merge_v(j = "Model") |>
  merge_v(j = "Period") |>
  hline(i = c(4, 8, 12)) |>
  hline(i = c(2, 6, 10, 14), border = officer::fp_border(style = "dotted")) |>
  bold(i = ~ ((CI_lower * CI_upper) > 0), j = 3:7) |>
  set_header_labels(CI_lower = "CI Lower", CI_upper = "CI Upper", p_value = "p value") |>
  align(part = "all", j = 4:7, align = "center") |>
  autofit()
rate_emmeans_ft
rate_emmeans_ft |> save_as_image(path = "Results/Rates_emmeans_table.png", res = 300)


rate_contrasts <- richrate_results$contrast_df |>
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

rate_contrasts_ft <- rate_contrasts |>
  select(!c(df, SE, statistic)) |>
  mutate(p_value1 = ifelse(p_value > 0.001, round(p_value, 3), NA) |> as.character(),
         p_value2 = ifelse(p_value < 0.001, "< 0.001", NA) |> as.character(),
         across(where(is.numeric), ~ round(., 2))) |>
  select(!p_value) |>
  mutate(p_value = coalesce(p_value1, p_value2)) |>
  select(!c(p_value1, p_value2)) |>
  mutate(Contrast = case_when(Contrast == "1A-2A" ~ "Period 1 – Period 2. Specialists",
                              Contrast == "1G-2G" ~ "Period 1 – Period 2. Generalists",
                              Contrast == "1A-1G" ~ "Specialists – Generalists. Period 1",
                              Contrast == "2A-2G" ~ "Specialists – Generalists. Period 2")) |>
  separate_wider_delim(cols = Contrast, delim = ".", names = c("Contrast", "Group")) |>
  clean_ft() |>
  merge_v(j = "Model") |>
  merge_v(j = "Contrast") |>
  hline(i = c(4, 8, 12)) |>
  hline(i = c(2, 6, 10, 14), border = officer::fp_border(style = "dotted")) |>
  bold(i = ~ ((CI_lower * CI_upper) > 0), j = -1) |>
  set_header_labels(CI_lower = "CI Lower", CI_upper = "CI Upper", p_value = "p value") |>
  align(part = "all", j = -c(1:3), align = "center") |>
  autofit()
rate_contrasts_ft
rate_contrasts_ft |> save_as_image(path = "Results/Rates_contrasts_table.png", res = 300)


# Figure

richrate_figure <- richrate_results$emmeans_df |>
  gg_results() +
  scale_x_continuous(limits = c(-3.12, 5.2),
                     labels = NULL) +
  labs(x = NULL, y = adj_label["richness"]) +
  theme(plot.margin = margin(0, 0, 10, 0))

new_figure <- new_results$emmeans_df |>
  gg_results() +
  scale_x_continuous(limits = c(-3.12, 5.2),
                     labels = NULL) +
  labs(x = NULL, y = adj_label["new"]) +
  theme(plot.margin = margin(0, 0, 10, 0))

lost_figure <- lost_results$emmeans_df |>
  gg_results() +
  scale_x_continuous(limits = c(-3.12, 5.2)) +
  labs(x = "Rate (species&nbsp;summit<sup>-1</sup>&nbsp;decade<sup>-1</sup>)", y = adj_label["lost"]) +
  theme(axis.title.x = element_markdown()) +
  theme(plot.margin = margin(0, 0, 10, 0))

altrate_figure <- altrate_results$emmeans_df |>
  gg_results() +
  scale_x_continuous(limits = c(-0.78, 1.3)) +
  labs(x = "Rate (metres&nbsp;summit<sup>-1</sup>&nbsp;decade<sup>-1</sup>)", y = adj_label["altitude"]) +
  theme(axis.title.x = element_markdown(),
        plot.margin = margin(10, 0, 0, 0))

rates_figure <- ggarrange(
  plotlist = list(richrate_figure, new_figure, lost_figure, altrate_figure),
  ncol = 1,
  nrow = 4,
  align = "v",
  common.legend = TRUE,
  heights = c(1, 1, 1.35, 1.35)
)
rates_figure
rates_figure |> ggsave(file = "Results/Rates_emmeans_figure.png", width = 20, height = 15, units = "cm", bg = "white")




# Nature type----

## Habitat names

habitat_species <- tar_read(habitat_species_clean)
habitat_cover <- tar_read(habitat_cover)

new_species_2024_2025 <- filefjell_simplified |>
  pivot_wider(names_from = year, values_from = distance) |>
  filter(is.na(second) & !is.na(third)) |>
  mutate(status = "new") |>
  select(summit, species, status)

habitat_names_ft <- habitat_cover |>
  select(habitat) |>
  distinct() |>
  mutate(habitat = factor(habitat, levels = c("T1", "T27", "T13", "T14", "T7", "V6", "T3", "T22"))) |>
  arrange(habitat) |>
  mutate("NiN name" = case_when(habitat == "T1" ~ "Bare rock",
                                habitat == "T27" ~ "Boulder field",
                                habitat == "T13" ~ "Scree",
                                habitat == "T14" ~ "Ridge",
                                habitat == "T7" ~ "Snowbed",
                                habitat == "V6" ~ "Wet snowbed",
                                habitat == "T3" ~ "Alpine heath, leeside and tundra",
                                habitat == "T22" ~ "Alpine grassland and grass tundra"),
         "Habitat" = case_when(habitat == "T1" ~ "Bare rock",
                                  habitat == "T27" ~ "Boulder field",
                                  habitat == "T13" ~ "Scree",
                                  habitat == "T14" ~ "Ridge",
                                  habitat == "T7" ~ "Snowbed",
                                  habitat == "V6" ~ "Wet snowbed",
                                  habitat == "T3" ~ "Alpine heath",
                                  habitat == "T22" ~ "Alpine grassland")) |>
  rename("NiN code" = habitat) |>
  relocate(Habitat) |>
  clean_ft() |>
  fontsize(part = "header", size = 12) |>
  fontsize(part = "body", size = 11) |>
  autofit()
habitat_names_ft
habitat_names_ft |> save_as_image(path = "Results/NiN_names.png", res = 300)


## New species occurrences per habitat

hab_area <- habitat_species |>
  select(summit, habitat) |>
  distinct() |>
  full_join(habitat_cover, by = c("summit", "habitat")) |>
  mutate(habitat_decare = ifelse(is.na(habitat_decare), 0.25, habitat_decare)) |>
  summarise(.by = habitat, habitat_decare = sum(habitat_decare)) |>
  mutate(percentage = 100 * habitat_decare / sum(habitat_decare)) |>
  arrange(habitat)

hab_new <- habitat_species |>
  left_join(new_species_2024_2025, by = c("summit", "species")) |>
  filter(status == "new") |>
  select(summit, habitat, specialisation, species) |>
  arrange(summit, habitat, specialisation, species) |>
  summarise(.by = c(habitat, specialisation), total = n()) |>
  arrange(habitat, specialisation)

hab_new_prop <- hab_new |>
  pivot_wider(names_from = specialisation, values_from = total, values_fill = 0) |>
  right_join(hab_area, by = "habitat") |>
  mutate(across(alpine:generalist, ~ ifelse(is.na(.x), 0, .x)),
         total_nr = alpine + generalist,
         total_byha = total_nr / (habitat_decare / 10),
         alpine_byha = alpine / (habitat_decare / 10),
         generalist_byha = generalist / (habitat_decare / 10)) |>
  select(!habitat_decare) |>
  relocate(c(total_nr, total_byha), .after = habitat) |>
  relocate(alpine_byha, .after = alpine) |>
  relocate(generalist_byha, .after = generalist) |>
  mutate(habitat = factor(habitat, levels = c("T1", "T27", "T13", "T14", "T7", "V6", "T3", "T22"))) |>
  arrange(habitat)


## Table

hab_new_header <- tibble(
  col_keys = c("habitat",
               "total",
               "total_nr",
               "total_byha",
               "alpine",
               "alpine_nr",
               "alpine_byha",
               "generalist",
               "generalist_nr",
               "generalist_byha",
               "percentage"),
  header1 = c("Habitat",
              "Total new occurrences",
              "Total new occurrences",
              "Total new occurrences",
              "New specialists",
              "New specialists",
              "New specialists",
              "New generalists",
              "New generalists",
              "New generalists",
              "% Total\nsummit area"),
  header2 = c("Habitat",
              "Total new occurrences",
              "#",
              "Per_ area",
              "Specialists",
              "#",
              "Per_area",
              "Generalists",
              "#",
              "Per_area",
              "% Total\nsummit area")
)

hab_new_prop_ft <- hab_new_prop |>
  mutate(habitat = case_when(habitat == "T1" ~ "Bare rock",
                             habitat == "T27" ~ "Boulder field",
                             habitat == "T13" ~ "Scree",
                             habitat == "T14" ~ "Ridge",
                             habitat == "T7" ~ "Snowbed",
                             habitat == "V6" ~ "Wet snowbed",
                             habitat == "T3" ~ "Alpine heath",
                             habitat == "T22" ~ "Alpine grassland"),
         across(where(is.numeric), ~ round(., 1))) |>
  rename("alpine_nr" = "alpine", "generalist_nr" = "generalist") |>
  flextable() |>
  separate_header() |>
  set_header_df(hab_new_header) |>
  compose(part = "header", i = 2, j = c(3, 5, 7),
          value = as_paragraph("# / 10,000 m", as_sup("2"))) |>
  merge_h(part = "header", i = 1) |>
  merge_v(part = "header", j = c(1, 8)) |>
  bg(part = "header", bg = "black") |>
  color(part = "header", color = "white") |>
  bold(part = "header") |>
  bg(part = "body", bg = "white") |>
  color(part = "body", color = "black") |>
  align(part = "all", j = -1, align = "center") |>
  vline(j = c(1, 3, 5, 7)) |>
  flextable::font(part = "all", fontname = "Times New Roman") |>
  fontsize(part = "header", size = 13) |>
  fontsize(part = "body", size = 12) |>
  autofit()
hab_new_prop_ft
hab_new_prop_ft |> save_as_image(path = "Results/New_prop_habitats.png", res = 300)


## Figure

hab_new_prop_v <- hab_area |>
  select(habitat) |>
  distinct() |>
  crossing(specialisation = levels(hab_new$specialisation)) |>
  left_join(hab_new, by = c("habitat", "specialisation")) |>
  mutate(total = coalesce(total, 0)) |>
  left_join(hab_area, by = "habitat") |>
  mutate(total_byha = total / (habitat_decare / 10)) |>
  select(!habitat_decare) |>
  relocate(total_byha, .after = total) |>
  mutate(habitat = factor(habitat, levels = c("T1", "T27", "T13", "T14", "T7", "V6", "T3", "T22"))) |>
  arrange(habitat) |>
  mutate(across(where(is.numeric), ~ round(., 1)))

hab_perc_gg <- hab_area |>
  mutate(habitat = factor(habitat, levels = c("T1", "T27", "T13", "T14", "T7", "V6", "T3", "T22"))) |>
  ggplot() +
  geom_col(aes(x = habitat, y = percentage)) +
  labs(title = "Estimated % area across all summits",
       x = NULL,
       y = NULL) +
  scale_x_discrete(labels = adj_label) +
  scale_y_continuous(limits = c(0, 38.05)) +
  theme_minimal() +
  theme(text = element_text(size = 11, family = "serif"),
        plot.title = element_text(size = 9, hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

hab_new_total_gg <- hab_new_prop_v |>
  ggplot() +
  geom_col(aes(x = habitat, y = total, fill = specialisation)) +
  scale_fill_manual("Specialisation", values = colour_mapping$specialisation, labels = adj_label) +
  labs(title = "# New species occurrences",
       x = NULL,
       y = NULL) +
  scale_x_discrete(labels = adj_label) +
  scale_y_continuous(limits = c(0, 38.05)) +
  theme_minimal() +
  theme(text = element_text(size = 11, family = "serif"),
        plot.title = element_text(size = 9, hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(l = 9, r = 20, b = 4)),
        legend.box.margin = margin(l = 180))

hab_new_prop_gg <- hab_new_prop_v |>
  ggplot() +
  geom_col(aes(x = habitat, y = total_byha, fill = specialisation)) +
  scale_fill_manual("Specialisation", values = colour_mapping$specialisation, labels = adj_label) +
  labs(title = expression("# New species occurrences / 10 000 m"^2),
       x = NULL,
       y = NULL) +
  scale_x_discrete(labels = adj_label) +
  scale_y_continuous(limits = c(0, 19.025)) +
  theme_minimal() +
  theme(text = element_text(size = 11, family = "serif"),
        plot.title = element_text(size = 9, hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank())

hab_perc_new_gg <- ggarrange(hab_perc_gg, hab_new_total_gg, hab_new_prop_gg,
                           ncol = 3, align = "h", common.legend = TRUE)
hab_perc_new_gg
hab_perc_new_gg |> ggsave(filename = "New_species_per_habitat_figure_area.png", path = "Results", dpi = 300, width = 16.5, height = 10, units = "cm", bg = "white")

