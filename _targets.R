# Created by use_targets().
# Setting up the environment----

# Load packages required to define the pipeline:
library(targets)


# Target options:
tar_option_set(
  packages = c("tidyverse", "janitor", "forcats", "vegan", "glmmTMB", "lme4", "splines"),
  format = "rds", 
  seed = 811
)

# R scripts in the R/ folder with custom functions:
tar_source("Scripts/0_setup.R")

# List of targets:----
list(
  # Tidy files----
  tar_target(
    name = filefjell_visit_dates_file, 
    command = "data_raw/Visit_dates.csv", 
    format = "file"
  ), 
  tar_target(
    name = filefjell_visit_dates, 
    command = read_csv2(filefjell_visit_dates_file)
  ), 
  tar_target(
    name = filefjell_1972_2009_file, 
    command = "data_raw/Filefjell_1972_2009.csv", 
    format = "file"
  ), 
  tar_target(
    name = filefjell_1972_2009, 
    command = read_csv2(filefjell_1972_2009_file)
  ), 
  tar_target(
    name = filefjell_1972_tidy,
    command = filefjell_1972_2009 |> 
      filter(Year == 1972) |> 
      left_join(filefjell_visit_dates |> 
                  select(!c(Year2:Data2)), 
                by = c("Summit", "Year")) |> 
      pivot_longer(cols = !c(Summit:Year, Date, Recorder), 
                   names_to = "species", 
                   values_to = "distance", 
                   values_drop_na = TRUE) |> 
      data_tidying()
  ),
  tar_target(
    name = filefjell_2008_2009_tidy,
    command = filefjell_1972_2009 |> 
      filter(Year == 2009) |> 
      select(!Year) |> 
      left_join(filefjell_visit_dates |> 
                  filter(Year %in% c(2008, 2009)) |> 
                  select(!c(Year2:Data2)), 
                by = c("Summit")) |> 
      pivot_longer(cols = !c(Summit, Height, Year:Recorder), 
                   names_to = "species", 
                   values_to = "distance", 
                   values_drop_na = TRUE) |> 
      data_tidying()
  ),
  tar_target(
    name = filefjell_1972_2008_2009_tidy,
    command = filefjell_1972_tidy |> 
      rbind(filefjell_2008_2009_tidy) |> 
      arrange(desc(elevation), year, species)
  ),
  tar_target(
    name = filefjell_2024_file,
    command = "data_raw/Filefjell_2024.csv",
    format = "file"
  ),
  tar_target(
    name = filefjell_2024,
    command = read_csv2(filefjell_2024_file)
  ),
  tar_target(
    name = filefjell_2024_tidy,
    command = filefjell_2024 |> 
      data_tidying() |> 
      mutate(distance = elevation - altitude) |> 
      select(!altitude) |> 
      relocate(distance, .after = species) |> 
      mutate(main_type = str_extract(type, "[^C]*")) |> 
      relocate(main_type, .before = type)
  ),
  tar_target(
    name = filefjell_2025_file,
    command = "data_raw/Filefjell_2025.csv",
    format = "file"
  ),
  tar_target(
    name = filefjell_2025,
    command = read_csv2(filefjell_2025_file)
  ),
  tar_target(
    name = filefjell_2025_tidy,
    command = filefjell_2025 |> 
      data_tidying() |> 
      mutate(distance = elevation - altitude) |> 
      select(!altitude) |> 
      relocate(distance, .after = species) |> 
      mutate(main_type = str_extract(type, "[^C]*")) |> 
      relocate(main_type, .before = type)
  ),
  tar_target(
    name = filefjell_2024_2025_tidy,
    command = filefjell_2024_tidy |> 
      select(year:type) |> 
      rbind(filefjell_2025_tidy) |> 
      arrange(desc(elevation), year, species)
  ),
  tar_target(
    name = filefjell_summit_data_file,
    command = "data_raw/Summit_data.csv",
    format = "file"
  ),
  tar_target(
    name = filefjell_summit_data,
    command = read_csv2(filefjell_summit_data_file)
  ),
  tar_target(
    name = filefjell_summit_data_tidy,
    command = filefjell_summit_data |> 
      clean_names() |> 
      mutate(summit = str_replace_all(summit, " ", "_"))
  ),
  tar_target(
    name = filefjell_type_cover_file,
    command = "data_raw/Type_cover.csv",
    format = "file"
  ),
  tar_target(
    name = filefjell_type_cover,
    command = read_csv2(filefjell_type_cover_file)
  ),
  tar_target(
    name = filefjell_type_cover_tidy,
    command = filefjell_type_cover |> 
      clean_names() |> 
      relocate(year) |> 
      mutate(date = dmy(date), 
             recorder = str_replace_all(recorder, " \\+ ", "_"), 
             type = ifelse(type == "Naken berg", "T1", type)) |> 
      rename(weather = vaer, 
             cover = percentage) |> 
      mutate(weather = str_replace_all(weather, c(" \\+ " = "_", ", " = "_", " " = "_")))
  ),
  tar_target(
    name = filefjell_maintype_cover_tidy,
    command = filefjell_type_cover_tidy |> 
      mutate(main_type = str_extract(type, "[^C]*")) |> 
      relocate(main_type, .before = type) |> 
      summarise(.by = c(year, summit, date, recorder, main_type), 
                cover = sum(cover))
  ),
  tar_target(
    name = filefjell_species_file,
    command = "data_raw/Filefjell_species.csv",
    format = "file"
  ),
  tar_target(
    name = filefjell_species,
    command = read_csv2(filefjell_species_file)
  ),
  # Clean files----
  tar_target(
    name = filefjell_1972_2008_2009_clean,
    command = filefjell_summit_data_tidy |>
      mutate(elevation_correct = elevation) |>
      select(summit, elevation_correct) |>
      right_join(filefjell_1972_2008_2009_tidy, by = "summit") |>
      relocate(year) |> 
      select(!elevation) |>
      rename(elevation = elevation_correct) |>
      mutate(species = case_when(species == "Cer_lan" ~ "Cer_alp_lan", 
                                 species == "Jun_trif" ~ "Jun_tri",
                                 species == "Poa_x_jem" ~ "Poa_jem",
                                 species == "Sil_acu" ~ "Sil_aca",
                                 TRUE ~ species))
  ),
  tar_target(
    name = filefjell_2024_2025_clean,
    command = filefjell_summit_data_tidy |>
      mutate(elevation_correct = elevation) |>
      select(summit, elevation_correct) |>
      right_join(filefjell_2024_2025_tidy, by = "summit") |>
      relocate(year) |> 
      select(!elevation) |> 
      rename(elevation = elevation_correct) |> 
      relocate(elevation, .after = summit) |> 
      mutate(species = case_when(species == "Alc_sp" ~ "Alc_glo", 
                                 TRUE ~ species)) |> 
      left_join(filefjell_maintype_cover_tidy |> select(summit, main_type, cover), by = c("summit", "main_type")) |> 
      relocate(cover, .after = type)
  ),
  tar_target(
    name = filefjell_data_clean,
    command = filefjell_2024_2025_clean |> 
      select(!c(weather, main_type, type, cover)) |> 
      rbind(filefjell_1972_2008_2009_clean) |> 
      left_join(filefjell_species, by = "species") |> 
      mutate(species = ifelse(!is.na(new_name), new_name, species)) |> 
      select(!new_name) |> 
      mutate(summit = factor(summit, levels = c("Berdalseken", "Suletinden", "Unnamed", "Storeknippa", "Graanosi", "Loppenosi", "Graveggi", "Krekanosi", "Rjupeskareggen", "Frostdalsnosi", "Krekanosi_S", "Slettningseggi", "Krekahoegdi"))) |> 
      relocate(category, .after = species) |> 
      arrange(summit, year, species)
  ),
  # Analyses datasets----
  tar_target(
    name = filefjell_wide,
    command = filefjell_data_clean |> 
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
  ),
  tar_target(
    name = filefjell_wide_new,
    command = filefjell_wide |> 
      mutate(adj_dist1 = ifelse(is.na(distance1) & !is.na(distance2), 33, distance1),
             adj_dist2 = ifelse(is.na(distance2) & !is.na(distance3), 33, distance2),
             adj_dist3 = ifelse(is.na(distance3) & is.na(distance1) & !is.na(distance2), 33, distance3))
  ),
  tar_target(
    name = filefjell_visit_years,
    command = filefjell_data_clean |> 
      select(year, summit) |> 
      distinct() |> 
      pivot_wider(names_from = year, names_prefix = "y", values_from = year) |> 
      mutate(first = 1972, 
             second = coalesce(y2008, y2009),
             third = coalesce(y2024, y2025)) |> 
      select(summit, first, second, third)
  ),
  tar_target(
    name = turnover_species,
    command = filefjell_wide |>
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
      relocate(development, .after = category) |> 
      mutate(rate1 = turnover1 / period1,
             rate2 = turnover2 / period2) |> 
      pivot_longer(cols = c(period1, period2), names_to = "period", values_to = "years") |> 
      mutate(period = as.factor(period)) |> 
      mutate(rate = ifelse(period == "period1", rate1, rate2))
  ),
  # Elevation----
  tar_target(
    name = elerate_all,
    command = filefjell_wide |>
      mutate(change1 = distance1 - distance2, 
             change2 = distance2 - distance3) |> 
      pivot_longer(cols = c(period1, period2), names_to = "period", values_to = "years") |>
      mutate(period = as.factor(period)) |>
      mutate(change = case_when(period == "period1" ~ change1,
                                period == "period2" ~ change2)) |>
      filter(!is.na(change)) |> 
      mutate(rate = change / years)
  ),
  tar_target(
    name = elerate_all_bayes,
    command = brm(
      bf(rate ~ 
           period * category + (1|summit) + (1|species),
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
  ),
  tar_target(
    name = elerate_all_results,
    command = elerate_all_bayes |> 
      emmeans( ~ period * category) |> 
      tidy(conf.int = TRUE) |> 
      clean_names() |> 
      rename(conf_low = lower_hpd,
             conf_high = upper_hpd) |> 
      mutate(model = "elevation") |> 
      relocate(model)
  ),
  tar_target(
    name = elerate_remained,
    command = filefjell_wide |>
      mutate(change1 = distance1 - distance2, 
             change2 = distance2 - distance3) |> 
      filter(!is.na(change1) & !is.na(change2)) |>
      pivot_longer(cols = c(period1, period2), names_to = "period", values_to = "years") |>
      mutate(period = as.factor(period)) |>
      mutate(change = case_when(period == "period1" ~ change1,
                                period == "period2" ~ change2)) |>
      mutate(rate = change / years) |>
      select(!c(change1, change2))
  ),
  tar_target(
    name = elerate_rem_bayes,
    command = brm(
      bf(rate ~ 
           period * category + (1|summit) + (1|species),
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
  ),
  tar_target(
    name = elerate_rem_results,
    command = elerate_rem_bayes |> 
      emmeans( ~ period * category) |> 
      tidy(conf.int = TRUE) |> 
      clean_names() |> 
      rename(conf_low = lower_hpd,
             conf_high = upper_hpd) |> 
      mutate(model = "elevation") |> 
      relocate(model)
  ),
  tar_target(
    name = elerate_new,
    command = filefjell_wide_new |>
      mutate(change1 = adj_dist1 - adj_dist2, 
             change2 = adj_dist2 - distance3) |> 
      filter(!is.na(change1) & !is.na(change2)) |>
      pivot_longer(cols = c(period1, period2), names_to = "period", values_to = "years") |>
      mutate(period = as.factor(period)) |>
      mutate(change = case_when(period == "period1" ~ change1,
                                period == "period2" ~ change2)) |>
      mutate(rate = change / years) |>
      select(!c(change1, change2))
  ),
  tar_target(
    name = elerate_new_bayes,
    command = brm(
      bf(rate ~ 
           period * category + (1|summit) + (1|species),
         sigma ~period),
      family = student(),
      data = elerate_new,
      control = list(adapt_delta = 0.999),
      seed = 811
    )
  ),
  tar_target(
    name = elerate_new_results,
    command = elerate_new_bayes |> 
      emmeans( ~ period * category) |> 
      tidy(conf.int = TRUE) |> 
      clean_names() |> 
      rename(conf_low = lower_hpd,
             conf_high = upper_hpd) |> 
      mutate(model = "elevation") |> 
      relocate(model)
  ),
  # Richness----
  tar_target(
    name = richness_rate,
    command = turnover_species |> 
      summarise(.by = c(summit, period), rate = sum(rate))
  ),
  tar_target(
    name = richrate_mod,
    command = glmmTMB(
      rate ~
        period + (1 | summit),
      family = gaussian,
      data = richness_rate)
  ),
  tar_target(
    name = richrate_results,
    command = richrate_mod |>
      emmeans( ~ period) |>
      tidy(conf.int = TRUE) |> 
      clean_names() |> 
      select(period, estimate, conf_low, conf_high) |>
      mutate(model = "richness") |> 
      relocate(model)
  ),
  # Turnover----
  tar_target(
    name = turnover_summit,
    command = turnover_species |> 
      summarise(.by = c(summit, period), 
                new = sum(case_when(rate > 0 ~ rate), na.rm = TRUE), 
                nochange = sum(case_when(rate == 0 ~ rate), na.rm = TRUE), 
                lost = sum(case_when(rate < 0 ~ rate), na.rm = TRUE))
    ),
  tar_target(
    name = turnew_mod,
    command = glmmTMB(
      new ~
        period + (1 | summit),
      family = gaussian,
      data = turnover_summit)
  ),
  tar_target(
    name = turnew_results,
    command = turnew_mod |>
      emmeans( ~ period) |>
      tidy(conf.int = TRUE) |> 
      clean_names() |> 
      select(period, estimate, conf_low, conf_high) |>
      mutate(model = "new") |> 
      relocate(model)
  ),
  tar_target(
    name = turlost_mod,
    command = glmmTMB(
      lost ~
        period + (1 | summit),
      dispformula = ~period,
      family = gaussian,
      data = turnover_summit)
  ),
  tar_target(
    name = turlost_results,
    command = turlost_mod |>
      emmeans( ~ period) |>
      tidy(conf.int = TRUE) |> 
      clean_names() |> 
      select(period, estimate, conf_low, conf_high) |>
      mutate(model = "lost") |> 
      relocate(model)
  )
  # # New species per nature type----
  # tar_target(
  #   name = colonizers_data,
  #   command = filefjell_2024_clean |>
  #     semi_join(
  #       species_turnover_data |>
  #         filter(turnover24 == 1) |>
  #         select(summit:species)) |>
  #     mutate(hovedtype = str_sub(type, start = 1L, end = 2L)) |>
  #     relocate(hovedtype, .before = type)
  # )
)
