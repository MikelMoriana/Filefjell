# This is a helper script to run the pipeline

library(targets)
library(tidyverse)
library(visNetwork)

tar_visnetwork(script = "_targets.R")
tar_make(script = "_targets.R")

vis_data <- tar_visnetwork(script = "_targets.R")
edges <- vis_data$x$edges

manual_positions <- tribble(
  ~name,                             ~x,    ~y,
  
  # Functions
  
  
  # Tidy data
  "elevation_1972_2009_file",         0,   -300,
  "elevation_1972_2009",            200,   -300,
  "elevation_1972_tidy",            400,   -300,
  "visit_dates_file",                 0,   -200,
  "visit_dates",                    200,   -200,
  "elevation_2008_2009_tidy",       400,   -200,
  "summit_data_file",                 0,   -100, 
  "summit_data",                    200,   -100,
  "summit_data_tidy",               400,   -100,
  
  "filefjell_2024_file",              0,      0,
  "filefjell_2024",                 200,      0,
  "filefjell_2025_file",              0,    100,
  "filefjell_2025",                 200,    100,
  "elevation_2024_2025_tidy",       400,      0,
  "type_species_tidy",              600,    100,
  
  "polygones_file",                   0,    200,
  "polygones",                      200,    200,
  "polygones_tidy",                 400,    200,
  "polygones_cover",                600,    200,
  "type_cover_file",                  0,    300, 
  "type_cover",                     200,    300,
  "type_cover_tidy",                400,    300,
  "maintype_cover_tidy",            600,    300,
  
  # Clean data
  "filefjell_species_file",         850,      0,
  "filefjell_species",             1050,      0,
  "elevation_1972_clean",           600,   -250,
  "elevation_2008_2009_clean",      600,   -150,
  "elevation_2024_2025_clean",      600,      0,
  "elevation_data_clean",          1200,   -100,
  "type_species_clean",            1000,    100,
  
  # Datasets for analyses
  "visit_years",                   1200,      0,
  "elevation_wide",                1400,      0,
  "elevation_wide_new",            1400,    250,
  "turnover_species",              1600,   -250,
  
  # Turnover
  "observations",                  1400,   -400,
  "turnover_grouped",              1600,   -350,
  "turnover_development",          1800,   -350,
  "observations_turnover_ft",      2000,   -400,
  "turnover_summit",               1600,   -100,
  "turnew_mod",                    1800,   -150,
  "turnew_results",                2000,   -150,
  "turlost_mod",                   1800,    -50,
  "turlost_results",               2000,    -50,
  
  # Richness rate
  "richness_rate",                 1800,   -250,
  "richrate_mod",                  2000,   -250,
  "richrate_results",              2200,   -250,
  
  # Elevation change
  "elerate_all",                   1800,     50,
  "elerate_all_bayes",             2000,     50,
  "elerate_all_results",           2200,     50,
  "elerate_remained",              1800,    150,
  "elerate_rem_bayes",             2000,    150,
  "elerate_rem_results",           2200,    150,
  "elerate_new",                   1800,    250,
  "elerate_new_bayes",             2000,    250,
  "elerate_new_results",           2200,    250,
  
  #Results
  "mod_summary",                   2600,      0
)

nodes <- vis_data$x$nodes |> 
  left_join(manual_positions, by = "name") |> 
  mutate(hidden = ifelse(name %in% c("adj_label", "backwards_selection", "colour_mapping", "data_tidying", "gg_modvars", "gg_results", "gg_yearline", "model_diagnosis", "model_distribution", "model_homoscedasticity", "optimizer", "remove_terms"), TRUE, FALSE))

visNetwork(nodes, edges) |> 
  visNodes() |> 
  visPhysics(enabled = FALSE)

