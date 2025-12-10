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
  "elevation_1972_2009_file",         0,   -350,
  "elevation_1972_2009",            200,   -350,
  "elevation_1972_tidy",            400,   -350,
  "visit_dates_file",                 0,   -250,
  "visit_dates",                    200,   -250,
  "elevation_2008_2009_tidy",       400,   -250,
  "summit_data_file",                 0,   -150, 
  "summit_data",                    200,   -150,
  "summit_data_tidy",               400,   -150,
  
  "filefjell_2024_file",              0,     50,
  "filefjell_2024",                 200,     50,
  "filefjell_2025_file",              0,    150,
  "filefjell_2025",                 200,    150,
  "elevation_2024_2025_tidy",       400,    100,
  "type_species_tidy",              600,    150,
  
  "polygones_file",                   0,    250,
  "polygones",                      200,    250,
  "polygones_tidy",                 400,    250,
  "polygones_cover",                600,    250,
  "type_cover_file",                  0,    350, 
  "type_cover",                     200,    350,
  "type_cover_tidy",                400,    350,
  "maintype_cover_tidy",            600,    350,
  
  # Clean data
  "filefjell_species_file",         400,    -50,
  "filefjell_species",              600,    -50,
  "elevation_1972_clean",           800,   -200,
  "elevation_2008_2009_clean",      800,   -100,
  "elevation_2024_2025_clean",      800,    100,
  "elevation_data_clean",          1000,      0,
  "type_species_clean",            1000,    200,
  
  # Datasets for analyses
  "filefjell_visit_years",         1100,    100,
  "filefjell_wide",                1200,      0,
  "filefjell_wide_new",            1400,    250,
  "turnover_species",              1400,   -175,
  
  # Elevation change
  "elerate_all",                   1600,     50,
  "elerate_all_bayes",             1800,     50,
  "elerate_all_results",           2000,     50,
  "elerate_remained",              1600,    150,
  "elerate_rem_bayes",             1800,    150,
  "elerate_rem_results",           2000,    150,
  "elerate_new",                   1600,    250,
  "elerate_new_bayes",             1800,    250,
  "elerate_new_results",           2000,    250,
  
  # Richness rate
  "richness_rate",                 1600,   -250,
  "richrate_mod",                  1800,   -250,
  "richrate_results",              2000,   -250,
  
  # Turnover
  "turnover_summit",               1600,   -100,
  "turnew_mod",                    1800,   -150,
  "turnew_results",                2000,   -150,
  "turlost_mod",                   1800,    -50,
  "turlost_results",               2000,    -50,
  
  #Results
  "mod_summary",                   2400,      0
)

nodes <- vis_data$x$nodes |> 
  left_join(manual_positions, by = "name") |> 
  mutate(hidden = ifelse(name %in% c("adj_label", "backwards_selection", "colour_mapping", "data_tidying", "gg_modvars", "gg_results", "gg_yearline", "model_diagnosis", "model_distribution", "model_homoscedasticity", "optimizer", "remove_terms"), TRUE, FALSE))

visNetwork(nodes, edges) |> 
  visNodes() |> 
  visPhysics(enabled = FALSE)

