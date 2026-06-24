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
  "filefjell_1972_2009_file",         0,   -300,
  "filefjell_1972_2009",            200,   -300,
  "filefjell_1972_tidy",            400,   -300,
  "visit_dates_file",                 0,   -200,
  "visit_dates",                    200,   -200,
  "filefjell_2008_2009_tidy",       400,   -200,
  "summit_data_file",                 0,   -100, 
  "summit_data",                    200,   -100,
  "summit_data_tidy",               400,   -100,
  
  "filefjell_2024_file",              0,      0,
  "filefjell_2024",                 200,      0,
  "filefjell_2025_file",              0,    100,
  "filefjell_2025",                 200,    100,
  "filefjell_2024_2025_tidy",       400,      0,
  "type_species_tidy",              600,    300,
  
  "polygones_file",                   0,    500,
  "polygones",                      200,    500,
  "polygones_tidy",                 400,    500,
  "polygones_cover",                600,    500,
  "type_cover_file",                  0,    600, 
  "type_cover",                     200,    600,
  "type_cover_tidy",                400,    600,
  "maintype_cover_tidy",            600,    600,
  
  # Clean data
  "filefjell_species_file",         750,    200,
  "filefjell_species",              900,    200,
  "filefjell_1972_clean",           600,   -250,
  "filefjell_2008_2009_clean",      600,   -150,
  "filefjell_2024_2025_clean",      600,      0,
  "filefjell_data_clean",           900,      0,
  "type_species_clean",             900,    550,
  
  # Datasets for analyses
  "filefjell_simplified",          1100,      0,
  "summit_periods",                1300,   -100,
  
  # Status
  "status",                        1300,   -450,
  "flows_all",                     1500,   -400,
  "strata",                        1500,   -500,
  "lodes_12",                      1700,   -450,
  "lodes_23",                      1700,   -350,
  "species_records_manually",      1500,   -600,
  "species_records_plot",          1900,   -500,
  
  # Richness rate
  "richness",                      1300,   -350,
  "richness_rate",                 1500,   -350,
  "richness_mod",                  1700,   -350,
  "richness_results",              1900,   -350,
  
  "turnover",                      1300,   -200,
  
  # New species
  "new_rate",                      1500,   -250,
  "new_mod",                       1700,   -250,
  "new_results",                   1900,   -250,
  
  # Lost species
  "lost_rate",                     1500,   -150,
  "lost_mod",                      1700,   -150,
  "lost_results",                  1900,   -150,
  
  # Original - Lost species
  "original_lost",                 1400,    -50,  
  "orilost_rate",                  1500,    -50,
  "orilost_mod",                   1700,    -50,
  "orilost_results",               1900,    -50,
  
  # Winners
  "new_lost",                      1500,     50,
  "winners",                       1700,     50,
  "winners_ft",                    1900.     50.
  
  # Altitude change
  "altitude_rate",                 1500,    150,
  "priors_t",                      1600,    250,
  "altitude_bay",                  1700,    150,
  "altitude_results",              1900,    150,
  
  #Results
  "mod_summary",                   2300,      0,
  "mod_types",                     2300,    400,
)

nodes <- vis_data$x$nodes |> 
  left_join(manual_positions, by = "name") |> 
  mutate(hidden = ifelse(name %in% c("adj_label", "alluvial_palette", "backwards_selection", "colour_mapping", "data_tidying", "gg_modvars", "gg_results", "gg_yearline", "model_diagnosis", "model_distribution", "model_homoscedasticity", "optimizer", "remove_terms"), TRUE, FALSE))

visNetwork(nodes, edges) |> 
  visNodes() |> 
  visPhysics(enabled = FALSE)

