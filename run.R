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
  
  "polygones_file",                   0,    750,
  "polygones",                      200,    750,
  "polygones_tidy",                 400,    750,
  "polygones_cover",                600,    750,
  "type_cover_file",                  0,    850, 
  "type_cover",                     200,    850,
  "type_tidy",                      400,    850,
  "habitat_cover",                  600,    800,
  "habitat_names_ft",               800,    900,
  
  # Clean data
  "filefjell_species_file",        1000,   -200,
  "filefjell_species",              800,   -200,
  "filefjell_1972_clean",           600,   -250,
  "filefjell_2008_2009_clean",      600,   -150,
  "filefjell_2024_2025_clean",      600,      0,
  "filefjell_data_clean",           900,      0,
  
  "types_with_species",             600,    400,
  
  # Datasets for analyses
  "filefjell_simplified",          1100,      0,
  "summit_periods",                1300,   -100,
  
  # Richness rate
  "richness",                      1300,   -350,
  "richness_overview",             1500,   -450,
  "richness_summits_ft",           1700,   -450,
  "richness_rate",                 1500,   -350,
  "richness_mod",                  1700,   -350,
  "richness_results",              1900,   -350,
  "richness_figure",               2200,   -350,
  
  "turnover",                      1300,   -200,
  
  # New species
  "new_rate",                      1500,   -250,
  "new_mod",                       1700,   -250,
  "new_results",                   1900,   -250,
  "new_figure",                    2200,   -250,
  
  # Lost species
  "lost_rate",                     1500,   -150,
  "lost_mod",                      1700,   -150,
  "lost_results",                  1900,   -150,
  "lost_figure",                   2200,   -150,
  
  # Original - Lost species
  "original_lost",                 1500,    -50,  
  "orilost_mod",                   1700,    -50,
  "orilost_results",               1900,    -50,
  "orilost_model_ft",              1900,     50,
  
  # Altitude change
  "altitude_rate",                 1500,    150,
  "priors_t",                      1600,     50,
  "altitude_bay",                  1700,    150,
  "altitude_results",              1900,    150,
  "altitude_figure",               2200,    100,
  
  "rates_figure",                  2400,   -150,
  
  # Winners
  "new_lost",                      1500,    300,
  "winners",                       1700,    300,
  "winners_ft",                    1900,    300,
  
  #Results
  "mod_summary",                   2100,    -50,
  
  # Alluvial plot
  "status",                        1300,    500,
  "flows_all",                     1500,    450,
  "strata",                        1500,    550,
  "lodes_12",                      1700,    400,
  "lodes_23",                      1700,    500,
  "species_records_manually",      1700,    600,
  "species_records_plot",          1900,    550,
  
  # Habitats
  "new_species_2024_2025",         1100,    600,
  "habitat_species_clean",         1000,    800,
  "habitat_new",                   1400,    700,
  "habitat_area",                  1400,    900,
  
  "habitat_new_proportions",       1600,    800,
  "habitat_new_header",            1700,    700,
  "habitat_new_proportions_ft",    1800,    800,
  
  "habitat_new_proportions_v",     1600,    900,
  "habitat_percentage_gg",         1800,   1100,
  "habitat_new_total_gg",          1800,    900,
  "habitat_new_proportions_gg",    1800,   1000,
  "habitat_percentage_new_gg",     2000,   1000
)

nodes <- vis_data$x$nodes |> 
  left_join(manual_positions, by = "name") |> 
  mutate(hidden = ifelse(name %in% c("adj_label", "alluvial_palette", "backwards_selection", "clean_ft", "colour_mapping", "data_cleaning", "gg_modvars", "gg_results", "gg_yearline", "model_diagnosis", "model_distribution", "model_homoscedasticity", "model_temporal_ac", "optimizer", "remove_terms"), TRUE, FALSE))

visNetwork(nodes, edges) |> 
  visNodes() |> 
  visPhysics(enabled = FALSE)

