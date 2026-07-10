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
  "filefjell_data_clean",           800,      0,
  "summit_ft",                      500,    100,
  
  "types_with_species",             600,    400,
  
  # Datasets for analyses
  "filefjell_simplified",          1000,      0,
  "summit_periods",                1200,   -100,
  
  # Richness rate
  "richness",                      1200,   -350,
  "richness_overview",             1400,   -450,
  "richness_summits_ft",           1600,   -450,
  "richness_rate",                 1400,   -350,
  "richness_mod",                  1600,   -350,
  "richness_results",              2000,   -350,
  "richness_figure",               2400,   -350,
  
  "turnover",                      1200,   -200,
  
  # New species
  "new_rate",                      1400,   -250,
  "new_mod",                       1600,   -250,
  "new_results",                   2000,   -250,
  "new_figure",                    2400,   -250,
  
  # Lost species
  "lost_rate",                     1400,   -150,
  "lost_mod",                      1600,   -150,
  "lost_results",                  2000,   -150,
  "lost_figure",                   2400,   -150,
  
  # Original - Lost species
  "original_lost",                 1400,    -50,  
  "orilost_mod",                   1600,    -50,
  "orilost_results",               2000,    -50,
  "orilost_model_ft",              2000,     50,
  
  # Altitude change
  "altitude_rate",                 1400,    150,
  "priors_t",                      1500,     50,
  "altitude_bay",                  1600,    150,
  "altitude_results",              2000,    150,
  "altitude_figure",               2400,    150,
  
  # Rates results
  "rate_emmeans",                  2200,    -50,
  "rate_emmeans_ft",               2400,    -50,
  "rate_contrasts",                2200,     50,
  "rate_contrasts_ft",             2400,     50,
  "rates_figure",                  2600,   -150,
  
  # Winners
  "new_lost",                      1400,    300,
  "winners",                       1600,    300,
  "winners_ft",                    1800,    300,
  
  #Results
  "mod_summary",                   1800,    -50,
  
  # Alluvial plot
  "status",                        1200,    500,
  "flows_all",                     1400,    450,
  "strata",                        1400,    550,
  "lodes_12",                      1600,    400,
  "lodes_23",                      1600,    500,
  "species_records_manually",      1600,    600,
  "species_records_plot",          1800,    550,
  
  # Habitats
  "new_species_2024_2025",         1000,    600,
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

