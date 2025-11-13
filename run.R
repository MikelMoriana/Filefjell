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
  "filefjell_1972_2009_file",         0,   -250,
  "filefjell_1972_2009",            200,   -250,
  "filefjell_1972_tidy",            400,   -250,
  "filefjell_dates_2008_2009_file",   0,   -150,
  "filefjell_dates_2008_2009",      200,   -150,
  "filefjell_2008_2009_tidy",       400,   -150,
  "filefjell_1972_2008_2009_tidy",  600,   -200,
  "filefjell_summit_data_file",       0,    -50, 
  "filefjell_summit_data",          200,    -50,
  "filefjell_summit_data_tidy",     400,    -50,
  
  "filefjell_2024_file",              0,     50,
  "filefjell_2024",                 200,     50,
  "filefjell_2024_tidy",            400,     50,
  "filefjell_2025_file",              0,    150,
  "filefjell_2025",                 200,    150,
  "filefjell_2025_tidy",            400,    150,
  "filefjell_2024_2025_tidy",       600,    100,
  "filefjell_type_cover_file",        0,    250, 
  "filefjell_type_cover",           200,    250,
  "filefjell_type_cover_tidy",      400,    250,
  "filefjell_maintype_cover_tidy",  600,    250,
  
  # Clean data
  "filefjell_1972_2008_2009_clean", 800,   -100,
  "filefjell_2024_2025_clean",      800,    100,
  "filefjell_data_clean",          1000,      0
  
  # # 6
  # "distance_change_data",          -100,   -100,
  # "richness_data",                    0,   -100,
  # "species_turnover_data",          150,   -100,
  # # 7
  # "distance_change_rate_mod",      -100,   -100,
  # "richness_change_data",             0,   -100,
  # "turnover_data",                  150,   -100,
  # "colonizers_data",                250,   -100,
  # # 8
  # "richness_change_rate_mod",         0,   -100,
  # "lost_rate_mod",                  100,   -100,
  # "new_rate_mod",                   200,   -100,
  # # Others
  # "adj_label",                      200,   -100,
  # "colour_mapping",                 300,   -100,
  # "gg_modvars",                     200,   -100,
  # "gg_yearline",                    300,   -100,
  # "model_ch_factors",               400,   -100,
  # "model_diagnosis",                500,   -100,
  # "model_distribution",             600,   -100,
  # "model_factors",                  700,   -100,
  # "model_homoscedasticity",         800,   -100,
  # "optimizer",                      900,   -100,
  # "remove_terms",                  1000,   -100,
  # "frequency_nmds_var",            1100,   -100
)

nodes <- vis_data$x$nodes |> 
  left_join(manual_positions, by = "name") |> 
  mutate(hidden = ifelse(name %in% c("adj_label", "backwards_selection", "colour_mapping", "data_tidying", "gg_modvars", "gg_yearline", "model_diagnosis", "model_distribution", "model_homoscedasticity", "optimizer", "remove_terms"), TRUE, FALSE))

visNetwork(nodes, edges) |> 
  visNodes() |> 
  visPhysics(enabled = FALSE)

