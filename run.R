# This is a helper script to run the pipeline

library(targets)
library(tidyverse)
library(visNetwork)

tar_visnetwork(script = "_targets.R")
tar_make(script = "_targets.R")

vis_data <- tar_visnetwork(script = "_targets.R")
edges <- vis_data$x$edges

manual_positions <- tribble(
  name,                              ~x,     ~y,
  
  # Functions
  
  
  # Cleaning the files
  "filefjell_1972_2009_file",      -100,       ,
  "filefjell_summit_data_file",       0,       , 
  "filefjell_2024_file",            100,       ,
  # 2
  "filefjell_1972_2009",           -100,       ,
  "filefjell_summit_data",            0,       ,
  "filefjell_2024",                 100,       ,
  # 3
  "filefjell_1972_2009_tidy",      -100,       ,
  "filefjell_summit_data_tidy",       0,       ,
  "filefjell_2024_tidy",            100,       ,
  # 4
  "filefjell_1972_2009_clean",      -50,       ,
  "filefjell_2024_clean",            50,       ,
  # 5
  "filefjell_data_clean",             0,       ,
  # 6
  "distance_change_data",          -100,       ,
  "richness_data",                    0,       ,
  "species_turnover_data",          150,       ,
  # 7
  "distance_change_rate_mod",      -100,       ,
  "richness_change_data",             0,       ,
  "turnover_data",                  150,       ,
  "colonizers_data",                250,       ,
  # 8
  "richness_change_rate_mod",         0,       ,
  "lost_rate_mod",                  100,       ,
  "new_rate_mod",                   200,       ,
  # Others
  "adj_label",                      200,       ,
  "colour_mapping",                 300,       ,
  "gg_modvars",                     200,       ,
  "gg_yearline",                    300,       ,
  "model_ch_factors",               400,       ,
  "model_diagnosis",                500,       ,
  "model_distribution",             600,       ,
  "model_factors",                  700,       ,
  "model_homoscedasticity",         800,       ,
  "optimizer",                      900,       ,
  "remove_terms",                  1000,       ,
  "frequency_nmds_var",            1100,       

)

nodes <- vis_data$x$nodes |> 
  left_join(manual_positions, by = "name")

visNetwork(nodes, edges) |> 
  visNodes() |> 
  visPhysics(enabled = FALSE)


