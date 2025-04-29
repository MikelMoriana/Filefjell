# This is a helper script to run the pipeline

library(targets)
library(tidyverse)
library(visNetwork)

tar_visnetwork(script = "_targets.R")
tar_make(script = "_targets.R")

vis_data <- tar_visnetwork(script = "_targets.R")
edges <- vis_data$x$edges
nodes <- vis_data$x$nodes |> 
  mutate(x = level * 200) |>
  mutate(y = case_when(
    # 1
    name == "filefjell_1972_2009_file" ~ -100, 
    name == "filefjell_summit_data_file" ~ 0, 
    name == "filefjell_2024_file" ~ 100, 
    # 2
    name == "filefjell_1972_2009" ~ -100, 
    name == "filefjell_summit_data" ~ 0, 
    name == "filefjell_2024" ~ 100, 
    # 3
    name == "filefjell_1972_2009_tidy" ~ -100, 
    name == "filefjell_summit_data_tidy" ~ 0, 
    name == "filefjell_2024_tidy" ~ 100, 
    # 4
    name == "filefjell_1972_2009_clean" ~ -50, 
    name == "filefjell_2024_clean" ~ 50, 
    # 5
    name == "filefjell_all_years" ~ 0, 
    # 6
    name == "filefjell_distance" ~ -100, 
    name == "filefjell_richness" ~ 0, 
    name == "filefjell_turnover" ~ 150, 
    # 7
    name == "filefjell_distance_change" ~ -100, 
    name == "richness_mod" ~ 0, 
    name == "filefjell_lost_aov" ~ 100, 
    name == "filefjell_new_aov" ~ 200, 
    # 8
    name == "distance_change_model" ~ -100, 
    # Others
    name == "adj_label" ~ 200, 
    name == "colour_mapping" ~ 300, 
    name == "gg_modvars" ~ 200, 
    name == "gg_yearline" ~ 300, 
    name == "model_ch_factors" ~ 400, 
    name == "model_diagnosis" ~ 500, 
    name == "model_distribution" ~ 600, 
    name == "model_factors" ~ 700, 
    name == "model_homoscedasticity" ~ 800, 
    name == "optimizer" ~ 900, 
    name == "remove_terms" ~ 1000, 
    name == "frequency_nmds_var" ~ 1100))

visNetwork(nodes, edges) |> 
  visNodes() |> 
  visPhysics(enabled = FALSE)


