# Created by use_targets().
# Setting up the environment----

# Load packages required to define the pipeline:
library(targets)


# Target options:
tar_option_set(
  packages = c("tidyverse", "forcats", "vegan", "glmmTMB"),
  format = "rds", 
  seed = 811
)

# R scripts in the R/ folder with custom functions:
tar_source("Scripts/Functions.R")

# List of targets:
list(
)
