# This is a helper script to run the pipeline

library(targets)

tar_visnetwork(script = "_targets.R")
tar_make(script = "_targets.R")
