# script for initial loading and preparing data
# clean environment
rm(list = ls())

# load tools and paths
source("scripts/000_tools.R")

# load data with target but also including some factors and potential predictors
data_regeneration <- import("data/data_regeneration.xlsx")

# clean columns names
data_flow <- data_regeneration %>% clean_names()
