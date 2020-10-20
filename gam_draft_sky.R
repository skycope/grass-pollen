# GAM model draft
# Model draft
rm(list = ls())
# libraries
{
  library(tidyverse)
  library(ranger)
  library(caret)
  library(mgcv)
  library(gbm)
  library(reshape2)
}

setwd("/Users/skycope/Documents/UCT/Stats\ Honours/Project/Data\ and\ code")

# Read in data
load("API_B_format.Rda")
grassdata = data
veg_index = read.csv("veg_index.csv", h = T, sep = ';')

# We want daily veg index
daily_indices = sort(rep(1:nrow(veg_index), 16)) %>% head(.,3368)

daily_veg = veg_index[daily_indices, ]  %>% 
  mutate(date = seq(as.Date('2011-04-23'), as.Date('2020-07-11'), by = 1)
  ) %>% dplyr::select(-Date)
grassdata$date <- as.Date(grassdata$date)

merged <- merge(daily_veg, grassdata, by = 'date') %>%
  mutate(season = case_when(
    ds > 240 | ds < 30 ~ "In season",
    ds >= 30 | ds <= 240 ~ "Not in season"
  ))

# In season divisons
in_season = filter(merged, season == "In season")
out_season = filter(merged, season == "Not in season")


