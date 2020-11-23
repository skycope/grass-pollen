# testing 
# This script is for testing our models

# Libraries 
{
  library(tidyverse)
}

# Data
# Set WD
setwd("/Users/skycope/Documents/GitHub/grass-pollen")
# setwd("/Users/chloestipinovich/Documents/2020/Thesis Project/grass-pollen")


# Read in data
total   = read.csv("Final_Data/data_complete.csv", h = T) %>% mutate(date = as.Date(date))

# EMA function -----------
EMA = function (series, n){
  ema <- c()
  ema[1:(n-1)] <- NA
  ema[n]<- mean(series[1:n])
  beta <- 0.4
  for (i in (n+1):length(series)){
    ema[i] <- beta * series[i] + 
      (1-beta) * ema[i-1]
  }
  return(ema)
}

train = filter(total, fyear <= 2018) # for 2012 and before

# moving average variables --------------
ma = train %>% 
  select(pollen_count, ds, min_temp, max_temp, veg_index, humid, rain, wind_speed, wind_dir, 
         fyear, season, pollen_cat) %>%
  mutate(rollmean_maxtemp   = lag(rollmean(max_temp, 7, na.pad = T, align = 'right'), 1),
         #rollmean_vegindex = lag(rollmean(veg_index, 16, na.pad = T, align = 'right'), 1),
         rollmean_pollen    = lag(rollmean(pollen_count, 7, na.pad = T, align = 'right'), 1),
         rollmean_rain      = lag(rollmean(rain, 7, na.pad = T, align = 'right'), 1),
         rollmean_windspeed = lag(rollmean(wind_speed, 7, na.pad = T, align = 'right'), 1),
         rollmean_humid = lag(rollmean(humid, 7, na.pad = T, align = 'right'), 1),
         rollmean_winddir = lag(rollmean(wind_dir, 7, na.pad = T, align = 'right'), 1)) %>%
  na.omit() %>%
  mutate(index = 1:nrow(.))

# exponential moving average variables
ema = train %>% 
  select(pollen_count, ds, min_temp, max_temp, veg_index, humid, rain, wind_speed, wind_dir, fyear, season, pollen_cat) %>%
  mutate(rollmean_maxtemp   = lag(EMA(max_temp, 7), 1),
         #rollmean_vegindex = lag(rollmean(veg_index, 16, na.pad = T, align = 'right'), 1),
         rollmean_pollen    = lag(EMA(pollen_count, 7), 1),
         rollmean_rain      = lag(EMA(rain, 7), 1),
         rollmean_windspeed = lag(EMA(wind_speed, 7), 1),
         rollmean_humid =  lag(EMA(humid, 7), 1),
         rollmean_winddir = lag(EMA(wind_dir, 7), 1),
         lag2_rain = lag(rain, 1),
         lag2_pollen = lag(pollen_count, 1)) %>%
  na.omit() %>%
  mutate(index = 1:nrow(.))

# Here we will need the models

#

# Testing metrics

# Brier score
test_df = data.frame(
                     p_verylow = c(0, 0),
                     p_low = c(0, 0.9),
                     p_mod = c(0.6, 0),
                     p_high = c(0.4, 0.1),
                     p_veryhigh = c(0, 0),
                     actual_verylow = c(0, 0),
                     actual_low = c(0, 1),
                     actual_mod = c(0, 0),
                     actual_high = c(1, 0),
                     actual_veryhigh = c(0, 0))


brier_score = function(df){
  brier_score = 0 # set to start at zero
  for(i in 1:nrow(df)){ # iterate over the rows
    day_score = 0 # for each day's contribution, start at 0
    for(j in 1:5){
      day_score = day_score + (df[i, j] - df[i, j+5])^2
    }
    brier_score = brier_score + day_score
  }
  return(brier_score/nrow(df))
}

brier_score(test_df) # seems like it works

# For accuracy and mse
observed = c(1, 2, 3, 4, 5, 4, 3, 1) %>% as.factor()
predicted = c(1, 4, 3, 2, 5, 4, 3, 1)  %>% as.factor() 

# Accuracy: use caret on confusion matrix
library(caret)
cm = confusionMatrix(observed, predicted)
cm_table = cm$table

mse = function(cm){
  mse = 0
  for(r in 1:5){
    for(c in 1:5){
      mse = mse + cm[r, c]* (r - c)^2
    }
  }
  return(mse/sum(cm))
}

mse(cm_table)

# MAE
mae = function(cm){
  mae = 0
  for(r in 1:5){
    for(c in 1:5){
      mae = mae + cm[r, c]* abs(r - c)
    }
  }
  return(mae/sum(cm))
}

mae(cm_table)






