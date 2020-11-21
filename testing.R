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



