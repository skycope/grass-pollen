# Resampling
library(tidyverse)
library(reshape2)
library(mgcv)
library(MASS)
library(dplyr)


# Set WD
# setwd("/Users/skycope/Documents/GitHub/grass-pollen")
setwd("/Users/chloestipinovich/Documents/2020/Thesis Project/grass-pollen")


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

train = filter(total, fyear <= 2014) # for 2014 and before
val = filter(total, fyear == 2018)
test = filter(total, fyear == 2019)
  
  
# Training Set: exponential moving average variables
ema = train %>% 
  dplyr::select(pollen_count, ds, min_temp, max_temp, veg_index, humid, rain, wind_speed, wind_dir, fyear, season, pollen_cat) %>%
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

# Validation Set: exponential moving average variables
val_ema = val %>% 
  dplyr::select(pollen_count, ds, min_temp, max_temp, veg_index, humid, rain, wind_speed, wind_dir, fyear, season, pollen_cat) %>%
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

# Test Set: exponential moving average variables
test_ema = test %>% 
  dplyr::select(pollen_count, ds, min_temp, max_temp, veg_index, humid, rain, wind_speed, wind_dir, fyear, season, pollen_cat) %>%
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

# Run GAMs with exponential moving averages
ema_1 = gam(pollen_count ~ 
              fyear +
              s(rollmean_maxtemp) +
              s(max_temp) + s(ds, bs = 'cc', by = fyear) +
              s(wind_dir, bs = 'cc') +  s(rollmean_winddir, bs = 'cc') +
              s(rain) +  s(rollmean_rain) + 
              s(rollmean_pollen) + s(wind_speed) + 
              s(rollmean_windspeed) + s(humid) + s(rollmean_humid) + 
              s(veg_index) + s(lag2_pollen) + s(lag2_rain), 
            family = nb(), data = ema, scale = -0.1)

(theta_est <- ema_1$family$getTheta(TRUE))

ema_2 = gam(pollen_count ~ 
              s(rollmean_maxtemp) +
              s(max_temp) + s(ds, bs = 'cc', by = fyear) +
              s(wind_dir, bs = 'cc') +  s(rollmean_winddir, bs = 'cc') +
              s(rain) +  s(rollmean_rain) + 
              s(rollmean_pollen) + s(wind_speed) + 
              s(rollmean_windspeed) + s(humid) + s(rollmean_humid) + 
              s(veg_index), 
            family = negbin(theta_est), data = ema)

# Set a two week period
twoWeeks   = test[1:14,] %>% 
                dplyr::select(pollen_count, ds, min_temp, max_temp,
                              veg_index, humid, rain, wind_speed, wind_dir, 
                              fyear, season)
twoWeeks$pollen_count[8:14] = NA

# Function that creates required moving averages for prediction:
# - dat    = two week data set
# - rng    = 8 day period, 8th day is the prediction day
# - output = single row of data with all variables required to make prediction
lags = function(dat, rng){
  output = dat[rng,] %>%
    mutate(rollmean_maxtemp   = lag(EMA(max_temp, 7), 1),
           rollmean_pollen    = lag(EMA(pollen_count, 7), 1),
           rollmean_rain      = lag(EMA(rain, 7), 1),
           rollmean_windspeed = lag(EMA(wind_speed, 7), 1),
           rollmean_humid     = lag(EMA(humid, 7), 1),
           rollmean_winddir   = lag(EMA(wind_dir, 7), 1),
           lag2_rain          = lag(rain, 1),
           lag2_pollen        = lag(pollen_count, 1))
  return(output[8,])
}

# Function that performs prediction using ema_2
GAM_predict = function(model, day){
  return(exp(predict(model, day)))
}

# Store Predictions
predictions = as.data.frame(matrix(NA, nrow = 7, ncol = 5))
names(predictions) = c("Very_Low", "Low", "Moderate", "High", "Very_High")
for (i in 1:7){
  
  day_ahead = lags(twoWeeks, c(1:(i+7)))
  predic
}
  
# Create data for one day ahead prediction
oneDayData  = lags(twoWeeks, c(1:8))



# Point prediction for one day ahead
mean_oneday = exp(predict(ema_2, val_ema[1,]))

# Sample from this dist 100 or so times
oneday_dist = MASS:rnegbin(100, mu = mean_oneday, theta = theta_est)

# Two-day ahead predictions ----
# Update weather variables and ds


# Get 100 predictions for two days ahead (one prediction for each pollen)

# For each prediction sample one value from the 
# negative binomial distribution around it
mean_pollen = 20 
theta_est = 1.5

dist = MASS::rnegbin(100, mu = mean_pollen, theta = exp(theta_est))

predict_cat = case_when(
  dist < 1 ~ "Very low",
  dist >= 1 & dist < 3 ~ "Low",
  dist >= 3 & dist < 8 ~ "Moderate",
  dist >= 8 & dist < 14.8 ~ "High",
  dist >= 14.8 ~ "Very high") %>%
  ordered(., levels = c("Very Low", "Low", "Moderate", "High", "Very High"))

plot(predict_cat)









