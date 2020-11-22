# Resampling

{
  library(tidyverse)
  library(reshape2)
  library(mgcv)
  library(MASS)
  library(dplyr)
}

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

# Set a two week period ----------
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
# - model = best GAM model with corresponding theta_est
# - day   = single row of all variables needed for prediction including moving averages
GAM_predict = function(model, day){
  return(exp(predict(model, day)))
}

# Function that returns Table of frequencies
# - pred = point estimate from day ahead prediction
# - n    = number of samples from neg binomial
# - table(dist_cat)/n = table of category probabilities
freq = function(pred, n){
  dist     = MASS::rnegbin(n, mu = pred, theta = theta_est)
  dist_cat = case_when(
    dist < 1 ~ "Very Low",
    dist >= 1 & dist < 3 ~ "Low",
    dist >= 3 & dist < 8 ~ "Moderate",
    dist >= 8 & dist < 14.8 ~ "High",
    dist >= 14.8 ~ "Very High") %>%
    ordered(., levels = c("Very Low", "Low", "Moderate", "High", "Very High"))
  return(list(freq_table = table(dist_cat)/n, samples = dist))
}

# Function that returns Table of frequencies from n_sample samples
# - dist = row of samples
# - n    = number of samples 
# - table(dist_cat)/n = table of category probabilities
freq2 = function(dist, n){
  dist_cat = case_when(
    dist < 1 ~ "Very Low",
    dist >= 1 & dist < 3 ~ "Low",
    dist >= 3 & dist < 8 ~ "Moderate",
    dist >= 8 & dist < 14.8 ~ "High",
    dist >= 14.8 ~ "Very High") %>%
    ordered(., levels = c("Very Low", "Low", "Moderate", "High", "Very High"))
  return(table(dist_cat)/n)
}

# Function that returns the correct historic data for a specific sample path
# - sample_row = sample path out of 1 to n_samples
# - num days   = number of days forward the sample path has predicted so far 
# - past_sample_data = two week period with specific sample path data
past = function(sample_row, num_days){
  past_sample_data = twoWeeks
  for (day in 1:num_days){
    past_sample_data$pollen_count[day+7]  = past_samples[day,sample_row]
  }
  return(past_sample_data)
}

# Initiate Storage for Predictions
predictions        = as.data.frame(matrix(NA, nrow = 7, ncol = 5))
names(predictions) = c("Very_Low", "Low", "Moderate", "High", "Very_High")
n_samples          = 1000 # Set the number fo sample paths
past_samples       = matrix(NA, ncol = n_samples, nrow = 7) # stores each of the n_sample paths for the 7 days 
with_post          = FALSE

# Make 7-day-ahead predictions 
# Update twoWeeks data set as you make a new prediction
for (i in 1:7){
  if (with_post==TRUE){
    if (i == 1){
      day_ahead       = lags(twoWeeks, c(i:(i+7) ) )
      pred            = as.numeric(GAM_predict(ema_2, day_ahead))
      results         = freq(pred, n_samples)
      predictions[i,] = results$freq_table
      past_samples[i,]= results$samples   # Save random samples called posterior samples
    }
    else{
      for (j in 1:n_samples){
        day_ahead          = lags(past(j,i), c(i:(i+7) ) )
        pred               = as.numeric(GAM_predict(ema_2, day_ahead))
        results            = freq(pred, 1)
        past_samples[i,j]  = results$samples
      }
      predictions[i,]      = freq2(past_samples[i,], n_samples)
    }
  }
  if (with_post==FALSE){
    day_ahead       = lags(twoWeeks, c(i:(i+7) ) )
    pred            = as.numeric(GAM_predict(ema_2, day_ahead))
    results         = freq(pred, n_samples)
    predictions[i,] = results$freq_table
    past_samples[i,]= results$samples   # Save random samples called posterior samples
    twoWeeks$pollen_count[i+7] = pred
  }
}
par(mfrow=(c(2,1)))
plot(as.numeric(predictions[1,]), type = "l")
for(k in 2:7){
  lines(as.numeric(predictions[k,]), col = k)
}

np = c()
for(i in 1:7){
  np = c(np, print(var(past_samples[i,])))
}

wp = c()
for(i in 1:7){
  wp = c(wp, print(var(past_samples[i,])))
}

np
wp



