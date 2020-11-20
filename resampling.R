# Resampling
library(tidyverse)
library(reshape2)
library(mgcv)
library(MASS)


# Set WD
setwd("/Users/skycope/Documents/GitHub/grass-pollen")

# Read in data
total   = read.csv("data_complete.csv", h = T) %>% mutate(date = as.Date(date))

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

# Point prediction for one day ahead
mean_oneday = exp(predict(ema_2, val))

# Sample from this dist 100 or so times
oneday_dist = MASS:rnegbin(100, mu = mean_oneday, theta = theta_est)

# Two-day ahead predictions ----
# Update weather variables and ds


# Get 100 predictions for two days ahead (one prediction for each pollen)

# For each prediction sample one value from the 
# negative binomial distribution around it









