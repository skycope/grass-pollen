# GAM model draft
# Model draft
rm(list = ls())
# libraries
{
  library(tidyverse)
  library(caret)
  library(mgcv)
  library(gbm)
  library(reshape2)
  library(zoo)
  library(corrplot)
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
  )) %>% rename(veg_index = Mean, pollen_count = value, 
                min_temp = value.temp_min, humid = value.humid,
                rain = value.rain, wind_speed = value.wind_speed,
                visibility = Visibility, mintemp_anom = mintemp.anom, 
                maxtemp_anom = maxtemp.anom, max_temp = value.temp_max, 
                wind_dir = value.wind_dir) %>%
  mutate(pollen_cat = case_when(pollen_count < 1 ~ "Very low",
         pollen_count >= 1 & pollen_count < 3 ~ "Low",
         pollen_count >= 3 & pollen_count < 8 ~ "Moderate",
         pollen_count >= 8 & pollen_count < 14.8 ~ "High",
         pollen_count >= 14.8 ~ "Very high"))

# In season divisons
in_season = filter(merged, season == "In season")
out_season = filter(merged, season == "Not in season")

# moving average variables
gam_data = merged %>% 
  select(pollen_count, ds, min_temp, max_temp, veg_index, humid, rain, wind_speed, 
         mintemp_anom, wind_dir, fyear, season, pollen_cat) %>%
  mutate(rollmean_maxtemp   = lag(rollmean(max_temp, 7, na.pad = T, align = 'right'), 1),
         #rollmean_vegindex = lag(rollmean(veg_index, 16, na.pad = T, align = 'right'), 1),
         rollmean_pollen    = lag(rollmean(pollen_count, 7, na.pad = T, align = 'right'), 1),
         rollmean_rain      = lag(rollmean(rain, 7, na.pad = T, align = 'right'), 1),
         rollmean_windspeed = lag(rollmean(wind_speed, 7, na.pad = T, align = 'right'), 1),
         rollmean_humid = lag(rollmean(humid, 7, na.pad = T, align = 'right'), 1)) %>%
  na.omit()



# inseason and outseason
in_season = filter(gam_data, season == "In season")
out_season = filter(gam_data, season == "Not in season")

oneday_train = filter(gam_data, fyear != 2014)
oneday_test = filter(gam_data, fyear == 2014)

plot(oneday_test$max_temp, type = 'l')
lines(oneday_test$rollmean_maxtemp, col = 'red')

select(gam_data, -season, -fyear, -pollen_cat) %>% cor() %>% ggcorrplot(type = 'upper', lab = T)

model_1 = gam(pollen_count ~ s(rollmean_pollen, bs = 'cc') + s(rollmean_maxtemp, bs = 'cc')  
              + s(max_temp, bs = 'cc') + s(ds, bs = 'cc') +
                s(wind_dir) + s(rain) + s(rollmean_rain) + s(humid) + s(wind_speed) + 
                s(rollmean_windspeed) + s(rollmean_humid), 
            family = nb(), data = oneday_train, scale = -0.1)

(theta_est <- model_1$family$getTheta(TRUE))

model_2 = gam(pollen_count ~ s(rollmean_pollen, bs = 'cc') + s(rollmean_maxtemp, bs = 'cc') +
                 s(max_temp, bs = 'cc') + s(ds, bs = 'cc') +
                s(wind_dir) + s(rain) + s(rollmean_rain) + s(humid) + s(wind_speed) + 
                s(rollmean_windspeed) + s(rollmean_humid), 
              family = negbin(theta_est), data = oneday_train)

AIC(model_1, model_2)

oneday_predict = exp(predict(model_2, newdata = oneday_test))

predict_cat = case_when(
  oneday_predict < 1 ~ "Very low",
  oneday_predict >= 1 & oneday_predict < 3 ~ "Low",
  oneday_predict >= 3 & oneday_predict < 8 ~ "Moderate",
  oneday_predict >= 8 & oneday_predict < 14.8 ~ "High",
  oneday_predict >= 14.8 ~ "Very high") %>%
  ordered(., levels = c("Very low", "Low", "Moderate", "High", "Very high"))

plot(predict_cat)

observed = oneday_test$pollen_count
observed_cat = oneday_test$pollen_cat %>% 
  ordered(., levels = c("Very low", "Low", "Moderate", "High", "Very high"))

confusionMatrix(observed_cat, predict_cat)

summary(model_2)
# two day ahead forecasts
oneday_test$pollen_count

twoday_test = oneday_test %>% 
  mutate(oneday_predict = oneday_predict) %>%
  mutate(rollmean_pollen = 
           (oneday_predict +
                 6*lag(rollmean(pollen_count, 6, fill = NA, align = "right"), 2)) / 7)


twoday_predict = exp(predict(model_2, newdata = twoday_test))


threeday_test = twoday_test %>% 
  mutate(twoday_predict = twoday_predict) %>%
  mutate(rollmean_pollen = 
           (oneday_predict + twoday_predict + 
              5*lag(rollmean(pollen_count, 5, fill = NA, align = "right"), 3)) / 7)

threeday_predict = exp(predict(model_2, newdata = threeday_test))

fourday_test = threeday_test %>% 
  mutate(threeday_predict = threeday_predict) %>%
  mutate(rollmean_pollen = 
           (threeday_predict + oneday_predict + twoday_predict + 
              4*lag(rollmean(pollen_count, 4, fill = NA, align = "right"), 4)) / 7)

fourday_predict = exp(predict(model_2, newdata = fourday_test))

fiveday_test = fourday_test %>% 
  mutate(fourday_predict = fourday_predict) %>%
  mutate(rollmean_pollen = 
           (threeday_predict + oneday_predict + twoday_predict + fourday_predict +
              3*lag(rollmean(pollen_count, 3, fill = NA, align = "right"), 5)) / 7)

fiveday_predict = exp(predict(model_2, newdata = fiveday_test))

sixday_test = fiveday_test %>% 
  mutate(fiveday_predict = fiveday_predict) %>%
  mutate(rollmean_pollen = 
           (threeday_predict + oneday_predict + twoday_predict + fourday_predict + 
              fiveday_predict +
              2*lag(rollmean(pollen_count, 2, fill = NA, align = "right"), 6)) / 7)
sixday_predict = exp(predict(model_2, newdata = sixday_test))

sevenday_test = sixday_test %>% 
  mutate(sixday_predict = sixday_predict) %>%
  mutate(rollmean_pollen = 
           (threeday_predict + oneday_predict + twoday_predict + fourday_predict +
              fiveday_predict + sixday_predict +
              lag(rollmean(pollen_count, 1, fill = NA, align = "right"), 7)) / 7)

sevenday_predict = exp(predict(model_2, newdata = sevenday_test))

sevenday_predict_cat = case_when(
  sevenday_predict < 1 ~ "Very low",
  sevenday_predict >= 1 & sevenday_predict < 3 ~ "Low",
  sevenday_predict >= 3 & sevenday_predict < 8 ~ "Moderate",
  sevenday_predict >= 8 & sevenday_predict < 14.8 ~ "High",
  sevenday_predict >= 14.8 ~ "Very high") %>%
  ordered(., levels = c("Very low", "Low", "Moderate", "High", "Very high"))

confusionMatrix(observed_cat, sevenday_predict_cat)



plot(oneday_predict, type = 'l', main = '1-day', col = 'red')

lines(twoday_predict, type = 'l', col = 'blue', main = '2-day')
lines(threeday_predict, type = 'l', col = 'red', main = '3-day')
lines(fourday_predict, type = 'l', main = '4-day', col= 'green')
lines(fiveday_predict, type = 'l', col = 'orange', main = '5-day')
lines(sixday_predict, type = 'l', col = 'violet', main = '6-day')
lines(sevenday_predict, type = 'l', main = '7-day', col = 'turquoise')

plot(oneday_test$pollen_count, oneday_predict)
abline(0, 1, col = 'red')
cor(oneday_test$pollen_count[8:length(sevenday_predict)], oneday_predict[8:length(sevenday_predict)])

sqrt(sum((oneday_test$pollen_count -  oneday_predict)^2))
mape(oneday_test$pollen_count,  oneday_predict)


plot(oneday_train$pollen_count, oneday_predict)

qqplot(oneday_test$pollen_count[8:length(sevenday_predict)], 
       sevenday_predict[8:length(sevenday_predict)])

abline(0, 1, col = 'red')

data.frame(oneday_predict, twoday_predict, threeday_predict, fourday_predict, 
           fiveday_predict, sixday_predict, sevenday_predict) %>% 
  na.omit() %>%
  cor() %>% 
  ggcorrplot() +
  scale_fill_gradient2(limit = c(0.87,1), low = "white", high =  "red", midpoint = 0.93)

