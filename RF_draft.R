# RF Draft

# Load Libraries ---------
{
  library(tidyverse)
  library(ranger)
  library(caret)
  library(mgcv)
  library(gbm)
  library(reshape2)
  library(dplyr)
  library(tidyverse)
  library(randomForest)
}

rm(list = ls())

setwd("/Users/chloestipinovich/Documents/2020/Thesis Project/grass-pollen")
#setwd("/Users/skycope/Documents/UCT/Stats\ Honours/Project/Data\ and\ code")

# Read in data ----------
load("API_B_format.Rda")
grassdata = data
veg_index = read.csv("veg_index.csv", h = T, sep = ';')

# We want daily veg index
daily_indices = sort(rep(1:nrow(veg_index), 16)) %>% head(.,3368)

daily_veg     = veg_index[daily_indices, ]  %>% 
  mutate(date = seq(as.Date('2011-04-23'), as.Date('2020-07-11'), by = 1)
  ) %>% dplyr::select(-Date)

# Remove Count, Range, Standard Deviation,
# Upper Quartile, Lower Quartile variable from veg index data

daily_veg     = daily_veg

# Initial Plots -------
ggplot(veg_index, aes(x = as.Date(Date), y = Mean)) +
  geom_line() +
  ylab("Vegetation Index") +
  xlab("Date") +
  theme_bw()

grassdata$date = as.Date(grassdata$date )

merged         = merge(daily_veg, grassdata, by = 'date') %>%
  mutate(season = case_when(
    ds > 240 | ds < 30 ~ "In",
    ds >= 30 | ds <= 240 ~ "Out"
  ))

pollen         = ggplot(merged, aes(x = as.Date(date))) +
  geom_line(aes(y = value), colour = 'darkblue') +
  ylab("Pollen Count") +
  xlab("") +
  theme_bw() +
  theme(legend.position = 'none')

vi_max         = ggplot(merged, aes(x = as.Date(date))) +
  geom_line(aes(y = Maximum, colour = 'darkred')) +
  ylab("Vegetation Index") +
  xlab("Date") +
  theme_bw() +
  theme(legend.position = 'none')

vi_mean        = ggplot(merged, aes(x = as.Date(date))) +
  geom_line(aes(y = Mean, colour = 'darkred')) +
  ylab("Vegetation Index") +
  xlab("Date") +
  theme_bw() +
  theme(legend.position = 'none')

ggpubr::ggarrange(pollen, vi_max,vi_mean, ncol = 1)


# Create Storage, set test year ------

models_RMSE           = matrix(NA, nrow = 5, ncol = 3)
colnames(models_RMSE) = c("Total", "In Season", "Out of Season")

testYear = 2014

# 1. Model ----------
{
  # Create lag values
  rf_data = dplyr::select(grassdata, -logvalue, -date) %>%
    mutate(count_cat_in = case_when(
      value < 1 ~ "Very low",
      value >= 1 & value <= 3 ~ "Low",
      value >= 3 & value <= 8 ~ "Moderate",
      value >= 8 & value <= 14.8 ~ "High",
      value >= 14.8 ~ "Very high")) %>% mutate(season = case_when(
      ds > 240 | ds < 30 ~ "In",
      ds >= 30 | ds <= 240 ~ "Out"
    )) %>% dplyr::select(-count_cat_in) %>% na.omit() 
  
  train = filter(rf_data, fyear != testYear)
  test  = filter(rf_data, fyear == testYear)
  
  summary(rf_data$fyear)
  rf_grid = expand.grid(mtry = 10:15,
                         splitrule = 'variance',
                         min.node.size = 5) #Default for regression is 5. Controls tree size.
  ctrl    = trainControl(method = 'oob', verboseIter = T)
  
  rf_gridsearch = train(value ~ ., 
                         data = train,
                         method = 'ranger',
                         num.trees = 2000,
                         verbose = T,
                         importance = 'impurity',
                         trControl = ctrl,
                         tuneGrid = rf_grid) #Here is the grid
}

plot(varImp(rf_gridsearch))
(varImportance = varImp(rf_gridsearch))
par(mfrow      = c(1, 1))
yhat           = predict(rf_gridsearch, test)
y              = test$value
(models_RMSE[1,1] = mean((y - yhat)^2))
# 35.6903

plot(yhat ~ y)
abline(0, 1, col = 'red')
cor(y, yhat)^2
# 0.574835

plot(y, type = 'l')
lines(yhat, col = 'red', type = 'l')

# How well does it perform in and out of season?

yIn     = (test %>% filter(season == "In"))$value
yOut    = (test %>% filter(season == "Out"))$value

yhatIn  = yhat[which(test$season == "In")]
yhatOut = yhat[which(test$season == "Out")]

(models_RMSE[1,2] = mean((yIn - yhatIn)^2))   # 68.3992
cor(yIn, yhatIn)^2       # 0.4778134
plot(yIn ~ yhatIn)
abline(0, 1, col = 'red')

(models_RMSE[1,3] = mean((yOut - yhatOut)^2)) # 11.13848
cor(yOut, yhatOut)^2     # 0.03024854
plot(yOut ~ yhatOut)
abline(0, 1, col = 'red')

# Now with categories

yhatMat    = as.data.frame(yhat, ncol = 1) 
colnames(yhatMat) = "value"
yhatCat    = dplyr::select(yhatMat) %>%
  mutate(value =  yhatMat) %>%
  mutate(cat = case_when(
    yhatMat < 1 ~ "Very low",
    yhatMat >= 1 & yhatMat <= 3 ~ "Low",
    yhatMat >= 3 & yhatMat <= 8 ~ "Moderate",
    yhatMat >= 8 & yhatMat <= 14.8 ~ "High",
    yhatMat >= 14.8 ~ "Very high"))
yhatCat$cat = as.factor(yhatCat$cat)

yMat    = as.data.frame(y, ncol = 1) 
colnames(yMat) = "value"
yCat    = dplyr::select(yMat) %>%
  mutate(value =  yMat) %>%
  mutate(cat = case_when(
    yMat < 1 ~ "Very low",
    yMat >= 1 & yMat <= 3 ~ "Low",
    yMat >= 3 & yMat <= 8 ~ "Moderate",
    yMat >= 8 & yMat <= 14.8 ~ "High",
    yMat >= 14.8 ~ "Very high"))
yCat$cat = as.factor(yCat$cat)

CM1 = confusionMatrix(yCat$cat, yhatCat$cat)


# 2. Model ----------
{
  # Create lag values
  rf_data <- dplyr::select(merged, -logvalue, -date, -Range) %>%
    mutate(count_cat_in = case_when(
      value < 1 ~ "Very low",
      value >= 1 & value <= 3 ~ "Low",
      value >= 3 & value <= 8 ~ "Moderate",
      value >= 8 & value <= 14.8 ~ "High",
      value >= 14.8 ~ "Very high")
    ) %>% dplyr::select(-count_cat_in) %>% na.omit() 
  
  
  train <- filter(rf_data, fyear != testYear)
  test <- filter(rf_data, fyear == testYear)
  
  summary(rf_data$fyear)
  rf_grid <- expand.grid(mtry = 10:15,
                         splitrule = 'variance',
                         min.node.size = 5) #Default for regression is 5. Controls tree size.
  ctrl <- trainControl(method = 'oob', verboseIter = T)
  
  rf_gridsearch <- train(value ~ ., 
                         data = train,
                         method = 'ranger',
                         num.trees = 2000,
                         verbose = T,
                         importance = 'impurity',
                         trControl = ctrl,
                         tuneGrid = rf_grid) #Here is the grid
}
plot(varImp(rf_gridsearch))
(varImportance = varImp(rf_gridsearch))
par(mfrow = c(1, 1))
yhat          = predict(rf_gridsearch, test)
y             = test$value
(models_RMSE[2,1] = mean((y - yhat)^2))
# 65.04818

cor(y, yhat)^2
plot(yhat ~ y)
abline(0, 1, col = 'red')
cor(y, yhat)

plot(y, type = 'l')
lines(yhat, col = 'red', type = 'l')

# How well does it perform in and out of season?

yIn     = (test %>% filter(season == "In"))$value
yOut    = (test %>% filter(season == "Out"))$value

yhatIn  = yhat[which(test$season == "In")]
yhatOut = yhat[which(test$season == "Out")]

(models_RMSE[2,2] = mean((yIn - yhatIn)^2))   # 126.2724
cor(yIn, yhatIn)^2       
plot(yIn ~ yhatIn)
abline(0, 1, col = 'red')

(models_RMSE[2,3] = mean((yOut - yhatOut)^2)) # 2.811988
cor(yOut, yhatOut)^2    
plot(yOut ~ yhatOut)
abline(0, 1, col = 'red')

# Now with categories

yhatMat    = as.data.frame(yhat, ncol = 1) 
colnames(yhatMat) = "value"
yhatCat    = dplyr::select(yhatMat) %>%
  mutate(value =  yhatMat) %>%
  mutate(cat = case_when(
    yhatMat < 1 ~ "Very low",
    yhatMat >= 1 & yhatMat <= 3 ~ "Low",
    yhatMat >= 3 & yhatMat <= 8 ~ "Moderate",
    yhatMat >= 8 & yhatMat <= 14.8 ~ "High",
    yhatMat >= 14.8 ~ "Very high"))
yhatCat$cat = as.factor(yhatCat$cat)

yMat    = as.data.frame(y, ncol = 1) 
colnames(yMat) = "value"
yCat    = dplyr::select(yMat) %>%
  mutate(value =  yMat) %>%
  mutate(cat = case_when(
    yMat < 1 ~ "Very low",
    yMat >= 1 & yMat <= 3 ~ "Low",
    yMat >= 3 & yMat <= 8 ~ "Moderate",
    yMat >= 8 & yMat <= 14.8 ~ "High",
    yMat >= 14.8 ~ "Very high"))
yCat$cat = as.factor(yCat$cat)

CM2 = confusionMatrix(yCat$cat, yhatCat$cat)

# 3. Model -----------

# Create lag values 
{
  rf_data <- dplyr::select(merged, -logvalue, -date, -Range) %>%
    mutate(count_lag1 = lag(value, 1),
           count_lag2 = lag(value, 2),
           count_lag3 = lag(value, 3),
           count_lag4 = lag(value, 4),
           count_lag5 = lag(value, 5)) %>%
    mutate(count_cat_in = case_when(
      value < 1 ~ "Very low",
      value >= 1 & value <= 3 ~ "Low",
      value >= 3 & value <= 8 ~ "Moderate",
      value >= 8 & value <= 14.8 ~ "High",
      value >= 14.8 ~ "Very high")
    ) %>% dplyr::select(-count_cat_in) %>% na.omit() 
  
  
  train <- filter(rf_data, fyear != testYear)
  test <- filter(rf_data, fyear == testYear)
  
  summary(rf_data$fyear)
  rf_grid <- expand.grid(mtry = 10:15,
                         splitrule = 'variance',
                         min.node.size = 5) #Default for regression is 5. Controls tree size.
  ctrl <- trainControl(method = 'oob', verboseIter = T)
  
  rf_gridsearch <- train(value ~ ., 
                         data = train,
                         method = 'ranger',
                         num.trees = 2000,
                         verbose = T,
                         importance = 'impurity',
                         trControl = ctrl,
                         tuneGrid = rf_grid) #Here is the grid
}

plot(varImp(rf_gridsearch))
(varImportance = varImp(rf_gridsearch))
par(mfrow = c(1, 1))
yhat          = predict(rf_gridsearch, test)
y             = test$value
(models_RMSE[3,1] = mean((y - yhat)^2))

cor(y, yhat)^2
plot(yhat ~ y)
abline(0, 1, col = 'red')
cor(y, yhat)

plot(y, type = 'l')
lines(yhat, col = 'red', type = 'l')

# How well does it perform in and out of season?

yIn     = (test %>% filter(season == "In"))$value
yOut    = (test %>% filter(season == "Out"))$value

yhatIn  = yhat[which(test$season == "In")]
yhatOut = yhat[which(test$season == "Out")]

(models_RMSE[3,2] = mean((yIn - yhatIn)^2))   # 68.3992
cor(yIn, yhatIn)^2       # 0.4778134
plot(yIn ~ yhatIn)
abline(0, 1, col = 'red')

(models_RMSE[3,3] = mean((yOut - yhatOut)^2)) # 11.13848
cor(yOut, yhatOut)^2     # 0.03024854
plot(yOut ~ yhatOut)
abline(0, 1, col = 'red')

# Now with categories

yhatMat    = as.data.frame(yhat, ncol = 1) 
colnames(yhatMat) = "value"
yhatCat    = dplyr::select(yhatMat) %>%
  mutate(value =  yhatMat) %>%
  mutate(cat = case_when(
    yhatMat < 1 ~ "Very low",
    yhatMat >= 1 & yhatMat <= 3 ~ "Low",
    yhatMat >= 3 & yhatMat <= 8 ~ "Moderate",
    yhatMat >= 8 & yhatMat <= 14.8 ~ "High",
    yhatMat >= 14.8 ~ "Very high"))
yhatCat$cat = as.factor(yhatCat$cat)

yMat    = as.data.frame(y, ncol = 1) 
colnames(yMat) = "value"
yCat    = dplyr::select(yMat) %>%
  mutate(value =  yMat) %>%
  mutate(cat = case_when(
    yMat < 1 ~ "Very low",
    yMat >= 1 & yMat <= 3 ~ "Low",
    yMat >= 3 & yMat <= 8 ~ "Moderate",
    yMat >= 8 & yMat <= 14.8 ~ "High",
    yMat >= 14.8 ~ "Very high"))
yCat$cat = as.factor(yCat$cat)

CM3 = confusionMatrix(yCat$cat, yhatCat$cat)

# 4. Model -----------

# Create lag values 
{
  rf_data <- dplyr::select(merged, -logvalue, -date, -Range) %>%
    mutate(count_lag1 = lag(value, 1),
           count_lag2 = lag(value, 2),
           count_lag3 = lag(value, 3),
           count_lag4 = lag(value, 4),
           count_lag5 = lag(value, 5),
           maxtemp.anom_lag1 = lag(maxtemp.anom, 1),
           maxtemp.anom_lag2 = lag(maxtemp.anom, 2),
           maxtemp.anom_lag3 = lag(maxtemp.anom, 3),
           maxtemp.anom_lag4 = lag(maxtemp.anom, 4),
           Visibility_lag1   = lag(Visibility, 1),
           Visibility_lag2   = lag(Visibility, 2),
           Visibility_lag3   = lag(Visibility, 3),
           Visibility_lag4   = lag(Visibility, 4),
           value.wind_speed_lag1    = lag(value.wind_speed, 1),
           value.wind_speed_lag2    = lag(value.wind_speed, 2),
           value.wind_speed_lag3    = lag(value.wind_speed, 3),
           value.humid_lag1         = lag(value.humid, 1),
           value.humid_lag2         = lag(value.humid, 2),
           value.humid_lag3         = lag(value.humid, 3)) %>% 
    mutate(wind_dir_bin = case_when(
      value.wind_dir > 100 & value.wind_dir < 200 ~ "dir1",
      value.wind_dir <= 100 | value.wind_dir >= 200 ~ "dir2"
    )) %>% mutate(count_cat_in = case_when(
      value < 1 ~ "Very low",
      value >= 1 & value <= 3 ~ "Low",
      value >= 3 & value <= 8 ~ "Moderate",
      value >= 8 & value <= 14.8 ~ "High",
      value >= 14.8 ~ "Very high")
    ) %>% dplyr::select(-count_cat_in) %>% na.omit() 
  
  
  train <- filter(rf_data, fyear != testYear)
  test <- filter(rf_data, fyear == testYear)
  
  summary(rf_data$fyear)
  rf_grid <- expand.grid(mtry = 10:15,
                         splitrule = 'variance',
                         min.node.size = 5) #Default for regression is 5. Controls tree size.
  ctrl <- trainControl(method = 'oob', verboseIter = T)
  
  rf_gridsearch <- train(value ~ ., 
                         data = train,
                         method = 'ranger',
                         num.trees = 2000,
                         verbose = T,
                         importance = 'impurity',
                         trControl = ctrl,
                         tuneGrid = rf_grid) #Here is the grid
}
plot(varImp(rf_gridsearch))
(varImportance = varImp(rf_gridsearch))
par(mfrow = c(1, 1))
yhat          = predict(rf_gridsearch, test)
y             = test$value
(models_RMSE[4,1] = mean((y - yhat)^2))

cor(y, yhat)^2
plot(yhat ~ y)
abline(0, 1, col = 'red')
cor(y, yhat)

plot(y, type = 'l')
lines(yhat, col = 'red', type = 'l')

# How well does it perform in and out of season?

yIn     = (test %>% filter(season == "In"))$value
yOut    = (test %>% filter(season == "Out"))$value

yhatIn  = yhat[which(test$season == "In")]
yhatOut = yhat[which(test$season == "Out")]

(models_RMSE[4,2] = mean((yIn - yhatIn)^2))   # 68.3992
cor(yIn, yhatIn)^2       # 0.4778134
plot(yIn ~ yhatIn)
abline(0, 1, col = 'red')

(models_RMSE[4,3] = mean((yOut - yhatOut)^2)) # 11.13848
cor(yOut, yhatOut)^2     # 0.03024854
plot(yOut ~ yhatOut)
abline(0, 1, col = 'red')

# Now with categories

yhatMat    = as.data.frame(yhat, ncol = 1) 
colnames(yhatMat) = "value"
yhatCat    = dplyr::select(yhatMat) %>%
  mutate(value =  yhatMat) %>%
  mutate(cat = case_when(
    yhatMat < 1 ~ "Very low",
    yhatMat >= 1 & yhatMat <= 3 ~ "Low",
    yhatMat >= 3 & yhatMat <= 8 ~ "Moderate",
    yhatMat >= 8 & yhatMat <= 14.8 ~ "High",
    yhatMat >= 14.8 ~ "Very high"))
yhatCat$cat = as.factor(yhatCat$cat)

yMat    = as.data.frame(y, ncol = 1) 
colnames(yMat) = "value"
yCat    = dplyr::select(yMat) %>%
  mutate(value =  yMat) %>%
  mutate(cat = case_when(
    yMat < 1 ~ "Very low",
    yMat >= 1 & yMat <= 3 ~ "Low",
    yMat >= 3 & yMat <= 8 ~ "Moderate",
    yMat >= 8 & yMat <= 14.8 ~ "High",
    yMat >= 14.8 ~ "Very high"))
yCat$cat = as.factor(yCat$cat)

CM4 = confusionMatrix(yCat$cat, yhatCat$cat)

# 5. Model -----------

# Create lag values 
{
  rf_data <- dplyr::select(merged, -logvalue, -date, -Range) %>%
    mutate(count_lag1 = lag(value, 1),
           count_lag2 = lag(value, 2),
           count_lag3 = lag(value, 3),
           count_lag4 = lag(value, 4),
           count_lag5 = lag(value, 5),
           maxtemp.anom_lag1 = lag(maxtemp.anom, 1),
           maxtemp.anom_lag2 = lag(maxtemp.anom, 2),
           maxtemp.anom_lag3 = lag(maxtemp.anom, 3),
           maxtemp.anom_lag4 = lag(maxtemp.anom, 4),
           Visibility_lag1   = lag(Visibility, 1),
           Visibility_lag2   = lag(Visibility, 2),
           Visibility_lag3   = lag(Visibility, 3),
           Visibility_lag4   = lag(Visibility, 4),
           value.wind_speed_lag1    = lag(value.wind_speed, 1),
           value.wind_speed_lag2    = lag(value.wind_speed, 2),
           value.wind_speed_lag3    = lag(value.wind_speed, 3),
           value.humid_lag1         = lag(value.humid, 1),
           value.humid_lag2         = lag(value.humid, 2),
           value.humid_lag3         = lag(value.humid, 3),
           VI_mean_lag1 = lag(Mean, 16),
           VI_mean_lag2 = lag(Mean, 32),
           VI_mean_lag3 = lag(Mean, 48),
           VI_max_lag1 = lag(Maximum, 16),
           VI_max_lag2 = lag(Maximum, 32),
           VI_max_lag3 = lag(Maximum, 48),
           VI_count_lag1 = lag(Count, 16),
           VI_count_lag2 = lag(Count, 32),
           VI_count_lag3 = lag(Count, 48),
           VI_up15_lag1 = lag(Upper.1.5.IQR, 16),
           VI_up15_lag2 = lag(Upper.1.5.IQR, 32),
           VI_up15_lag3 = lag(Upper.1.5.IQR, 48)) %>% 
    mutate(wind_dir_bin = case_when(
      value.wind_dir > 100 & value.wind_dir < 200 ~ "dir1",
      value.wind_dir <= 100 | value.wind_dir >= 200 ~ "dir2"
    )) %>% mutate(count_cat_in = case_when(
      value < 1 ~ "Very low",
      value >= 1 & value <= 3 ~ "Low",
      value >= 3 & value <= 8 ~ "Moderate",
      value >= 8 & value <= 14.8 ~ "High",
      value >= 14.8 ~ "Very high")
    ) %>% dplyr::select(-count_cat_in) %>% na.omit() 
  
  
  train <- filter(rf_data, fyear != testYear)
  test <- filter(rf_data, fyear == testYear)
  
  summary(rf_data$fyear)
  rf_grid <- expand.grid(mtry = 10:15,
                         splitrule = 'variance',
                         min.node.size = 5) #Default for regression is 5. Controls tree size.
  ctrl <- trainControl(method = 'oob', verboseIter = T)
  
  rf_gridsearch = train(value ~ ., 
                         data = train,
                         method = 'ranger',
                         num.trees = 2000,
                         verbose = T,
                         importance = 'impurity',
                         #probability = T,
                         trControl = ctrl,
                         tuneGrid = rf_grid) #Here is the grid
  # modelo_ranger = ranger(value ~., data = train,
  #                         num.trees = 1000, 
  #                         mtry = rf_gridsearch$bestTune[,1], 
  #                         write.forest = T, 
  #                         min.node.size = rf_gridsearch$bestTune[,3], 
  #                         probability = T)
  library(randomForest)
  rf = randomForest(value~., data = train,
                    mtry = rf_gridsearch$bestTune[,1],
                    min.node.size = rf_gridsearch$bestTune[,3],
                    norm.votes = TRUE, proximity = TRUE)
}

plot(varImp(rf_gridsearch))
(varImportance = varImp(rf_gridsearch))
par(mfrow = c(1, 1))
yhat          = predict(rf_gridsearch, test)
y             = test$value
(models_RMSE[5,1] = mean((y - yhat)^2))

cor(y, yhat)^2
plot(yhat ~ y)
abline(0, 1, col = 'red')
cor(y, yhat)

plot(y, type = 'l')
lines(yhat, col = 'red', type = 'l')

# How well does it perform in and out of season?

yIn     = (test %>% filter(season == "In"))$value
yOut    = (test %>% filter(season == "Out"))$value

yhatIn  = yhat[which(test$season == "In")]
yhatOut = yhat[which(test$season == "Out")]

(models_RMSE[5,2] = mean((yIn - yhatIn)^2))   # 68.3992
cor(yIn, yhatIn)^2       # 0.4778134
plot(yIn ~ yhatIn)
abline(0, 1, col = 'red')

(models_RMSE[5,3] = mean((yOut - yhatOut)^2)) # 11.13848
cor(yOut, yhatOut)^2     # 0.03024854
plot(yOut ~ yhatOut)
abline(0, 1, col = 'red')

# Now with categories

yhatMat    = as.data.frame(yhat, ncol = 1) 
colnames(yhatMat) = "value"
yhatCat    = dplyr::select(yhatMat) %>%
  mutate(value =  yhatMat) %>%
  mutate(cat = case_when(
    yhatMat < 1 ~ "Very low",
    yhatMat >= 1 & yhatMat <= 3 ~ "Low",
    yhatMat >= 3 & yhatMat <= 8 ~ "Moderate",
    yhatMat >= 8 & yhatMat <= 14.8 ~ "High",
    yhatMat >= 14.8 ~ "Very high"))
yhatCat$cat = as.factor(yhatCat$cat)

yMat    = as.data.frame(y, ncol = 1) 
colnames(yMat) = "value"
yCat    = dplyr::select(yMat) %>%
  mutate(value =  yMat) %>%
  mutate(cat = case_when(
    yMat < 1 ~ "Very low",
    yMat >= 1 & yMat <= 3 ~ "Low",
    yMat >= 3 & yMat <= 8 ~ "Moderate",
    yMat >= 8 & yMat <= 14.8 ~ "High",
    yMat >= 14.8 ~ "Very high"))
yCat$cat = as.factor(yCat$cat)

CM5 = confusionMatrix(yCat$cat, yhatCat$cat)


# Save workspace ------------
# save.image('RF_draft_workspace.RData')
# load('RF_draft_workspace.RData')


# GBM -------------
ctrl <- trainControl(method = 'cv', number = 10, verboseIter = T)
gbm_grid <- expand.grid(n.trees = c(300, 500, 1000), # Try these numbers of trees 
                        interaction.depth = c(1, 2, 6), # Try these interaction depths
                        shrinkage = c(0.01, 0.005, 0.001), # 
                        n.minobsinnode = 1) # Set min node size to 1

gbm_gridsearch <- train(value ~ ., data = train,
                        method = 'gbm', 
                        distribution = 'gaussian', # Because its a regression problem
                        trControl = ctrl, 
                        verbose = F, # Keeps me updated on the progress
                        tuneGrid = gbm_grid)

yhat_gbm <- predict(gbm_gridsearch, test)

data.frame(`GBM` = yhat_gbm, 'Random Forest' = yhat, Actual = y) %>%
  mutate(x = 1:nrow(.)) %>%
  melt(id.vars = 'x') %>% 
  rename('Pollen Count' = value, 'Model' = variable) %>%
  ggplot(aes(x, `Pollen Count`, colour = Model)) +
  geom_line() +
  facet_wrap(.~Model, nrow = 3) +
  xlab("Day Number") +
  theme_minimal()

plot(yhat_gbm ~ y)
cor(yhat_gbm, y)^2
abline(0, 1)
yhat_gbm2 = c(yhat_gbm[1:280], c(0))

cor(yhat_gbm2, y)^2

plot(yhat_gbm2, type = 'l')
lines(y, col = 'red')



# Weekly Predictions ----------
rf_data    = rf_data %>% mutate(pollen_cat = case_when(
  value < 1 ~ "Very low",
  value >= 1 & value <= 3 ~ "Low",
  value >= 3 & value <= 8 ~ "Moderate",
  value >= 8 & value <= 14.8 ~ "High",
  value >= 14.8 ~ "Very high")
) 

# inseason and outseason
in_season    = filter(rf_data, season == "In season")
out_season   = filter(rf_data, season == "Not in season")

# Isolate training and test periods
oneday_train = filter(rf_data, fyear != 2014)
oneday_test  = filter(rf_data, fyear == 2014)

best         = rf_gridsearch

# 1 day prediction --------

# oneday_predict = predict(best, newdata = oneday_test, predict.all = TRUE)
# oneday_predict <- predict(modelo_ranger, oneday_test, type = "prob")
oneday_predict = predict(rf, oneday_test, predict.all = TRUE)

oneday_predict$individual
hist(oneday_predict$individual[1,])

predict_cat = case_when(
  oneday_predict$aggregate < 1 ~ "Very low",
  oneday_predict$aggregate >= 1 & oneday_predict$aggregate < 3 ~ "Low",
  oneday_predict$aggregate >= 3 & oneday_predict$aggregate < 8 ~ "Moderate",
  oneday_predict$aggregate >= 8 & oneday_predict$aggregate < 14.8 ~ "High",
  oneday_predict$aggregate >= 14.8 ~ "Very high") %>%
  ordered(., levels = c("Very low", "Low", "Moderate", "High", "Very high"))

plot(predict_cat)

# Compare with observed
observed = oneday_test$value
observed_cat = oneday_test$pollen_cat %>% 
  ordered(., levels = c("Very low", "Low", "Moderate", "High", "Very high"))

confusionMatrix(observed_cat, predict_cat)

# 2 day prediction ----------

# count lag 1 becomes the prediction from previous day
# Counts are shifted back one ie new count lag 2 = old count lag 1

twoday_test = oneday_test %>% 
  mutate(oneday_predict = oneday_predict$aggregate) %>%
  mutate(count_lag1 = oneday_predict) %>%
  mutate(count_lag2 = oneday_test$count_lag1) %>%
  mutate(count_lag3 = oneday_test$count_lag2) %>%
  mutate(count_lag4 = oneday_test$count_lag3) %>%
  mutate(count_lag5 = oneday_test$count_lag4)

twoday_predict = predict(rf, twoday_test, predict.all = TRUE)
hist(twoday_predict$individual[1,])


# 3 day prediction ------------

# count lag 1 becomes the prediction from previous day
# Counts are shifted back one ie new count lag 2 = old count lag 1
threeday_test = twoday_test %>% 
  mutate(twoday_predict = twoday_predict$aggregate) %>%
  mutate(count_lag1 = twoday_predict) %>%
  mutate(count_lag2 = twoday_test$count_lag1) %>%
  mutate(count_lag3 = twoday_test$count_lag2) %>%
  mutate(count_lag4 = twoday_test$count_lag3) %>%
  mutate(count_lag5 = twoday_test$count_lag4)

threeday_predict = predict(rf, newdata = threeday_test, predict.all = TRUE)

# 4 day prediction ------------

# count lag 1 becomes the prediction from previous day
# Counts are shifted back one ie new count lag 2 = old count lag 1
fourday_test = threeday_test %>% 
  mutate(threeday_predict = threeday_predict$aggregate) %>%
  mutate(count_lag1 = threeday_predict) %>%
  mutate(count_lag2 = threeday_test$count_lag1) %>%
  mutate(count_lag3 = threeday_test$count_lag2) %>%
  mutate(count_lag4 = threeday_test$count_lag3) %>%
  mutate(count_lag5 = threeday_test$count_lag4)

fourday_predict = predict(rf, newdata = fourday_test, predict.all = TRUE)

# 5 day prediction ------------

# count lag 1 becomes the prediction from previous day
# Counts are shifted back one ie new count lag 2 = old count lag 1
fiveday_test = fourday_test %>% 
  mutate(fourday_predict = fourday_predict$aggregate) %>%
  mutate(count_lag1 = fourday_predict) %>%
  mutate(count_lag2 = fourday_test$count_lag1) %>%
  mutate(count_lag3 = fourday_test$count_lag2) %>%
  mutate(count_lag4 = fourday_test$count_lag3) %>%
  mutate(count_lag5 = fourday_test$count_lag4)

fiveday_predict = predict(rf, newdata = fiveday_test, predict.all = TRUE)

# 6 day prediction ------------

# count lag 1 becomes the prediction from previous day
# Counts are shifted back one ie new count lag 2 = old count lag 1
sixday_test = fiveday_test %>% 
  mutate(fiveday_predict = fiveday_predict$aggregate) %>%
  mutate(count_lag1 = fiveday_predict) %>%
  mutate(count_lag2 = fiveday_test$count_lag1) %>%
  mutate(count_lag3 = fiveday_test$count_lag2) %>%
  mutate(count_lag4 = fiveday_test$count_lag3) %>%
  mutate(count_lag5 = fiveday_test$count_lag4)

sixday_predict = predict(rf, newdata = sixday_test, predict.all = TRUE)

# 7 day prediction ------------

# count lag 1 becomes the prediction from previous day
# Counts are shifted back one ie new count lag 2 = old count lag 1
sevenday_test = sixday_test %>% 
  mutate(sixday_predict = sixday_predict$aggregate) %>%
  mutate(count_lag1 = sixday_predict) %>%
  mutate(count_lag2 = sixday_test$count_lag1) %>%
  mutate(count_lag3 = sixday_test$count_lag2) %>%
  mutate(count_lag4 = sixday_test$count_lag3) %>%
  mutate(count_lag5 = sixday_test$count_lag4)

sevenday_predict = predict(rf, newdata = sevenday_test, predict.all = TRUE)


plot(sevenday_predict$aggregate, type = 'l', main = '7-day', col = 'turquoise')
lines(oneday_predict$aggregate, type = 'l', main = '1-day', col = 'red')
lines(twoday_predict$aggregate, type = 'l', col = 'blue', main = '2-day')
lines(threeday_predict$aggregate, type = 'l', col = 'red', main = '3-day')
lines(fourday_predict$aggregate, type = 'l', main = '4-day', col= 'green')
lines(fiveday_predict$aggregate, type = 'l', col = 'orange', main = '5-day')
lines(sixday_predict$aggregate, type = 'l', col = 'violet', main = '6-day')
lines(observed, type = 'l', col = 'grey')

