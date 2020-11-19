rm(list = ls())

setwd("/Users/chloestipinovich/Documents/2020/Thesis Project/grass-pollen")

readRDS("RF_saved/2YearsTraining/Models/ModelAccuracy2TY.rds")
readRDS("RF_saved/2YearsTraining/Models/TrainingAccuracy2TY.rds")


readRDS("RF_saved/3YearsTraining/Models/ModelAccuracy2TY.rds")
readRDS("RF_saved/3YearsTraining/Models/TrainingAccuracy2TY.rds")

readRDS("RF_saved/4YearsTraining/Models/ModelAccuracy2TY.rds")
readRDS("RF_saved/4YearsTraining/Models/TrainingAccuracy2TY.rds")

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
  library(skimr)
}

rm(list = ls())

setwd("/Users/chloestipinovich/Documents/2020/Thesis Project/grass-pollen")
#setwd("/Users/skycope/Documents/UCT/Stats\ Honours/Project/Data\ and\ code")

# Read in data ----------
data      = read.csv("Final_Data/data_complete.csv")
data      = as.data.frame(data)
names(data)
head(data)

data      = dplyr::select(data, -File.Name, -Dataset, -aid, -Variance)
data      = rename(data,  VI_count = Count, VI_minimum = Minimum, VI_maximum = Maximum,
                   VI_range = Range, VI_mean = Mean, VI_sd = Standard.Deviation,
                   VI_UQ = Upper.Quartile,VI_U1.5.IQR = Upper.1.5.IQR, VI_L1.5.IQR = Lower.1.5.IQR,
                   VI_LQ = Lower.Quartile)
names(data)

pred      = function(model, data){
  p = predict(model, data)
  if (class(p)=="numeric"){
    yhatMat    = as.data.frame(p, ncol = 1) 
    colnames(yhatMat) = "pollen_count"
    yhatCat    = dplyr::select(yhatMat) %>%
      mutate(pollen_count =  yhatMat) %>%
      mutate(cat = case_when(
        yhatMat < 1 ~ "Very low",
        yhatMat >= 1 & yhatMat <= 3 ~ "Low",
        yhatMat >= 3 & yhatMat <= 8 ~ "Moderate",
        yhatMat >= 8 & yhatMat <= 14.8 ~ "High",
        yhatMat >= 14.8 ~ "Very high"))
    yhatCat$cat = as.factor(yhatCat$cat)
    
    yMat    = as.data.frame(data$pollen_count, ncol = 1) 
    colnames(yMat) = "pollen_count"
    yCat    = dplyr::select(yMat) %>%
      mutate(pollen_count =  yMat) %>%
      mutate(cat = case_when(
        yMat < 1 ~ "Very low",
        yMat >= 1 & yMat <= 3 ~ "Low",
        yMat >= 3 & yMat <= 8 ~ "Moderate",
        yMat >= 8 & yMat <= 14.8 ~ "High",
        yMat >= 14.8 ~ "Very high"))
    yCat$cat = as.factor(yCat$cat)
    
    cm = confusionMatrix(table(yCat$cat, yhatCat$cat))
    return(cm$overall['Accuracy'])
  }
  if (class(p) == "factor"){
    p  = as.factor(p)
    cm = confusionMatrix( table(p,data$pollen_cat))
    return(cm$overall['Accuracy'])
  }
}

test       = data[which(data$fyear=="2018"),]
test =  dplyr::select(test, -logvalue) %>%
  mutate(cat_lag1 = lag(pollen_cat, 1),
         cat_lag2 = lag(pollen_cat, 2),
         cat_lag3 = lag(pollen_cat, 3),
         cat_lag4 = lag(pollen_cat, 4),
         cat_lag5 = lag(pollen_cat, 5),
         maxtemp_anom_lag1 = lag(maxtemp_anom, 1),
         maxtemp_anom_lag2 = lag(maxtemp_anom, 2),
         maxtemp_anom_lag3 = lag(maxtemp_anom, 3),
         maxtemp_anom_lag4 = lag(maxtemp_anom, 4),
         # visibility_lag1   = lag(visibility, 1),
         # visibility_lag2   = lag(visibility, 2),
         # visibility_lag3   = lag(visibility, 3),
         # visibility_lag4   = lag(visibility, 4),
         wind_speed_lag1   = lag(wind_speed, 1),
         wind_speed_lag2   = lag(wind_speed, 2),
         wind_speed_lag3   = lag(wind_speed, 3),
         humid_lag1        = lag(humid, 1),
         humid_lag2        = lag(humid, 2),
         humid_lag3        = lag(humid, 3),
         VI_mean_lag1 = lag(VI_mean, 16),
         VI_mean_lag2 = lag(VI_mean, 32),
         VI_mean_lag3 = lag(VI_mean, 48),
         VI_max_lag1 = lag(VI_maximum, 16),
         VI_max_lag2 = lag(VI_maximum, 32),
         VI_max_lag3 = lag(VI_maximum, 48),
         VI_count_lag1 = lag(VI_count, 16),
         VI_count_lag2 = lag(VI_count, 32),
         VI_count_lag3 = lag(VI_count, 48),
         VI_up15_lag1 = lag(VI_U1.5.IQR, 16),
         VI_up15_lag2 = lag(VI_U1.5.IQR, 32),
         VI_up15_lag3 = lag(VI_U1.5.IQR, 48)) %>% 
  mutate(wind_dir_bin = case_when(
    wind_dir > 100 & wind_dir < 200 ~ "dir1",
    wind_dir <= 100 | wind_dir >= 200 ~ "dir2"
  )) %>% na.omit() 

best       = readRDS("RF_saved/3YearsTraining/Models/M5_Class.rds")
yhat       = predict(best, test)
acc        = pred(best, test)
acc

# How well does it perform in and out of season?

yIn     = as.factor((test %>% filter(season == "In season"))$pollen_cat)
yOut    = as.factor((test %>% filter(season == "Not in season"))$pollen_cat)
yhatIn  = yhat[which(test$season == "In season")]
yhatOut = yhat[which(test$season == "Not in season")]

CM = confusionMatrix(table(yIn, yhatIn))
(IN = CM$overall['Accuracy'])

CM = confusionMatrix(yOut, yhatOut)
(OUT = CM$overall['Accuracy'])

confusionMatrix(table(yhat, test$pollen_cat))


