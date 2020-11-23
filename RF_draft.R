# RF Draft
# Using Data as at 2 November 2020

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

# Number of data points in each year

data %>%
  group_by(fyear) %>%
  summarise(no_rows = length(fyear))


# Initial Plots -------
ggplot(data, aes(x = as.Date(date), y = veg_index)) +
  geom_line() +
  ylab("Vegetation Index") +
  xlab("Date") +
  theme_bw()

data$date = as.Date(data$date)

pollen         = ggplot(data, aes(x = as.Date(date))) +
  geom_line(aes(y = pollen_count), colour = 'darkblue') +
  ylab("Pollen Count") +
  xlab("") +
  theme_bw() +
  theme(legend.position = 'none')

vi_max         = ggplot(data, aes(x = as.Date(date))) +
  geom_line(aes(y = veg_index, colour = 'darkred')) +
  ylab("Vegetation Index") +
  xlab("Date") +
  theme_bw() +
  theme(legend.position = 'none')

# ggpubr::ggarrange(pollen, vi_max, ncol = 1)

# Create Storage, set test year ------

train      = data[which(data$fyear=="2011"|data$fyear=="2012"),]

validation = data[which(data$fyear=="2014"),]
nrow(validation)
skim(validation)

test       = data[which(data$fyear=="2018"),]

models_RMSE               = matrix(NA, nrow = 5, ncol = 3)
models_accuracy           = matrix(NA, ncol = 4, nrow = 5)
colnames(models_RMSE)     = c("Total", "In Season", "Out of Season")
colnames(models_accuracy) = c("Accuracy for Reg to cats", "Accuracy straight to cats", "In Season","Out of Season")


# pdf("1 Year Counts.pdf", width=10, height=7)
plot(validation$pollen_count, type = 'l', xlab = "Days Since 1 January",
     ylab = "Pollen Count")
# dev.off()


# testYear = 2014

# Regression Models -----------
# Model 1; no veg index, no lags. Just weather data, ds and in/out seasin
# 1.1 Reg Model ----------
{
  M1_Reg_Data = dplyr::select(train, -logvalue, -date, - pollen_cat, -X,
                          -veg_index, - VI_count, - VI_minimum, - VI_maximum,
                          - VI_range, -VI_mean, - VI_sd, - VI_UQ, 
                          -VI_U1.5.IQR, -veg_index, -VI_L1.5.IQR, -VI_LQ, -visibility) #%>%

  rf_grid    = expand.grid(mtry = 10:15,
                         splitrule = 'variance',
                         min.node.size = 5) #Default for regression is 5. Controls tree size.
  ctrl       = trainControl(method = 'oob', verboseIter = T)
  
  M1_Reg     = train(pollen_count ~ ., 
                         data = M1_Reg_Data,
                         method = 'ranger',
                         num.trees = 2000,
                         verbose = T,
                         trainControl(method = "timeSlice"),
                         importance = 'impurity',
                         trControl = ctrl,
                         set.seed(2020),
                         tuneGrid = rf_grid) #Here is the grid
}

saveRDS(M1_Reg, "RF_saved/4YearsTraining/Models/M1_Reg.rds")

plot(varImp(M1_Reg)) 
# ds, maxtemp_anom, month, visibility, mintemp_anom
(varImportanceM1_Reg = varImp(M1_Reg))
saveRDS(varImportanceM1_Reg, "RF_saved/4YearsTraining/Models/M1_Reg_VarImp.rds")

par(mfrow      = c(1, 1))
yhat           = predict(M1_Reg, validation)
y              = validation$pollen_count
(models_RMSE[1,1] = mean((y - yhat)^2))
# 10.81856
library(modeest)
res <- residuals(naive(yhat))
autoplot(res) + xlab("Day") + ylab("") +
  ggtitle("Residuals from naïve method")

{
  pdf("./RF_saved/4YearsTraining/Images/M1_Reg_fitted.pdf", width=10, height=7)
  plot(yhat ~ y, pch = 16, col = alpha("black", 0.5))
  abline(0, 1, col = 'red')
  dev.off()
}

cor(y, yhat)^2
# 0.9047211

{
  pdf("./RF_saved/4YearsTraining/Images//M1_Reg_obsVsPred.pdf", width=10, height=7)
  plot(y, type = 'l')
  lines(yhat, col = 'red', type = 'l')
  legend("topleft", legend = c("Observed", "RF Predicted"),
         col = c("black", "red"), lty = c(1,1), bty = "n")
  dev.off()
}

# How well does it perform in and out of season?

yIn     = (validation %>% filter(season == "In season"))$pollen_count
yOut    = (validation %>% filter(season == "Not in season"))$pollen_count
yhatIn  = yhat[which(validation$season == "In season")]
yhatOut = yhat[which(validation$season == "Not in season")]

(models_RMSE[1,2] = mean((yIn - yhatIn)^2))   # 21.37
cor(yIn, yhatIn)^2       # 0.873
plot(yIn ~ yhatIn)
abline(0, 1, col = 'red')

(models_RMSE[1,3] = mean((yOut - yhatOut)^2)) # 0.265
cor(yOut, yhatOut)^2     # 0.799
plot(yOut ~ yhatOut)
abline(0, 1, col = 'red')

# Now with categories

yhatMat    = as.data.frame(yhat, ncol = 1) 
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

yMat    = as.data.frame(y, ncol = 1) 
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

CM1 = confusionMatrix(yhatCat$cat,yCat$cat)
saveRDS(CM1, "RF_saved/4YearsTraining/CMs/M1_Reg_CM.rds")
CM1$overall[1] # 0.764
models_accuracy[1,1] = CM1$overall['Accuracy']

# 1.2 Classification Model ---------

M1_Reg_Data_Class = dplyr::select(train, -logvalue, -date, - pollen_count, -X,
                           -veg_index, - VI_count, - VI_minimum, - VI_maximum,
                           - VI_range, -VI_mean, - VI_sd, - VI_UQ, 
                           -VI_U1.5.IQR, -veg_index, -VI_L1.5.IQR, -VI_LQ, - visibility) #%>%

rf_grid = expand.grid(mtry = 10:15,
                      splitrule = 'gini',
                      min.node.size = 5) #Default for regression is 5. Controls tree size.
ctrl    = trainControl(method = 'oob', verboseIter = T)

M1_Class = train(pollen_cat ~ ., 
                      data = M1_Reg_Data_Class,
                      method = 'ranger',
                      num.trees = 2000,
                      verbose = T,
                      importance = 'impurity',
                      set.seed(2020),
                      trControl = ctrl,
                      tuneGrid = rf_grid) #Here is the grid
saveRDS(M1_Class, "RF_saved/4YearsTraining/Models/M1_Class.rds")

plot(varImp(M1_Class))
# ds, wind_dir, max_temp, visability, humid
(varImportanceM1_Class = varImp(M1_Class))
saveRDS(varImportanceM1_Class, "RF_saved/4YearsTraining/Models/M1_Class_VarImp.rds")

yhat           = as.factor(predict(M1_Class, validation))
y              = as.factor(validation$pollen_cat)

CM1_Class = confusionMatrix(y, yhat)
saveRDS(CM1_Class, "RF_saved/4YearsTraining/CMs/M1_Class_CM.rds")
CM1_Class$overall[1] # 0.964
(models_accuracy[1,2] = CM1_Class$overall['Accuracy'])

# How well does it perform in and out of season?

yIn     = as.factor((validation %>% filter(season == "In season"))$pollen_cat)
yOut    = as.factor((validation %>% filter(season == "Not in season"))$pollen_cat)
yhatIn  = yhat[which(validation$season == "In season")]
yhatOut = yhat[which(validation$season == "Not in season")]

CM1_Class_In = confusionMatrix(yIn, yhatIn)
saveRDS(CM1_Class_In, "RF_saved/4YearsTraining/CMs/M1_Class_In_CM.rds")
CM1_Class_In$overall[1] # 0.957
(models_accuracy[1,3] = CM1_Class_In$overall['Accuracy'])


CM1_Class_Out = confusionMatrix(yOut, yhatOut)
saveRDS(CM1_Class_Out, "RF_saved/4YearsTraining/CMs/M1_Class_Out_CM.rds")
CM1_Class_Out$overall[1] # 0.971
(models_accuracy[1,4] = CM1_Class_Out$overall['Accuracy'])


# 2.1 Reg Model ----------
# Model 2: with veg index no lags
{
  # Create lag values
  M2_Reg_Data <- dplyr::select(train, -logvalue,
                           - pollen_cat, -date, -X,
                           - visibility, -VI_range) %>% 
                        na.omit() 
  
  rf_grid <- expand.grid(mtry = 10:15,
                         splitrule = 'variance',
                         min.node.size = 5) #Default for regression is 5. Controls tree size.
  ctrl <- trainControl(method = 'oob', verboseIter = T)
  
  M2_Reg <- train(pollen_count ~ ., 
                         data = M2_Reg_Data,
                         method = 'ranger',
                         num.trees = 2000,
                         verbose = T,
                         importance = 'impurity',
                         set.seed(2020),
                         trControl = ctrl,
                         tuneGrid = rf_grid) #Here is the grid
}
saveRDS(M2_Reg, "RF_saved/4YearsTraining/Models/M2_Reg.rds")

plot(varImp(M2_Reg))
(varImportanceM2_Reg = varImp(M2_Reg))
saveRDS(varImportanceM2_Reg, "RF_saved/4YearsTraining/Models/M2_Reg_VarImp.rds")
par(mfrow = c(1, 1))
yhat          = predict(M2_Reg, validation)
y             = validation$pollen_count
(models_RMSE[2,1] = mean((y - yhat)^2))

cor(y, yhat)
cor(y, yhat)^2

{
  pdf("RF_saved/4YearsTraining/Images/M2_Reg_fitted.pdf", width=10, height=7)
  plot(yhat ~ y, pch = 16, col = alpha("black", 0.5))
  abline(0, 1, col = 'red')
  dev.off()
}

{
  pdf("RF_saved/4YearsTraining/Images/M2_Reg_obsVsPred.pdf", width=10, height=7)
  plot(y, type = 'l')
  lines(yhat, col = 'red', type = 'l')
  legend("topleft", legend = c("Observed", "RF Predicted"),
         col = c("black", "red"), lty = c(1,1), bty = "n")
  dev.off()
}

# How well does it perform in and out of season?

yIn     = (validation %>% filter(season == "In season"))$pollen_count
yOut    = (validation %>% filter(season == "Not in season"))$pollen_count
yhatIn  = yhat[which(validation$season == "In season")]
yhatOut = yhat[which(validation$season == "Not in season")]

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

yMat    = as.data.frame(y, ncol = 1) 
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

CM2 = confusionMatrix(yhatCat$cat, yCat$cat)
saveRDS(CM2, "RF_saved/4YearsTraining/CMs/M2_Reg_CM.rds")
CM2$overall[1]
(models_accuracy[2,1] = CM2$overall['Accuracy'])

# 2.2 Classification Model ---------

M2_Reg_Data_Class = dplyr::select(train, -logvalue, 
                                  -date, - pollen_count, -X,
                                  -VI_range, -visibility) %>% na.omit()
rf_grid = expand.grid(mtry = 10:15,
                      splitrule = 'gini',
                      min.node.size = 5) #Default for regression is 5. Controls tree size.
ctrl    = trainControl(method = 'oob', verboseIter = T)

M2_Class = train(pollen_cat ~ ., 
                 data = M2_Reg_Data_Class,
                 method = 'ranger',
                 num.trees = 2000,
                 verbose = T,
                 importance = 'impurity',
                 set.seed(2020),
                 trControl = ctrl,
                 tuneGrid = rf_grid) #Here is the grid
saveRDS(M2_Class, "RF_saved/4YearsTraining/Models/M2_Class.rds")

plot(varImp(M2_Class))
(varImportanceM2_Class = varImp(M2_Class))
saveRDS(varImportanceM2_Class, "RF_saved/4YearsTraining/Models/M2_Class_VarImp.rds")
yhat           = as.factor(predict(M2_Class, validation))
y              = as.factor(validation$pollen_cat)

CM2_Class = confusionMatrix(y, yhat)
saveRDS(CM2_Class, "RF_saved/4YearsTraining/CMs/M2_Class_CM.rds")
(models_accuracy[2,2] = CM2_Class$overall['Accuracy'])


# How well does it perform in and out of season?

yIn     = as.factor((validation %>% filter(season == "In season"))$pollen_cat)
yOut    = as.factor((validation %>% filter(season == "Not in season"))$pollen_cat)

yhatIn  = as.factor(yhat[which(validation$season == "In season")])
yhatOut = as.factor(yhat[which(validation$season == "Not in season")])

CM2_Class_In = confusionMatrix(yIn, yhatIn)
saveRDS(CM2_Class_In, "RF_saved/4YearsTraining/CMs/M2_Class_In_CM.rds")
(models_accuracy[2,3] = CM2_Class_In$overall['Accuracy'])

CM2_Class_Out = confusionMatrix(yOut, yhatOut)
saveRDS(CM2_Class_Out, "RF_saved/4YearsTraining/CMs/M2_Class_Out_CM.rds")
(models_accuracy[2,4] = CM2_Class_Out$overall['Accuracy'])


# 3.1 Reg Model -----------
# Model 3 : add count lags
# Create lag values 
{
  M3_Reg_Data <- dplyr::select(train, -logvalue, -date, 
                               - pollen_cat, -X, -visibility, -VI_range) %>%
    mutate(count_lag1 = lag(pollen_count, 1),
           count_lag2 = lag(pollen_count, 2),
           count_lag3 = lag(pollen_count, 3),
           count_lag4 = lag(pollen_count, 4),
           count_lag5 = lag(pollen_count, 5)) %>% na.omit() 

  rf_grid <- expand.grid(mtry = 10:15,
                         splitrule = 'variance',
                         min.node.size = 5) #Default for regression is 5. Controls tree size.
  ctrl <- trainControl(method = 'oob', verboseIter = T)
  
  M3_Reg <- train(pollen_count ~ ., 
                         data = M3_Reg_Data,
                         method = 'ranger',
                         num.trees = 2000,
                         verbose = T,
                         importance = 'impurity',
                         set.seed(2020),
                         trControl = ctrl,
                         tuneGrid = rf_grid) #Here is the grid
}
saveRDS(M3_Reg, "RF_saved/4YearsTraining/Models/M3_Reg.rds")

validation_lags <- dplyr::select(validation, -logvalue) %>%
  mutate(count_lag1 = lag(pollen_count, 1),
         count_lag2 = lag(pollen_count, 2),
         count_lag3 = lag(pollen_count, 3),
         count_lag4 = lag(pollen_count, 4),
         count_lag5 = lag(pollen_count, 5)) %>% na.omit() 

plot(varImp(M3_Reg))
(varImportanceM3_Reg = varImp(M3_Reg))
saveRDS(varImportanceM3_Reg, "RF_saved/4YearsTraining/Models/M3_Reg_VarImp.rds")
par(mfrow = c(1, 1))
yhat          = predict(M3_Reg, validation_lags)
y             = validation_lags$pollen_count
(models_RMSE[3,1] = mean((y - yhat)^2))

cor(y, yhat)^2


{
  pdf("RF_saved/4YearsTraining/Images/M3_Reg_fitted.pdf", width=10, height=7)
  plot(yhat ~ y, pch = 16, col = alpha("black", 0.5))
  abline(0, 1, col = 'red')
  dev.off()
}

{
  pdf("RF_saved/4YearsTraining/Images/M3_Reg_obsVsPred.pdf", width=10, height=7)
  plot(y, type = 'l')
  lines(yhat, col = 'red', type = 'l')
  legend("topleft", legend = c("Observed", "RF Predicted"),
         col = c("black", "red"), lty = c(1,1), bty = "n")
  dev.off()
}

# How well does it perform in and out of season?

yIn     = (validation_lags %>% filter(season == "In season"))$pollen_count
yOut    = (validation_lags %>% filter(season == "Not in season"))$pollen_count
yhatIn  = yhat[which(validation_lags$season == "In season")]
yhatOut = yhat[which(validation_lags$season == "Not in season")]

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

yMat    = as.data.frame(y, ncol = 1) 
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

CM3 = confusionMatrix(yhatCat$cat, yCat$cat)
saveRDS(CM3, "RF_saved/4YearsTraining/CMs/M3_Reg_CM.rds")
(models_accuracy[3,1] = CM3$overall['Accuracy'])

# 3.2 Classification Model ---------

{
  M3_Reg_Data_Class <- dplyr::select(train, -logvalue, -date,
                                     - pollen_count, -X, -VI_range,
                                     -visibility) %>%
    mutate(cat_lag1 = lag(pollen_cat, 1),
           cat_lag2 = lag(pollen_cat, 2),
           cat_lag3 = lag(pollen_cat, 3),
           cat_lag4 = lag(pollen_cat, 4),
           cat_lag5 = lag(pollen_cat, 5)) %>% na.omit() 
  
  rf_grid <- expand.grid(mtry = 10:15,
                         splitrule = 'gini',
                         min.node.size = 5) #Default for regression is 5. Controls tree size.
  ctrl <- trainControl(method = 'oob', verboseIter = T)
  
  M3_Class <- train(pollen_cat ~ ., 
                  data = M3_Reg_Data_Class,
                  method = 'ranger',
                  num.trees = 2000,
                  verbose = T,
                  set.seed(2020),
                  importance = 'impurity',
                  trControl = ctrl,
                  tuneGrid = rf_grid) #Here is the grid
}
saveRDS(M3_Class, "RF_saved/4YearsTraining/Models/M3_Class.rds")


validation_lags <- dplyr::select(validation, -logvalue) %>%
  mutate(cat_lag1 = lag(pollen_cat, 1),
         cat_lag2 = lag(pollen_cat, 2),
         cat_lag3 = lag(pollen_cat, 3),
         cat_lag4 = lag(pollen_cat, 4),
         cat_lag5 = lag(pollen_cat, 5)) %>% na.omit() 

plot(varImp(M3_Class))
(varImportanceM3_Class = varImp(M3_Class))
saveRDS(varImportanceM3_Class, "RF_saved/4YearsTraining/Models/M3_Class_VarImp.rds")

yhat           = as.factor(predict(M3_Class, validation_lags))
y              = as.factor(validation_lags$pollen_cat)

CM3_Class = confusionMatrix(y, yhat)
saveRDS(CM3_Class, "RF_saved/4YearsTraining/CMs/M3_Class_CM.rds")
(models_accuracy[3,2] = CM3_Class$overall['Accuracy'])


# How well does it perform in and out of season?

yIn     = as.factor((validation_lags %>% filter(season == "In season"))$pollen_cat)
yOut    = as.factor((validation_lags %>% filter(season == "Not in season"))$pollen_cat)

yhatIn  = yhat[which(validation_lags$season == "In season")]
yhatOut = yhat[which(validation_lags$season == "Not in season")]

CM3_Class_In = confusionMatrix(yIn, yhatIn)
saveRDS(CM3_Class_In, "RF_saved/4YearsTraining/CMs/M3_Class_In_CM.rds")
(models_accuracy[3,3] = CM3_Class_In$overall['Accuracy'])

CM3_Class_Out = confusionMatrix(yOut, yhatOut)
saveRDS(CM3_Class_Out, "RF_saved/4YearsTraining/CMs/M3_Class_Out_CM.rds")
(models_accuracy[3,4] = CM3_Class_Out$overall['Accuracy'])

# 4.1 Reg Model -----------
# Model 4 : add weather variable lags
# Create lag values 
{
  M4_Reg_Data <- dplyr::select(train, -logvalue,
                           -date, - pollen_cat, -X,
                           -visibility, -VI_range) %>%
    mutate(count_lag1 = lag(pollen_count, 1),
           count_lag2 = lag(pollen_count, 2),
           count_lag3 = lag(pollen_count, 3),
           count_lag4 = lag(pollen_count, 4),
           count_lag5 = lag(pollen_count, 5),
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
           humid_lag3        = lag(humid, 3)) %>% 
    mutate(wind_dir_bin = case_when(
      wind_dir > 100 & wind_dir < 200 ~ "dir1",
      wind_dir <= 100 | wind_dir >= 200 ~ "dir2"
    ))  %>% na.omit() 
  
  rf_grid <- expand.grid(mtry = 10:15,
                         splitrule = 'variance',
                         min.node.size = 5) #Default for regression is 5. Controls tree size.
  ctrl <- trainControl(method = 'oob', verboseIter = T)
  
  M4_Reg <- train(pollen_count ~ ., 
                         data = M4_Reg_Data,
                         method = 'ranger',
                         num.trees = 2000,
                         verbose = T,
                         set.seed(2020),
                         importance = 'impurity',
                         trControl = ctrl,
                         tuneGrid = rf_grid) #Here is the grid
}
saveRDS(M4_Reg, "RF_saved/4YearsTraining/Models/M4_Reg.rds")



validation_lags <- dplyr::select(validation, -logvalue) %>%
  mutate(count_lag1 = lag(pollen_count, 1),
         count_lag2 = lag(pollen_count, 2),
         count_lag3 = lag(pollen_count, 3),
         count_lag4 = lag(pollen_count, 4),
         count_lag5 = lag(pollen_count, 5),
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
         humid_lag3        = lag(humid, 3)) %>% 
          mutate(wind_dir_bin = case_when(
            wind_dir > 100 & wind_dir < 200 ~ "dir1",
            wind_dir <= 100 | wind_dir >= 200 ~ "dir2"
          )) %>% na.omit() 
        

plot(varImp(M4_Reg))
(varImportanceM4_Reg = varImp(M4_Reg))
saveRDS(varImportanceM4_Reg, "RF_saved/4YearsTraining/Models/M4_Reg_VarImp.rds")
par(mfrow = c(1, 1))
yhat          = predict(M4_Reg, validation_lags)
y             = validation_lags$pollen_count
(models_RMSE[4,1] = mean((y - yhat)^2))

cor(y, yhat)^2

{
  pdf("RF_saved/4YearsTraining/Images/M4_Reg_fitted.pdf", width=10, height=7)
  plot(yhat ~ y, pch = 16, col = alpha("black", 0.5))
  abline(0, 1, col = 'red')
  dev.off()
}

{
  pdf("RF_saved/4YearsTraining/Images/M4_Reg_obsVsPred.pdf", width=10, height=7)
  plot(y, type = 'l')
  lines(yhat, col = 'red', type = 'l')
  legend("topleft", legend = c("Observed", "RF Predicted"),
         col = c("black", "red"), lty = c(1,1), bty = "n")
  dev.off()
}

# How well does it perform in and out of season?

yIn     = (validation_lags %>% filter(season == "In season"))$pollen_count
yOut    = (validation_lags %>% filter(season == "Not in season"))$pollen_count
yhatIn  = yhat[which(validation_lags$season == "In season")]
yhatOut = yhat[which(validation_lags$season == "Not in season")]

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

yMat    = as.data.frame(y, ncol = 1) 
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

CM4 = confusionMatrix(yhatCat$cat, yCat$cat)
saveRDS(CM4, "RF_saved/4YearsTraining/CMs/M4_Reg_CM.rds")
(models_accuracy[4,1] = CM4$overall['Accuracy'])

# 4.2 Classification Model ---------

{
  M4_Reg_Data_Class <- dplyr::select(train, -logvalue, -date, 
                                     - pollen_count, -X,-VI_range,
                                     -visibility) %>%
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
           humid_lag3        = lag(humid, 3)) %>% 
    mutate(wind_dir_bin = case_when(
      wind_dir > 100 & wind_dir < 200 ~ "dir1",
      wind_dir <= 100 | wind_dir >= 200 ~ "dir2"
    ))  %>% na.omit()
  
  rf_grid <- expand.grid(mtry = 10:15,
                         splitrule = 'gini',
                         min.node.size = 5) #Default for regression is 5. Controls tree size.
  ctrl <- trainControl(method = 'oob', verboseIter = T)
  
  M4_Class <- train(pollen_cat ~ ., 
                    data = M4_Reg_Data_Class,
                    method = 'ranger',
                    num.trees = 2000,
                    verbose = T,
                    set.seed(2020),
                    importance = 'impurity',
                    trControl = ctrl,
                    tuneGrid = rf_grid) #Here is the grid
}
saveRDS(M4_Class, "RF_saved/4YearsTraining/Models/M4_Class.rds")



validation_lags <- dplyr::select(validation, -logvalue) %>%
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
         humid_lag3        = lag(humid, 3)) %>% 
  mutate(wind_dir_bin = case_when(
    wind_dir > 100 & wind_dir < 200 ~ "dir1",
    wind_dir <= 100 | wind_dir >= 200 ~ "dir2"
  )) %>% na.omit() 

plot(varImp(M4_Class))
(varImportanceM4_Class = varImp(M4_Class))
saveRDS(varImportanceM4_Class, "RF_saved/4YearsTraining/Models/M4_Class_VarImp.rds")

yhat           = as.factor(predict(M4_Class, validation_lags))
y              = as.factor(validation_lags$pollen_cat)

CM4_Class = confusionMatrix(y, yhat)
saveRDS(CM4_Class, "RF_saved/4YearsTraining/CMs/M4_Class_CM.rds")
(models_accuracy[4,2] = CM4_Class$overall['Accuracy'])


# How well does it perform in and out of season?

yIn     = as.factor((validation_lags %>% filter(season == "In season"))$pollen_cat)
yOut    = as.factor((validation_lags %>% filter(season == "Not in season"))$pollen_cat)
yhatIn  = yhat[which(validation_lags$season == "In season")]
yhatOut = yhat[which(validation_lags$season == "Not in season")]

CM4_Class_In = confusionMatrix(yIn, yhatIn)
saveRDS(CM4_Class_In, "RF_saved/4YearsTraining/CMs/M4_Class_In_CM.rds")
(models_accuracy[4,3] = CM4_Class_In$overall['Accuracy'])

CM4_Class_Out = confusionMatrix(yOut, yhatOut)
saveRDS(CM4_Class_Out, "RF_saved/4YearsTraining/CMs/M4_Class_Out_CM.rds")
(models_accuracy[4,4] = CM4_Class_Out$overall['Accuracy'])

# 5.1 Reg Model -----------
# Model 5: Lag important VI values

# Create lag values 
{
  M5_Reg_Data <- dplyr::select(train, -logvalue,
                               -date, - pollen_cat, -X,
                               -visibility, -VI_range) %>%
    mutate(count_lag1 = lag(pollen_count, 1),
           count_lag2 = lag(pollen_count, 2),
           count_lag3 = lag(pollen_count, 3),
           count_lag4 = lag(pollen_count, 4),
           count_lag5 = lag(pollen_count, 5),
           maxtemp_anom_lag1 = lag(maxtemp_anom, 1),
           maxtemp_anom_lag2 = lag(maxtemp_anom, 2),
           maxtemp_anom_lag3 = lag(maxtemp_anom, 3),
           maxtemp_anom_lag4 = lag(maxtemp_anom, 4),
           # Visibility_lag1   = lag(Visibility, 1),
           # Visibility_lag2   = lag(Visibility, 2),
           # Visibility_lag3   = lag(Visibility, 3),
           # Visibility_lag4   = lag(Visibility, 4),
           wind_speed_lag1    = lag(wind_speed, 1),
           wind_speed_lag2    = lag(wind_speed, 2),
           wind_speed_lag3    = lag(wind_speed, 3),
           humid_lag1         = lag(humid, 1),
           humid_lag2         = lag(humid, 2),
           humid_lag3         = lag(humid, 3),
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
    ))  %>% na.omit() 
  
  rf_grid <- expand.grid(mtry = 10:15,
                         splitrule = 'variance',
                         min.node.size = 5) #Default for regression is 5. Controls tree size.
  ctrl <- trainControl(method = 'oob', verboseIter = T)
  
  M5_Reg = train(pollen_count ~ ., 
                         data = M5_Reg_Data,
                         method = 'ranger',
                         num.trees = 2000,
                         verbose = T,
                         importance = 'impurity',
                         #probability = T,
                         set.seed(2020),
                         trControl = ctrl,
                         tuneGrid = rf_grid) #Here is the grid

  # rf = randomForest(value~., data = train,
  #                   mtry = rf_gridsearch$bestTune[,1],
  #                   min.node.size = rf_gridsearch$bestTune[,3],
  #                   norm.votes = TRUE, proximity = TRUE)
}
saveRDS(M5_Reg, "RF_saved/4YearsTraining/Models/M5_Reg.rds")

validation_lags <- dplyr::select(validation, -logvalue) %>%
  mutate(count_lag1 = lag(pollen_count, 1),
         count_lag2 = lag(pollen_count, 2),
         count_lag3 = lag(pollen_count, 3),
         count_lag4 = lag(pollen_count, 4),
         count_lag5 = lag(pollen_count, 5),
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

plot(varImp(M5_Reg))
(varImportanceM5_Reg = varImp(M5_Reg))
saveRDS(varImportanceM5_Reg, "RF_saved/4YearsTraining/Models/M5_Reg_VarImp.rds")
par(mfrow = c(1, 1))
yhat          = predict(M5_Reg, validation_lags)
y             = validation_lags$pollen_count
(models_RMSE[5,1] = mean((y - yhat)^2))

cor(y, yhat)^2

{
  pdf("RF_saved/4YearsTraining/Images/M5_Reg_fitted.pdf", width=10, height=7)
  plot(yhat ~ y, pch = 16, col = alpha("black", 0.5))
  abline(0, 1, col = 'red')
  dev.off()
}

{
  pdf("RF_saved/4YearsTraining/Images/M5_Reg_obsVsPred.pdf", width=10, height=7)
  plot(y, type = 'l')
  lines(yhat, col = 'red', type = 'l')
  legend("topleft", legend = c("Observed", "RF Predicted"),
         col = c("black", "red"), lty = c(1,1), bty = "n")
  dev.off()
}

# How well does it perform in and out of season?

yIn     = (validation_lags %>% filter(season == "In season"))$pollen_count
yOut    = (validation_lags %>% filter(season == "Not in season"))$pollen_count
yhatIn  = yhat[which(validation_lags$season == "In season")]
yhatOut = yhat[which(validation_lags$season == "Not in season")]

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

yMat    = as.data.frame(y, ncol = 1) 
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

CM5 = confusionMatrix(yhatCat$cat, yCat$cat)
saveRDS(CM5, "RF_saved/4YearsTraining/CMs/M5_Reg_CM.rds")
(models_accuracy[5,1] = CM5$overall['Accuracy'])

# 5.2 Classification Model ---------

{
  M5_Reg_Data_Class <- dplyr::select(train, -logvalue, -date, 
                                     - pollen_count, -X,-VI_range,
                                     -visibility) %>%
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
    ))  %>% na.omit()
  
  rf_grid <- expand.grid(mtry = 10:15,
                         splitrule = 'gini',
                         min.node.size = 5) #Default for regression is 5. Controls tree size.
  ctrl <- trainControl(method = 'oob', verboseIter = T)
  
  M5_Class <- train(pollen_cat ~ ., 
                    data = M5_Reg_Data_Class,
                    method = 'ranger',
                    num.trees = 2000,
                    verbose = T,
                    set.seed(2020),
                    importance = 'impurity',
                    trControl = ctrl,
                    tuneGrid = rf_grid) #Here is the grid
}
saveRDS(M5_Class, "RF_saved/3YearsTraining/Models/M5_Class.rds")



validation_lags <- dplyr::select(validation, -logvalue) %>%
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

plot(varImp(M5_Class))
(varImportanceM5_Class = varImp(M5_Class))
saveRDS(varImportanceM5_Class, "RF_saved/4YearsTraining/Models/M5_Class_VarImp.rds")

yhat           = as.factor(predict(M5_Class, validation_lags))
y              = as.factor(validation_lags$pollen_cat)

CM5_Class = confusionMatrix(y, yhat)
saveRDS(CM5_Class, "RF_saved/4YearsTraining/CMs/M5_Class_CM.rds")
(models_accuracy[5,2] = CM5_Class$overall['Accuracy'])


# How well does it perform in and out of season?

yIn     = as.factor((validation_lags %>% filter(season == "In season"))$pollen_cat)
yOut    = as.factor((validation_lags %>% filter(season == "Not in season"))$pollen_cat)
yhatIn  = yhat[which(validation_lags$season == "In season")]
yhatOut = yhat[which(validation_lags$season == "Not in season")]

CM5_Class_In = confusionMatrix(yIn, yhatIn)
saveRDS(CM5_Class_In, "RF_saved/4YearsTraining/CMs/M5_Class_In_CM.rds")
(models_accuracy[5,3] = CM5_Class_In$overall['Accuracy'])

CM5_Class_Out = confusionMatrix(yOut, yhatOut)
saveRDS(CM5_Class_Out, "RF_saved/4YearsTraining/CMs/M5_Class_Out_CM.rds")
(models_accuracy[5,4] = CM5_Class_Out$overall['Accuracy'])

saveRDS(models_accuracy, "RF_saved/4YearsTraining/Models/ModelAccuracy2TY.rds")
saveRDS(models_RMSE, "RF_saved/4YearsTraining/Models/RMSE2TY.rds")

# Training set performance ---------

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

(tr_M1_Reg     = pred(M1_Reg, na.omit(train)))
(tr_M1_Class   = pred(M1_Class, na.omit(train)))
(tr_M2_Reg     = pred(M2_Reg, na.omit(train)))
(tr_M2_Class   = pred(M2_Class, na.omit(train)))
train_reg_lags =  dplyr::select(train, -logvalue) %>%
  mutate(count_lag1 = lag(pollen_count, 1),
         count_lag2 = lag(pollen_count, 2),
         count_lag3 = lag(pollen_count, 3),
         count_lag4 = lag(pollen_count, 4),
         count_lag5 = lag(pollen_count, 5),
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

train_class_lags =  dplyr::select(train, -logvalue) %>%
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
(tr_M3_Reg     = pred(M3_Reg, na.omit(train_reg_lags)))
(tr_M3_Class   = pred(M3_Class, na.omit(train_class_lags)))
(tr_M4_Reg     = pred(M4_Reg, na.omit(train_reg_lags)))
(tr_M4_Class   = pred(M4_Class, na.omit(train_class_lags)))
(tr_M5_Reg     = pred(M5_Reg, na.omit(train_reg_lags)))
(tr_M5_Class   = pred(M5_Class, na.omit(train_class_lags)))

training_accuracy           = matrix(NA, ncol = 2, nrow = 5)
colnames(training_accuracy) = c("Regression Model Accuracy", "Classification Model accuracy")
training_accuracy[1,1] = tr_M1_Reg
training_accuracy[1,2] = tr_M1_Class
training_accuracy[2,1] = tr_M2_Reg
training_accuracy[2,2] = tr_M2_Class
training_accuracy[3,1] = tr_M3_Reg
training_accuracy[3,2] = tr_M3_Class
training_accuracy[4,1] = tr_M4_Reg
training_accuracy[4,2] = tr_M4_Class
training_accuracy[5,1] = tr_M5_Reg
training_accuracy[5,2] = tr_M5_Class

saveRDS(training_accuracy, "RF_saved/4YearsTraining/Models/TrainingAccuracy2TY.rds")

colMeans(models_accuracy)
# Save workspace ------------
# save.image('RF_draft_workspace.RData')
# load('RF_draft_workspace.RData')

# # GBM -------------
# ctrl <- trainControl(method = 'cv', number = 10, verboseIter = T)
# gbm_grid <- expand.grid(n.trees = c(300, 500, 1000), # Try these numbers of trees 
#                         interaction.depth = c(1, 2, 6), # Try these interaction depths
#                         shrinkage = c(0.01, 0.005, 0.001), # 
#                         n.minobsinnode = 1) # Set min node size to 1
# 
# gbm_gridsearch <- train(value ~ ., data = train,
#                         method = 'gbm', 
#                         distribution = 'gaussian', # Because its a regression problem
#                         trControl = ctrl, 
#                         verbose = F, # Keeps me updated on the progress
#                         tuneGrid = gbm_grid)
# 
# yhat_gbm <- predict(gbm_gridsearch, test)
# 
# data.frame(`GBM` = yhat_gbm, 'Random Forest' = yhat, Actual = y) %>%
#   mutate(x = 1:nrow(.)) %>%
#   melt(id.vars = 'x') %>% 
#   rename('Pollen Count' = value, 'Model' = variable) %>%
#   ggplot(aes(x, `Pollen Count`, colour = Model)) +
#   geom_line() +
#   facet_wrap(.~Model, nrow = 3) +
#   xlab("Day Number") +
#   theme_minimal()
# 
# plot(yhat_gbm ~ y)
# cor(yhat_gbm, y)^2
# abline(0, 1)
# yhat_gbm2 = c(yhat_gbm[1:280], c(0))
# 
# cor(yhat_gbm2, y)^2
# 
# plot(yhat_gbm2, type = 'l')
# lines(y, col = 'red')
# 
# 
# 
# # Weekly Predictions ----------
# rf_data    = rf_data %>% mutate(pollen_cat = case_when(
#   value < 1 ~ "Very low",
#   value >= 1 & value <= 3 ~ "Low",
#   value >= 3 & value <= 8 ~ "Moderate",
#   value >= 8 & value <= 14.8 ~ "High",
#   value >= 14.8 ~ "Very high")
# ) 
# 
# # inseason and outseason
# in_season    = filter(rf_data, season == "In season")
# out_season   = filter(rf_data, season == "Not in season")
# 
# # Isolate training and test periods
# oneday_train = filter(rf_data, fyear != 2014)
# oneday_test  = filter(rf_data, fyear == 2014)
# 
# best         = rf_gridsearch
# 
# # 1 day prediction Reg --------
# 
# # oneday_predict = predict(best, newdata = oneday_test, predict.all = TRUE)
# # oneday_predict <- predict(modelo_ranger, oneday_test, type = "prob")
# oneday_predict = predict(rf, oneday_test, predict.all = TRUE)
# 
# oneday_predict$individual
# hist(oneday_predict$individual[1,])
# 
# predict_cat = case_when(
#   oneday_predict$aggregate < 1 ~ "Very low",
#   oneday_predict$aggregate >= 1 & oneday_predict$aggregate < 3 ~ "Low",
#   oneday_predict$aggregate >= 3 & oneday_predict$aggregate < 8 ~ "Moderate",
#   oneday_predict$aggregate >= 8 & oneday_predict$aggregate < 14.8 ~ "High",
#   oneday_predict$aggregate >= 14.8 ~ "Very high") %>%
#   ordered(., levels = c("Very low", "Low", "Moderate", "High", "Very high"))
# 
# plot(predict_cat)
# 
# # Compare with observed
# observed = oneday_test$value
# observed_cat = oneday_test$pollen_cat %>% 
#   ordered(., levels = c("Very low", "Low", "Moderate", "High", "Very high"))
# 
# confusionMatrix(observed_cat, predict_cat)
# 
# # 2 day prediction ----------
# 
# # count lag 1 becomes the prediction from previous day
# # Counts are shifted back one ie new count lag 2 = old count lag 1
# 
# twoday_test = oneday_test %>% 
#   mutate(oneday_predict = oneday_predict$aggregate) %>%
#   mutate(count_lag1 = oneday_predict) %>%
#   mutate(count_lag2 = oneday_test$count_lag1) %>%
#   mutate(count_lag3 = oneday_test$count_lag2) %>%
#   mutate(count_lag4 = oneday_test$count_lag3) %>%
#   mutate(count_lag5 = oneday_test$count_lag4)
# 
# twoday_predict = predict(rf, twoday_test, predict.all = TRUE)
# hist(twoday_predict$individual[1,])
# 
# 
# # 3 day prediction ------------
# 
# # count lag 1 becomes the prediction from previous day
# # Counts are shifted back one ie new count lag 2 = old count lag 1
# threeday_test = twoday_test %>% 
#   mutate(twoday_predict = twoday_predict$aggregate) %>%
#   mutate(count_lag1 = twoday_predict) %>%
#   mutate(count_lag2 = twoday_test$count_lag1) %>%
#   mutate(count_lag3 = twoday_test$count_lag2) %>%
#   mutate(count_lag4 = twoday_test$count_lag3) %>%
#   mutate(count_lag5 = twoday_test$count_lag4)
# 
# threeday_predict = predict(rf, newdata = threeday_test, predict.all = TRUE)
# 
# # 4 day prediction ------------
# 
# # count lag 1 becomes the prediction from previous day
# # Counts are shifted back one ie new count lag 2 = old count lag 1
# fourday_test = threeday_test %>% 
#   mutate(threeday_predict = threeday_predict$aggregate) %>%
#   mutate(count_lag1 = threeday_predict) %>%
#   mutate(count_lag2 = threeday_test$count_lag1) %>%
#   mutate(count_lag3 = threeday_test$count_lag2) %>%
#   mutate(count_lag4 = threeday_test$count_lag3) %>%
#   mutate(count_lag5 = threeday_test$count_lag4)
# 
# fourday_predict = predict(rf, newdata = fourday_test, predict.all = TRUE)
# 
# # 5 day prediction ------------
# 
# # count lag 1 becomes the prediction from previous day
# # Counts are shifted back one ie new count lag 2 = old count lag 1
# fiveday_test = fourday_test %>% 
#   mutate(fourday_predict = fourday_predict$aggregate) %>%
#   mutate(count_lag1 = fourday_predict) %>%
#   mutate(count_lag2 = fourday_test$count_lag1) %>%
#   mutate(count_lag3 = fourday_test$count_lag2) %>%
#   mutate(count_lag4 = fourday_test$count_lag3) %>%
#   mutate(count_lag5 = fourday_test$count_lag4)
# 
# fiveday_predict = predict(rf, newdata = fiveday_test, predict.all = TRUE)
# 
# # 6 day prediction ------------
# 
# # count lag 1 becomes the prediction from previous day
# # Counts are shifted back one ie new count lag 2 = old count lag 1
# sixday_test = fiveday_test %>% 
#   mutate(fiveday_predict = fiveday_predict$aggregate) %>%
#   mutate(count_lag1 = fiveday_predict) %>%
#   mutate(count_lag2 = fiveday_test$count_lag1) %>%
#   mutate(count_lag3 = fiveday_test$count_lag2) %>%
#   mutate(count_lag4 = fiveday_test$count_lag3) %>%
#   mutate(count_lag5 = fiveday_test$count_lag4)
# 
# sixday_predict = predict(rf, newdata = sixday_test, predict.all = TRUE)
# 
# # 7 day prediction ------------
# 
# # count lag 1 becomes the prediction from previous day
# # Counts are shifted back one ie new count lag 2 = old count lag 1
# sevenday_test = sixday_test %>% 
#   mutate(sixday_predict = sixday_predict$aggregate) %>%
#   mutate(count_lag1 = sixday_predict) %>%
#   mutate(count_lag2 = sixday_test$count_lag1) %>%
#   mutate(count_lag3 = sixday_test$count_lag2) %>%
#   mutate(count_lag4 = sixday_test$count_lag3) %>%
#   mutate(count_lag5 = sixday_test$count_lag4)
# 
# sevenday_predict = predict(rf, newdata = sevenday_test, predict.all = TRUE)
# 
# 
# plot(sevenday_predict$aggregate, type = 'l', main = '7-day', col = 'turquoise')
# lines(oneday_predict$aggregate, type = 'l', main = '1-day', col = 'red')
# lines(twoday_predict$aggregate, type = 'l', col = 'blue', main = '2-day')
# lines(threeday_predict$aggregate, type = 'l', col = 'red', main = '3-day')
# lines(fourday_predict$aggregate, type = 'l', main = '4-day', col= 'green')
# lines(fiveday_predict$aggregate, type = 'l', col = 'orange', main = '5-day')
# lines(sixday_predict$aggregate, type = 'l', col = 'violet', main = '6-day')
# lines(observed, type = 'l', col = 'grey')
# 
