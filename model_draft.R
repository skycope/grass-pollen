# Model draft

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
  ) %>% select(-Date)

ggplot(veg_index, aes(x = as.Date(Date), y = Mean)) +
  geom_line() +
  ylab("Vegetation Index") +
  xlab("Date") +
  theme_bw()

grassdata$date <- as.Date(grassdata$date )
merged <- merge(daily_veg, grassdata, by = 'date') 

pollen <- ggplot(merged, aes(x = as.Date(date))) +
  geom_line(aes(y = value), colour = 'darkblue') +
  ylab("Pollen Count") +
  xlab("") +
  theme_bw() +
  theme(legend.position = 'none')

vi <- ggplot(merged, aes(x = as.Date(date))) +
  geom_line(aes(y = Maximum, colour = 'darkred')) +
  ylab("Vegetation Index") +
  xlab("Date") +
  theme_bw() +
  theme(legend.position = 'none')

ggpubr::ggarrange(pollen, vi, ncol = 1)


rf_data <- select(merged, -logvalue, -date, -Range) %>%
  mutate(lag1_value = lag(value, 1),
         lag2_value = lag(value, 1),
         lag3_value = lag(value, 1),
         lag4_value = lag(value, 1),
         lag5_value = lag(value, 1),
         lag1_VI = lag(Mean, 16),
         lag2_VI = lag(Mean, 32),
         lag1Max = lag(Maximum, 16),
         lag2Max = lag(Maximum, 32),
         lag1_visibility = lag(Visibility, 1),
         lag2_visibility = lag(Visibility, 2)) %>% 
  mutate(wind_dir_bin = case_when(
    value.wind_dir > 100 & value.wind_dir < 200 ~ "dir1",
    value.wind_dir <= 100 | value.wind_dir >= 200 ~ "dir2"
  )) %>% mutate(value_cat = case_when(
    value == 0 ~ "Very low",
    value > 0 & value <= 5 ~ "Low",
    value > 5 & value <= 20 ~ "Moderate",
    value > 20 & value <= 200 ~ "High",
    value > 200 ~ "Very high")
  ) %>% select(-value_cat) %>% na.omit() 


train <- filter(rf_data, fyear != 2011)
test <- filter(rf_data, fyear == 2011)

summary(rf_data$fyear)
rf_grid <- expand.grid(mtry = 10:20,
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

plot(varImp(rf_gridsearch))
par(mfrow= c(1, 1))
yhat <- predict(rf_gridsearch, test)
y = test$value

#confusionMatrix(yhat, as.factor(y))

plot(yhat ~ y)
abline(0, 1, col = 'red')

plot(yhat, type = 'l')
lines(y, col = 'red', type = 'l')

dev.off()
cor(y, yhat)^2

### negative scale indicates that theta needs to be estimated

par(mfrow = c(2, 1))
mg2. <- gam(value ~ fyear +  s(ds, bs = "cc", by = fyear) + s(value.temp_max) + s(value.temp_min) + 
              value.rain + s(value.wind_speed) + value.humid + rain.lag1 + rain.lag2 +
              s(value.wind_dir, bs = "cc") + s(lag1_value, bs = 'cc') + s(Visibility, bs = 'cc') + 
              s(lag2_value, bs = 'cc') + s(Dew.Point, bs = 'cc') + s(Mean, bs = 'cc') + wind_dir_bin, family = nb(), data = rf_data, scale = -0.1)

(theta.est <- mg2.$family$getTheta(TRUE))


mg8 <- gam(value ~ fyear +  s(ds, bs = "cc", by = fyear) + s(value.temp_max) + s(value.temp_min) + 
             value.rain + s(value.wind_speed) + value.humid + rain.lag1 + rain.lag2 +
             s(value.wind_dir, bs = "cc") + s(lag1_value, bs = 'cc') + s(Visibility, bs = 'cc') + 
             s(lag2_value, bs = 'cc') + s(Dew.Point, bs = 'cc') + s(Mean, bs = 'cc') + wind_dir_bin, 
           family = negbin(theta.est), data = rf_data)

gam_predict <- exp(predict(mg8, test))
plot(yhat ~ y)
plot(gam_predict ~ y)

cor(gam_predict, y)^2

par(mfrow = c(2, 1))
plot(gam_predict, type = 'l')
plot(y, type = 'l')

data.frame(`GAM` = gam_predict, 'Random Forest' = yhat, Actual = y) %>% 
  mutate(x = 1:nrow(.)) %>%
  melt(id.vars = 'x') %>% 
  rename('Pollen Count' = value, 'Model' = variable) %>%
  ggplot(aes(x, `Pollen Count`, colour = Model)) +
  geom_line() +
  facet_wrap(.~Model, nrow = 3) +
  xlab("Day Number") +
  theme_minimal()

plot(gam_predict, type = 'l', main = 'GAM')
lines(y, col = 'blue')

plot(yhat, type = 'l', main = "Random Forest")
lines(y, col = 'blue')

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
