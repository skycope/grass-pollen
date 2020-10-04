rm(list=ls())
# Load required packages -------------
{
  library(randomForest)
}

# Load data set ---------
load(file = "API_B_format.Rda")

# Adapt data for tree based methods -----------
names(data)
str(data)
skim(data)
data$rain.lag1[1] = mean(data$rain.lag1[-1])
data$rain.lag2[1:2] = mean(data$rain.lag2[-c(1,2)])
# Remove day, month, year, Visibility (NAs), logvalue 
tree.data = data[,-c(10,13,14,15,18)]
names(tree.data)


# Training and testing data -------------
train         = which(tree.data$date<"2014-01-01")
data.train    = tree.data[train,]
data.test     = tree.data[-train,]
nrow(data.train)
nrow(data.test)

## Bagging Model 500 trees -----------------
set.seed(9)
bag_250_time <- system.time(
  bag <- randomForest(value ~ .-date, data = data.train, 
                            mtry = ncol(data.train) - 2,
                            ntree = 500, 
                            importance = TRUE, 
                            na.action = na.exclude, 
                            do.trace = 25)
)

## Random Forest 500 trees -------------------
set.seed(70)
rf_250_time <- system.time(
  rf <- randomForest(value ~ .-date, data = data.train,
                           ntree = 500, 
                           importance = TRUE, 
                           na.action = na.exclude,
                           do.trace = 25)
)

rf

# % Var explained ----------------
y <- data.train$value
yhat <- rf$predicted
(1 - sum((y - yhat)^2)/sum((y - mean(y))^2))*100
# Proportion of total variation in y accounted for by model


## Single tree for reference ------------
single <- randomForest(value ~ .-date, data = data.train, 
                           mtry = ncol(data.train) - 2,
                           ntree = 1, 
                           importance = T, 
                           na.action = na.exclude, 
                           keep.forest = T, 
                           replace = F)

single$forest$nrnodes #Huge tree! 


## Compare variable importance -------------
# par(mfrow = c(1,3))
# varImpPlot(bag, type = 1)
# varImpPlot(rf, type = 1)
# varImpPlot(single, type = 1)


## Compare OOB Errors -------------
par(mfrow = c(1,1))
plot(bag$mse, type = 'l', xlab = 'Number of trees', ylab = 'OOB MSE', 
     col = 'orange', lwd = 2, ylim = c(20, single$mse))
lines(rf$mse, col = 'red', lwd = 2)
abline(h = single$mse, lty = 2, col = 'darkgrey', lwd = 2)
legend('bottomright', legend = c('Bagging', 'Random Forest', 'Single Tree'), 
       col = c('orange', 'red', 'darkgrey'), lwd = 2, lty = c('solid', 'solid', 'dashed'),
       bty = "n")


## Predictions --------------- 
bag_pred <- predict(bag, newdata = data[-train, ])
rf_pred <- predict(rf, newdata = data[-train, ])
tree_pred <- predict(single, newdata = data[-train, ])


## Prediction accuracy
ytest <- data[-train, "value"]
bag_mse <- mean((ytest - bag_pred)^2)
rf_mse <- mean((ytest - rf_pred)^2)
tree_mse <- mean((ytest - tree_pred)^2)

## Plot Accuracy ------------------
{
  plot(rf$mse, type = 'l', xlab = 'Number of trees', ylab = 'MSE', 
       col = 'red', lwd = 2, ylim = c(20, max(rf$mse)))
  lines(bag$mse, col = 'orange', lwd = 2)
  abline(h = single$mse, col = 'darkgrey', lwd = 2)
  abline(h = bag_mse, col = 'orange', lty = 2, lwd = 2)
  abline(h = rf_mse, col = 'red', lty = 2, lwd = 2)
  abline(h = tree_mse, col = 'darkgrey', lty = 2, lwd = 2)
  legend(200,46, legend = c('Bagging OOB', 'Random Forest OOB', 'Lonely Tree OOB', "Testing MSE's"), 
         col=c('orange', 'red', 'darkgrey', 'black'), lwd = 2, lty = c('solid', 'solid', 'solid', 'dashed'),
         bty = "n")
}


