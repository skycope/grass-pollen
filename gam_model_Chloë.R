rm(list=ls())

# Install required packages ------------
{
  library(dplyr)
  library(ggplot2)
  library(RColorBrewer)
  library(mgcv)
  library(quantmod)            # for finding peaks
  library(splines)
  library(mgcv)
  library(visreg)
  library(zoo)
  library(skimr)
  library(MASS)
}

# Load API Data -----------
load('countAPI.RData')
head(data)

# Adjust data to suite Birgit's layout ---------
# Create ds variable
data$day <- substr(data$date, 9, 10)
data$month <- substr(data$date, 6, 7)
data$year <- substr(data$date, 1, 4)
data$fyear <- as.factor(data$year)
head(data)
data <- mutate(data, ds = as.numeric(date - as.Date(paste(year,"-01-01", sep = ""))) + 1)

head(data)
str(data)
names(data)

# Make variable names the same as in Birgit's code for variables that are equal
names(data) <- c("date",
                 "value.temp_min",
                 "value.temp_max",
                 "Temperature",
                 "Dew.Point",
                 "value.humid",
                 "value.rain",
                 "value.wind_speed",
                 "value.wind_dir",
                 "Visibility",
                 "Conditions",
                 "value",
                 "day",
                 "month",
                 "year",
                 "fyear",
                 "ds"
                 )
names(data)

# Create log value variable
data$logvalue <- log(data$value+1)

# Checking for missing data
data <- data[order(data$date),]
skim(data)
# All missing data is Visibility predictor

# Create lagged rainfall
data$rain.lag1 <- lag(data$value.rain)
data$rain.lag2 <- lag(data$value.rain, n = 2)
head(data)

# Autocorrelation, scatter plots ------------

# Scatter plots and value against date plot
plot(data, pch = 19, cex = 0.4)
plot(value ~ date, data = data, pch='.')
plot(value ~ date, data = data, type = "l",
     col = "darkgreen", las = 1, ylab = "pollen count", xlab = "")

# Check auto correlation lags
acf((data$value[200:350]))
acf(data$value[755:830])
acf((data$value), lag.max = 6000)
#data$value

# Actually not sure what these variables are...
Lmin <- expression("minimum temperature ("~degree~C~")")
Lmax <- expression("maximum temperature ("~degree~C~")")

par(mfrow = c(3, 2))
with(data, {
  scatter.smooth(log(value + 1) ~ value.temp_max, pch = 19, cex = 0.6, las = 1, 
                 xlab = Lmax, lpars = list(col = "red", lwd = 2), 
                 ylab = "ln(count + 1)")
  scatter.smooth(log(value + 1) ~ value.temp_min, pch = 19, cex = 0.6, las = 1, 
                 xlab = Lmin, lpars = list(col = "red", lwd = 2), 
                 ylab = "ln(count + 1)")
  scatter.smooth(log(value + 1) ~ value.rain, pch = 19, cex = 0.6, las = 1, 
                 xlab = "rain (mm)", lpars = list(col = "red", lwd = 2), 
                 ylab = "ln(count + 1)")
  scatter.smooth(log(value + 1) ~ value.humid, pch = 19, cex = 0.6, las = 1, 
                 xlab = "humidity (%)", lpars = list(col = "red", lwd = 2), 
                 ylab = "ln(count + 1)")
  scatter.smooth(log(value + 1) ~ value.wind_speed, pch = 19, cex = 0.6, las = 1, 
                 xlab = "wind speed (m/s)", lpars = list(col = "red", lwd = 2), 
                 ylab = "ln(count + 1)")
  scatter.smooth(log(value + 1) ~ value.wind_dir, pch = 19, cex = 0.6, las = 1, 
                 xlab = "wind direction", lpars = list(col = "red", lwd = 2), 
                 ylab = "ln(count + 1)")
})

# Cross-correlation ------------------------
# CCF on differenced time series
pollen <- ts(diff(data$value))
hum <- ts(diff(data$value.humid))
tmp <- ts(diff(data$value.temp_max))
rain <- ts(diff(data$value.rain))
wsp <- ts(diff(data$value.wind_speed))

par(mfrow = c(2, 2))
ccf(wsp, pollen, na.action = na.pass)
ccf(hum, pollen, na.action = na.pass)
ccf(tmp, pollen, na.action = na.pass)
ccf(rain, pollen, na.action = na.pass)

### crosscorrelation for original time series

pollen <- ts((data$value))
hum <- ts((data$value.humid))
tmp <- ts((data$value.temp_max))
rain <- ts((data$value.rain))
wsp <- ts((data$value.wind_speed))

ccf(wsp, pollen, na.action = na.pass)
ccf(hum, pollen, na.action = na.pass)
ccf(tmp, pollen, na.action = na.pass)
ccf(rain, pollen, na.action = na.pass)

# Coloured plot of all variables -------------

ggplot(data = data, aes(x = date)) +
  geom_line(aes(y = value.humid, col = "Humidity")) +
  geom_line(aes(y = value.temp_max, col = "Max. Temp.")) +
  geom_line(aes(y = value.wind_speed+40, col = "Wind speed")) +
  geom_point(aes(y = value.rain, col = "Rain (mm)")) +
  geom_line(aes(y = sqrt(value), col = "Poaceae")) +
  ylab("sqrt(count)") +
  scale_colour_manual("", 
                      breaks = c("Poaceae", "Humidity", "Max. Temp.", "Wind speed", "Rain (mm)"),
                      values = c("Poaceae" = "black", "Humidity" = "orange", "Max. Temp." = "red", 
                                 "Wind speed" = "green", "Rain (mm)" = "blue") ) +
  theme(text = element_text(size = 20)) +
  theme_bw()


# Calculate anomalies of weather variables -----

names(data)
# Min temperature
spl.mintemp <- gam(value.temp_min ~ s(ds, bs = "cc"), data = data)

par(mfrow = c(1, 1))
plot(spl.mintemp, ylim=c(-5,19), ylab = "Min Temperature")
points(predict(spl.mintemp), pch = 20, col = "blue")
data$mintemp.anom <- data$value.temp_min - predict(spl.mintemp)

plot(mintemp.anom ~ date, data = data,
     pch = 16, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.25))

# Max temperature
spl.maxtemp <- gam(value.temp_max ~ s(ds, bs = "cc"), data = data)

plot(spl.maxtemp, ylim = c(-4,30))
points(predict(spl.maxtemp))
data$maxtemp.anom <- data$value.temp_max - predict(spl.maxtemp)

plot(maxtemp.anom ~ date, data = data,
     pch = 16, col = rgb(red = 0, green = 0, blue = 1, alpha = 0.25))

par(mfrow = c(1, 2))
with(data, {
  
  scatter.smooth(logvalue ~ maxtemp.anom, pch = 19, cex = 0.6, las = 1, 
                 xlab = Lmax, lpars = list(col = "red", lwd = 2), 
                 ylab = "ln(count + 1)")
  scatter.smooth(logvalue ~ mintemp.anom, pch = 19, cex = 0.6, las = 1, 
                 xlab = Lmin, lpars = list(col = "red", lwd = 2), 
                 ylab = "ln(count + 1)")
  
})

par(mfrow = c(1, 1))
plot(data$value.temp_max ~ data$ds,
     col = rgb(red = 1, green = 0, blue = 0, alpha = 0.25),
     pch = 16, ylab = "Max Temperature", xlab = "Days Since")
points(predict(spl.maxtemp) ~ data$ds, col = "black", pch = ".")
legend("bottomleft", legend = c("Actual", "Predicted"), pch = 16,
       col = c(rgb(red = 1, green = 0, blue = 0, alpha = 0.25), "black"),
       bty = "n")


# GAMs --------------

# Model 1 ---------
# Model value on days since 1 Jan using negative binomial 
mg1 <- gam(value ~ s(ds, bs = "cc"), family = nb(link = "log"),
           data = data)
par(mfrow = c(1, 1))
plot(mg1)
pred.mg1 <- predict(mg1, se.fit = TRUE)

plot(value ~ date, data = data, type = "p", pch = 16 , cex = 0.5, las = 1)
lines(data$date, exp(pred.mg1$fit), col = "red", lwd = 2)
lines(data$date, exp(pred.mg1$fit - 2*pred.mg1$se.fit), lty = 2, col = "orange", lwd = 1)
lines(data$date, exp(pred.mg1$fit + 2*pred.mg1$se.fit), lty = 2, col = "orange", lwd = 1)

# Model 2 -----------
# Include all varibales. smooth ds, max temp, min temp wind speed, wind dir
mg2. <- gam(value ~ s(ds, bs = "cc") + s(value.temp_max) + s(value.temp_min) + 
              value.rain + s(value.wind_speed) + value.humid + rain.lag1 + rain.lag2 +
              s(value.wind_dir, bs = "cc"), family = nb(), data = data, scale = -0.1)
### negative scale indicates that theta needs to be estimated

par(mfrow = c(2, 3))
plot(mg2.)
summary(mg2.)
(theta.est <- mg2.$family$getTheta(TRUE))

# Now use the newly found theta estimate in mg2
mg2 <- gam(value ~ s(ds, bs = "cc") + s(value.temp_max) + s(value.temp_min) + 
             value.rain + s(value.wind_speed) + value.humid + rain.lag1 + rain.lag2 +
             s(value.wind_dir, bs = "cc"), family = negbin(theta.est), data = data)
### negative scale indicates that theta needs to be estimated
summary(mg2) # Nothing has really changed?

par(mfrow = c(4, 3))
visreg(mg2.)
par(mfrow = c(4, 3))
visreg(mg2)

par(mfrow = c(4, 3))
visreg(mg2., scale = "response")
par(mfrow = c(4, 3))
visreg(mg2, scale = "response")

# Make predictions
pred.mg2 <- predict(mg2, se.fit = TRUE)

par(mfrow = c(1, 1))
plot(value ~ date, data = data, type = "p", pch = 16 , cex = 0.5, las = 1)
lines(data$date[-c(1,2)], exp(pred.mg2$fit), col = "red", lwd = 2)
lines(data$date[-c(1,2)], exp(pred.mg2$fit - 2*pred.mg1$se.fit[-c(1,2)]), lty = 2, col = "orange", lwd = 1)
lines(data$date[-c(1,2)], exp(pred.mg2$fit + 2*pred.mg1$se.fit[-c(1,2)]), lty = 2, col = "orange", lwd = 1)

par(mfrow = c(2, 2))
hist(resid(mg2))
hist(resid(mg2.))
acf(resid(mg2), xlim = c(1, 30))
acf(resid(mg2.), xlim = c(1, 30))
# Pretty much no difference?

pred.mg2 <- predict(mg2, se.fit = TRUE)
str(pred.mg2)

# Plot predicted against observed for mg2
par(mfrow = c(1, 1))
plot(exp(pred.mg2$fit), data$value[-c(1,2)], ylab = "observed", xlab = "predicted", 
     las = 1, pch = 16, cex = 0.8, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.25))
# Correlation between predicted and observed for mg 2
cor(exp(predict(mg2)), data$value[-c(1,2)])
cor(exp(predict(mg2)), data$value[-c(1,2)])^2

par(mfrow = c(2, 3), mar = c(5, 4, 1, 1))
visreg(mg2, "ds",  ylim = c(-5, 5), 
       xlab = "day of year",  ylim = c(-5, 5), ylab = "change in mean ln(count)") 
visreg(mg2, "value.temp_max",  ylim = c(-5, 5), 
       xlab = Lmax, ylab = "change in mean ln(count)")
visreg(mg2, "value.temp_min",  ylim = c(-5, 5), 
       xlab = Lmin, ylab = "change in mean ln(count)")
visreg(mg2, "value.rain",  ylim = c(-5, 5), 
       xlab = "rain (mm)", ylab = "change in mean ln(count)")
visreg(mg2, "rain.lag1", ylim = c(-5, 5), xlab = "rain on previous day (mm)", ylab = "change in mean ln(count)")
visreg(mg2, "value.wind_dir", ylim = c(-5, 5), xlab = "wind direction", ylab = "change in mean ln(count)")

visreg(mg2, c("ds"), scale = "response", ylab = "change in median count")
visreg(mg2, c("value.temp_max", "value.temp_min", "value.rain", "rain.lag1", 
              "value.wind_dir"), scale = "response", ylim = c(0, 10), 
       ylab = "change in median count")

# 3D representation of mg2 model
vis.gam(mg2, col = "red")

## without seasonal
mg2c <- gam(value ~ s(value.temp_max) + s(value.temp_min) + 
              value.rain + s(value.wind_speed) + value.humid + rain.lag1 + rain.lag2 +
              s(value.wind_dir, bs = "cc"), family = negbin(theta.est), data = data)
### negative scale indicates that theta needs to be estimated
summary(mg2c) 
# Performs poorly

## anomalies instead of absolute values (max and min temperature)
mg2b <- gam(value ~ s(ds, bs = "cc") + s(maxtemp.anom) + s(mintemp.anom) + 
              value.rain + s(value.wind_speed) + value.humid + rain.lag1 + rain.lag2 +
              s(value.wind_dir, bs = "cc"), family = nb(link = "log"), data = data)

summary(mg2b)
AIC(mg2, mg2b, mg2c)
# mg2 performs the best

par(mfrow = c(4, 3), mar = c(4, 4, 1, 1))
visreg(mg2b)
par(mfrow = c(4, 3), mar = c(4, 4, 1, 1))
visreg(mg2b, scale = "response")



plot(maxtemp.anom ~ value.temp_max, data = data)

# Attempt at gamm() function --------
# ### does not work ... does not like the negative binomial part
# library(nlme) 
# 
# 
# m3 <- gamm(value ~ s(ds, bs = "cc") + s(value.temp_max) + s(value.temp_min) + 
#             value.rain + s(value.wind_speed) + value.humid + rain.lag1 + rain.lag2 +
#             s(value.wind_dir, bs = "cc"), family = nb(link = "log"), data = dm, 
#            correlation = corARMA(form = ~ ds), p = 1)
# 

# Model 4 ----------

# Tensor product based smooths for ds and max temp using 
# Cyclical cubic and Cubic Regression spline bases
m4  <- gam(value ~ te(ds, value.temp_max, bs = c("cc", "cr")), 
           family = nb(link = "log"), data = data)
summary(m4) # Explains slightly less than Model 2

# Visualize GAM surface
par(mfrow = c(1, 1))
vis.gam(m4, view = c("ds", "value.temp_max"))
vis.gam(m4, view = c("value.temp_max", "ds"))

## temperature has a large effect during the middle of the year, much smaller in summer
AIC(mg1, m4, mg2)

# Same model as above but now using wind speed
m4  <- gam(value ~ te(ds, value.wind_speed, bs = c("cc", "cr")), 
           family = nb(link = "log"), data = data)
summary(m4) # Performs worse than max temp model above
vis.gam(m4) 

m4  <- gam(value ~ te(value.wind_dir, value.wind_speed, bs = c("cc", "cr")), 
           family = nb(link = "log"), data = data)
summary(m4)
vis.gam(m4) 

m4  <- gam(value ~ te(ds, value.rain, bs = c("cc", "cr")), 
           family = nb(link = "log"), data = data)
summary(m4)
vis.gam(m4) 

summary(mg1)

# Model 3 ---------

m3        = gam(value ~ te(ds, value.temp_max, bs = c("cc", "cr")) + s(value.temp_min) + 
            value.rain + s(value.wind_speed) + value.humid + rain.lag1 + rain.lag2 +
            s(value.wind_dir, bs = "cc"), family = nb(link = "log"), data = data)
summary(m3)
par(mfrow = c(1, 1))
acf(resid(m3))

AIC(mg1, mg2, m3)

# Linear Model --------
lm2       = lm(log(value + 1) ~ value.temp_max + value.temp_min + 
            value.rain + value.wind_speed + value.humid + rain.lag1 + rain.lag2 +
            value.wind_dir, data = data)
par(mfrow = c(2, 2))

plot(lm2)
summary(lm2)

par(mfrow = c(4, 3))
visreg(lm2)

# Model 5 ----
## interaction seasonal and year
data$year <-  as.numeric(  substr(data$date, 1, 4))
# mean.rain           = mean(data$value.rain)
# data$rain.lag1[1]   = mean.rain
# data$rain.lag2[1:2] = mean.rain
mg5                = gam(value ~ te(ds, year,  bs = c("cc", "cr"),k = c(7, 3)) + s(value.temp_max) +
                       s(value.temp_min) + value.rain + s(value.wind_speed) +
                       value.humid + rain.lag1 + rain.lag2 + 
                       s(value.wind_dir, bs = "cc"), family = negbin(theta.est), data = data)

mg5b                = gam(value ~ te(ds, year, bs = c("cc", "cr"), k = c(7, 3)) + s(value.temp_max) + s(value.temp_min) + 
                        value.rain + s(value.wind_speed) + value.humid + rain.lag1 + rain.lag2 +
                        s(value.wind_dir, bs = "cc"), family = negbin(theta.est), data = data)
par(mfrow = c(3, 3))
plot(mg5b)
vis.gam(mg5b, view = c("ds", "year"))

# Model 6 ------------
## take out some not so important factors
mg6 <- gam(value ~ te(ds, year, bs = c("cc", "cr"), k = c(7, 3)) + s(value.temp_max) + s(value.temp_min) + 
             value.rain + value.humid + rain.lag1, family = negbin(theta.est), data = data)
summary(mg6)
# Model 7 -------
## add constant year effect
mg7 <- gam(value ~ fyear +  s(ds, bs = "cc") + s(value.temp_max) + s(value.temp_min) + 
             value.rain + s(value.wind_speed) + value.humid + rain.lag1 + rain.lag2 +
             s(value.wind_dir, bs = "cc"), family = negbin(theta.est), data = data)
### negative scale indicates that theta needs to be estimated
summary(mg7)

# Model 8 ---------
# Smooth ds by year
mg8 <- gam(value ~ fyear +  s(ds, bs = "cc", by = fyear) + s(value.temp_max) + s(value.temp_min) + 
             value.rain + s(value.wind_speed) + value.humid + rain.lag1 + rain.lag2 +
             s(value.wind_dir, bs = "cc"), family = negbin(theta.est), data = data)
### negative scale indicates that theta needs to be estimated
summary(mg8)

cor(exp(predict(mg8)), data$value[-c(1,2)])
cor(exp(predict(mg8)), data$value[-c(1,2)])^2


par(mfrow = c(4, 3))
visreg(mg8)

par(mfrow = c(2, 1))
acf(resid(mg8))
pacf(resid(mg8))

AIC(mg2, mg5, mg6, mg5b, mg7, mg8)

# Plots of seasonal changes ---------


pdat <- with(data,
             data.frame(year = rep(2010:2014, each = 365),
                        ds = rep(1:365, times = 5), value.wind_speed = median(value.wind_speed), 
                        value.temp_max = median(value.temp_max), value.temp_min = median(value.temp_min), 
                        value.rain = median(value.rain), value.humid = median(value.humid), 
                        rain.lag1 = 0, rain.lag2 = 0, 
                        value.wind_dir = median(value.wind_dir)))

# Next, the predict() method generates predicted values for the new data pairs, 
# with standard errors for each predicted value

pred <- predict(mg5b, newdata = pdat, se.fit = TRUE)
crit <- qt(0.975, df = df.residual(mg5)) # ~95% interval critical t
pdat <- transform(pdat, fitted = pred$fit, se = pred$se.fit, fyear = as.factor(year))
pdat <- transform(pdat,
                  upper = fitted + (crit * se),
                  lower = fitted - (crit * se))



p1 <- ggplot(pdat, aes(x = ds, y = fitted, group = fyear)) +
  geom_ribbon(mapping = aes(ymin = lower, ymax = upper,
                            fill = fyear), alpha = 0.2) + # confidence band
  geom_line(aes(colour = fyear)) +    # predicted temperatures
  theme_bw() +                        # minimal theme
  theme(legend.position = "top") +    # push legend to the top
  #labs(y = expression(Temperature ~ (degree*C)), x = NULL) +
  scale_fill_discrete(name = "Year") + # correct legend name
  scale_colour_discrete(name = "Year") 
# scale_x_continuous(breaks = 1:12,   # tweak where the x-axis ticks are
#                    labels = month.abb, # & with what labels
#                    minor_breaks = NULL)
p1



#### ----- plots for model 8
#### ========================

# mg8 <- gam(value ~ fyear +  s(ds, bs = "cc", by = fyear) + s(value.temp_max) + s(value.temp_min) + 
#              value.rain + s(value.wind_speed) + value.humid + rain.lag1 + rain.lag2 +
#              s(value.wind_dir, bs = "cc"), family = negbin(theta.est), data = dm)

pdat <- with(data,
             data.frame(fyear = as.factor(rep(2011:2013, each = 365)),
                        ds = rep(1:365, times = 3), value.wind_speed = median(value.wind_speed), 
                        value.temp_max = median(value.temp_max), value.temp_min = median(value.temp_min), 
                        value.rain = median(value.rain), value.humid = median(value.humid), 
                        rain.lag1 = 0, rain.lag2 = 0, 
                        value.wind_dir = median(value.wind_dir)))

#Next, the predict() method generates predicted values for the new data pairs, with standard errors for each predicted value

pred <- predict(mg8, newdata = pdat, se.fit = TRUE)
crit <- qt(0.975, df = df.residual(mg5)) # ~95% interval critical t
pdat <- transform(pdat, fitted = pred$fit, se = pred$se.fit)
pdat <- transform(pdat,
                  upper = fitted + (crit * se),
                  lower = fitted - (crit * se))



p1 <- ggplot(pdat, aes(x = ds, y = fitted, group = fyear)) +
  geom_ribbon(mapping = aes(ymin = lower, ymax = upper,
                            fill = fyear), alpha = 0.2) + # confidence band
  geom_line(aes(colour = fyear)) +    # predicted temperatures
  theme_bw() +                        # minimal theme
  theme(legend.position = "top") +    # push legend to the top
  #labs(y = expression(Temperature ~ (degree*C)), x = NULL) +
  scale_fill_discrete(name = "Year") + # correct legend name
  scale_colour_discrete(name = "Year") 
# scale_x_continuous(breaks = 1:12,   # tweak where the x-axis ticks are
#                    labels = month.abb, # & with what labels
#                    minor_breaks = NULL)
p1


par(mfrow = c(2, 3), mar = c(5, 4, 1, 1))

visreg(mg8, "ds",  by = "fyear", ylim = c(-5, 5), 
       xlab = "day of year",  ylim = c(-5, 5), ylab = "change in mean ln(count)") 
visreg(mg8, "value.temp_max",  ylim = c(-5, 5), 
       xlab = Lmax, ylab = "change in mean ln(count)")
visreg(mg8, "value.temp_min",  ylim = c(-5, 5), 
       xlab = Lmin, ylab = "change in mean ln(count)")
visreg(mg8, "value.rain",  ylim = c(-5, 5), 
       xlab = "rain (mm)", ylab = "change in mean ln(count)")
visreg(mg8, "rain.lag1", ylim = c(-5, 5), xlab = "rain on previous day (mm)", ylab = "change in mean ln(count)")
visreg(mg8, "value.wind_dir", ylim = c(-5, 5), xlab = "wind direction", ylab = "change in mean ln(count)")

visreg(mg8, "value.wind_dir", ylim = c(-5, 5), xlab = "wind direction", ylab = "change in mean ln(count)")

# Save data set -----
save(data, file = "API_B_format.Rda") 

