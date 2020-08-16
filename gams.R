library(mgcv)
library(visreg)

setwd("/Users/skycope/Documents/UCT/Stats\ Honours/Project/Data\ and\ code")

dm <- read.csv("grassdata.csv", header = T)

mg1 <- gam(value ~ s(ds, bs = "cc"), family = nb(link = "log"), data = dm)
par(mfrow = c(1, 1))
plot(mg1)
# points(prof.mean$x, log(prof.mean$value + 1) - mean(log(prof.mean$value + 1)))
pred.mg1 <- predict(mg1, se.fit = TRUE)

plot(value ~ date, data = dm, type = "p", pch = 19 , cex = 0.5, las = 1)
lines(dm$date, exp(pred.mg1$fit), col = "seagreen", lwd = 2)
lines(dm$date, exp(pred.mg1$fit - 2*pred.mg1$se.fit), lty = 2, col = "seagreen", lwd = 2)
lines(dm$date, exp(pred.mg1$fit + 2*pred.mg1$se.fit), lty = 2, col = "seagreen", lwd = 2)

#library(MASS)
mg2. <- gam(value ~ s(ds, bs = "cc") + s(value.temp_max) + s(value.temp_min) + 
              value.rain + s(value.wind_speed) + value.humid + rain.lag1 + rain.lag2 +
              s(value.wind_dir, bs = "cc"), family = nb(), data = dm, scale = -0.1)
### negative scale indicates that theta needs to be estimated

par(mfrow = c(2, 3))
plot(mg2.)
summary(mg2.)
(theta.est <- mg2.$family$getTheta(TRUE))

mg2 <- gam(value ~ s(ds, bs = "cc") + s(value.temp_max) + s(value.temp_min) + 
             value.rain + s(value.wind_speed) + value.humid + rain.lag1 + rain.lag2 +
             s(value.wind_dir, bs = "cc"), family = negbin(theta.est), data = dm)
### negative scale indicates that theta needs to be estimated
summary(mg2)

par(mfrow = c(4, 3))
visreg(mg2.)
par(mfrow = c(4, 3))
visreg(mg2)

par(mfrow = c(4, 3))
visreg(mg2., scale = "response")
par(mfrow = c(4, 3))
visreg(mg2, scale = "response")


# points(prof.mean$x, log(prof.mean$value + 1) - mean(log(prof.mean$value + 1)))
pred.mg2 <- predict(mg2, se.fit = TRUE)

# plot(value ~ date, data = dm, type = "p", pch = 19 , cex = 0.5, las = 1)
# lines(dm$date, exp(pred.mg2$fit), col = "seagreen", lwd = 2)
# lines(dm$date, exp(pred.mg2$fit - 2*pred.mg1$se.fit), lty = 2, col = "seagreen", lwd = 2)
# lines(dm$date, exp(pred.mg2$fit + 2*pred.mg1$se.fit), lty = 2, col = "seagreen", lwd = 2)
# 
par(mfrow = c(2, 2))
hist(resid(mg2))
hist(resid(mg2.))
acf(resid(mg2), xlim = c(1, 30))
acf(resid(mg2.), xlim = c(1, 30))

#acf(dm$value)

pred.mg2 <- predict(mg2, se.fit = TRUE)
str(pred.mg2)

filename = paste(obssite, "obspred.pdf", sep = "")
pdf(filename)
plot(exp(pred.mg2$fit), na.omit(dm)$value, ylab = "observed", xlab = "predicted", 
     las = 1, pch = 19, cex = 0.8)

cor(exp(predict(mg2)), na.omit(dm)$value)
cor(exp(predict(mg2)), na.omit(dm)$value)^2
abline(0, 1) 
dev.off()

filename = paste(obssite, "gam1.pdf", sep = "")
pdf(filename, width = 12, height = 8)
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
dev.off()


plot(exp(pred.mg2$fit), na.omit(dm)$value)


vis.gam(mg2)


## without seasonal
mg2c <- gam(value ~ s(value.temp_max) + s(value.temp_min) + 
              value.rain + s(value.wind_speed) + value.humid + rain.lag1 + rain.lag2 +
              s(value.wind_dir, bs = "cc"), family = negbin(theta.est), data = dm)
### negative scale indicates that theta needs to be estimated
summary(mg2c)

## anomalies instead of absolute values (max and min temperature)
mg2b <- gam(value ~ s(ds, bs = "cc") + s(maxtemp.anom) + s(mintemp.anom) + 
              value.rain + s(value.wind_speed) + value.humid + rain.lag1 + rain.lag2 +
              s(value.wind_dir, bs = "cc"), family = nb(link = "log"), data = dm)

summary(mg2b)
AIC(mg2, mg2b, mg2c)

par(mfrow = c(4, 3), mar = c(4, 4, 1, 1))
visreg(mg2b)
par(mfrow = c(4, 3), mar = c(4, 4, 1, 1))
visreg(mg2b, scale = "response")


plot(maxtemp.anom ~ value.temp_max, data = dm)

# ### does not work ... does not like the negative binomial part
# library(nlme) 
# 
# 
# m3 <- gamm(value ~ s(ds, bs = "cc") + s(value.temp_max) + s(value.temp_min) + 
#             value.rain + s(value.wind_speed) + value.humid + rain.lag1 + rain.lag2 +
#             s(value.wind_dir, bs = "cc"), family = nb(link = "log"), data = dm, 
#            correlation = corARMA(form = ~ ds), p = 1)
# 


m4  <- gam(value ~ te(ds, value.temp_max, bs = c("cc", "cr")), 
           family = nb(link = "log"), data = dm)
summary(m4)

par(mfrow = c(1, 1))
vis.gam(m4)
vis.gam(m4, view = c("ds", "value.temp_max"))
vis.gam(m4, view = c("value.temp_max", "ds"))

## temperature has a large effect during the middle of the year, much smaller in summer
AIC(mg1, m4, mg2)

m4  <- gam(value ~ te(ds, value.wind_speed, bs = c("cc", "cr")), 
           family = nb(link = "log"), data = dm)
summary(m4)
vis.gam(m4) 

m4  <- gam(value ~ te(value.wind_dir, value.wind_speed, bs = c("cc", "cr")), 
           family = nb(link = "log"), data = dm)
summary(m4)
vis.gam(m4) 

m4  <- gam(value ~ te(ds, value.rain, bs = c("cc", "cr")), 
           family = nb(link = "log"), data = dm)
summary(m4)
vis.gam(m4) 

summary(mg1)


m3 <- gam(value ~ te(ds, value.temp_max, bs = c("cc", "cr")) + s(value.temp_min) + 
            value.rain + s(value.wind_speed) + value.humid + rain.lag1 + rain.lag2 +
            s(value.wind_dir, bs = "cc"), family = nb(link = "log"), data = dm)
summary(m3)

acf(resid(m3))

AIC(mg1, mg2, m3)


### -----



lm2 <- lm(log(value + 1) ~ value.temp_max + value.temp_min + 
            value.rain + value.wind_speed + value.humid + rain.lag1 + rain.lag2 +
            value.wind_dir, data = dm)
par(mfrow = c(2, 2))

plot(lm2)
summary(lm2)

par(mfrow = c(4, 3))
visreg(lm2)

dm$year <-  as.numeric(  substr(dm$date, 1, 4))

## interaction seasonal and year
mg5 <- gam(value ~ te(ds, year, bs = c("cc", "cr")) + s(value.temp_max) + s(value.temp_min) + 
             value.rain + s(value.wind_speed) + value.humid + rain.lag1 + rain.lag2 +
             s(value.wind_dir, bs = "cc"), family = negbin(theta.est), data = dm)

mg5b <- gam(value ~ te(ds, year, bs = c("cc", "cr"), k = c(7, 3)) + s(value.temp_max) + s(value.temp_min) + 
              value.rain + s(value.wind_speed) + value.humid + rain.lag1 + rain.lag2 +
              s(value.wind_dir, bs = "cc"), family = negbin(theta.est), data = dm)
par(mfrow = c(3, 3))
plot(mg5)

par(mfrow = c(1, 1))
#plot(mg5, pers = TRUE)

pdf("dsyearspline.pdf")
vis.gam(mg5, view = c("ds", "year"))
dev.off()

vis.gam(mg5b, view = c("ds", "year"))

## take out some not so important factors
mg6 <- gam(value ~ te(ds, year, bs = c("cc", "cr")) + s(value.temp_max) + s(value.temp_min) + 
             value.rain + value.humid + rain.lag1, family = negbin(theta.est), data = dm)

## add constant year effect
mg7 <- gam(value ~ fyear +  s(ds, bs = "cc") + s(value.temp_max) + s(value.temp_min) + 
             value.rain + s(value.wind_speed) + value.humid + rain.lag1 + rain.lag2 +
             s(value.wind_dir, bs = "cc"), family = negbin(theta.est), data = dm)
### negative scale indicates that theta needs to be estimated
summary(mg7)

mg8 <- gam(value ~ fyear +  s(ds, bs = "cc", by = fyear) + s(value.temp_max) + s(value.temp_min) + 
             value.rain + s(value.wind_speed) + value.humid + rain.lag1 + rain.lag2 +
             s(value.wind_dir, bs = "cc"), family = negbin(theta.est), data = dm)
### negative scale indicates that theta needs to be estimated
summary(mg8)

cor(exp(predict(mg8)), na.omit(dm)$value)
cor(exp(predict(mg8)), na.omit(dm)$value)^2


par(mfrow = c(4, 3))
visreg(mg8)

par(mfrow = c(1, 2))
acf(resid(mg8))
pacf(resid(mg8))

AIC(mg2, mg5, mg6, mg5b, mg7, mg8)

### -----

# library(quantreg)
# 
# q1 <- rq(logvalue ~ ds + value.temp_max + value.temp_min + 
#      value.rain + value.wind_speed + value.humid + rain.lag1 + rain.lag2 +
#      value.wind_dir, data = dm, tau = 0.9)
# 
# summary(q1)
# plot(q1)
# 


### ===================================================
### plots of seasonal changes
### ===================================================


pdat <- with(dm,
             data.frame(year = rep(2010:2014, each = 365),
                        ds = rep(1:365, times = 5), value.wind_speed = median(value.wind_speed), 
                        value.temp_max = median(value.temp_max), value.temp_min = median(value.temp_min), 
                        value.rain = median(value.rain), value.humid = median(value.humid), 
                        rain.lag1 = 0, rain.lag2 = 0, 
                        value.wind_dir = median(value.wind_dir)))

#Next, the predict() method generates predicted values for the new data pairs, with standard errors for each predicted value

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

pdat <- with(dm,
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




pdf("gam8.pdf", width = 12, height = 8)
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

dev.off()

