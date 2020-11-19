# An R script that 
# 1: fetches the required data
# 2: runs the model
# 3: outputs a set of predictions
# 4: writes the predictions, which are then uploaded to github
library(tidyverse)

setwd("/Users/skycope/Documents/GitHub/grass-pollen")

# Fetch pollen data from past 7 days
fetch_pollen = function(){
  pollen = read.csv('https://raw.githubusercontent.com/skycope/grass-pollen/master/Workflow/pollen_counts.csv')
  pollen = data.frame(pollen_count = c(pollen$pollen_count, rep(NA, 7)))
  return(pollen)
}


# Fetch most recent veg index
fetch_vegindex = function(){
  vegetation = read.csv('https://raw.githubusercontent.com/skycope/grass-pollen/master/Workflow/veg_index_new.csv')
  vegetation = data.frame(veg_index = rep(vegetation$veg_index, 14))
  return(vegetation)
}


# Finally, output the predictions
predictions = data.frame(day = c("Monday", "Tuesday", "Wednesday",
                   "Thursday", "Friday", "Saturday", "Sunday"),
           Very_Low  = c(0.1, 0, 0.1, 0.7, 0.9, 0.4, 0.01),
           Low       = c(0.8, 0.1, 0.2, 0.2, 0.05, 0.3, 0.1),
           Moderate  = c(0.1, 0.1, 0.4, 0.05, 0.05, 0.1, 0.1),
           High      = c(0, 0.5, 0.2, 0.05, 0, 0.1, 0.29),
           Very_High = c(0, 0.3, 0.1, 0, 0, 0.1, 0.5)) 

# write to csv
write.csv(predictions, 'predictions.csv')


