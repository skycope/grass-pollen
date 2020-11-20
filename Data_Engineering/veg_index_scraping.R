library(httr)
library(readr)
library(jsonlite)
loc     = "-33.9345,18.4771"

r <- GET("https://modis.ornl.gov/rst/api/v1/MOD13Q1/dates?latitude=-33.9345&longitude=18.4771")
dates <- content(r)$dates

# daysVI <- lapply(dates, function(dt) dt$)
calendar_dates <- lapply(dates, function(dt) dt$calendar_date)

# Print the first ten MODIS dates
dates[1:10]

library(MODISTools)
MODISSubsets(LoadDat = PREDICTS,
             Products = "MOD13Q1",
             Bands = c("250m_16_days_EVI",
                       "250m_16_days_VI_Quality"), 
             Size = c(3,3),StartDate = FALSE, TimeSeriesLength = 1) 


