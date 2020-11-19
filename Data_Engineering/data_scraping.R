
# Libraries -----------
{
  library(request)
  library(jsonlite)
  library(httr)
  library(zoo)
  library(pastecs)
  library(data.table)
  library(corrplot)
  library(dplyr)
}

# Fetch past 7 days weather data & 7 days ahead forecasts
APIrequest = function(){
  keys     = c("D93BEE3MG1DDV9HFKD4TDZ2B3","PZSMCLY0TV8WDVR85EKPBLMYQ", "94PTQDZ21VSDMHHT2XVFJK40U", "EBAR2HSG4KACLUX2B3E8XEHXU",
              "YGRJRH8DV62KZ7MAW43SQL900", "EAPTA3QB308L41LWVE35DYEYL", "HA5E9345NW0XENSZCQC5RY4WT")
  # Forecast --------
  request_future     = GET(paste("https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/weatherdata/forecast?location=-33.9345,18.4771&aggregateHours=24&unitGroup=metric&shortColumnNames=false&contentType=json&key=", keys[1], sep = ""))
  (forecast          = fromJSON(rawToChar(request_future$content))$locations$'-33.9345,18.4771'$values)
  forecast$datetimeStr = as.Date(forecast$datetimeStr)
  forecast = forecast[,c("datetimeStr", "mint", "maxt", "humidity", "precip","wspd", "wdir")]
  forecast = forecast %>% rename(date = datetimeStr, min_temp = mint, max_temp = maxt, humid = humidity, rain = precip, wind_speed = wspd, wind_dir = wdir)
  # Historical --------
  startDate         = Sys.Date()-7
  endDate           = Sys.Date() 
  request_history   = read.csv(paste("https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/weatherdata/history?&aggregateHours=24&startDateTime=", startDate, "&endDateTime=", endDate, 
                      "&unitGroup=metric&contentType=csv&dayStartTime=0:0:00&dayEndTime=0:0:00&location=-33.9345,18.4771&key=", keys[1], sep = ""))
  request_history$Date.time = as.Date(request_history$Date.time, format = "%m/%d/%Y")
  historical = request_history[,c("Date.time", "Minimum.Temperature", "Maximum.Temperature", "Relative.Humidity", "Precipitation", "Wind.Speed", "Wind.Direction")]
  historical = historical %>% rename(date = Date.time, min_temp = Minimum.Temperature, max_temp = Maximum.Temperature, humid = Relative.Humidity, rain = Precipitation, wind_speed = Wind.Speed, wind_dir = Wind.Direction)
  # Combined --------
  return(as.data.frame(rbind(historical[2:8,], forecast[2:8,])))
}

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


