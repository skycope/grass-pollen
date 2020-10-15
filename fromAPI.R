# Attains 

# Exploratory data analysis on API data

{
  library(request)
  library(jsonlite)
  library(httr)
  library(zoo)
  library(pastecs)
  library(data.table)
  library(corrplot)
}


# Parameters ----------

# Set forecast parameters for API request
type    = c("forecast","history")
loc     = "-33.9345,18.4771"
a.hrs   = "24"
keys    = c("D93BEE3MG1DDV9HFKD4TDZ2B3","PZSMCLY0TV8WDVR85EKPBLMYQ",
            "94PTQDZ21VSDMHHT2XVFJK40U", "EBAR2HSG4KACLUX2B3E8XEHXU",
            "YGRJRH8DV62KZ7MAW43SQL900", "EAPTA3QB308L41LWVE35DYEYL")

# Forecast Example ---------

URL     = paste("https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/weatherdata/",
                type[1], "?location=", loc, "&aggregateHours=", a.hrs,
                "&unitGroup=metric&shortColumnNames=false&contentType=json&key=", keys[1], sep = "")
# request = GET(URL)
# (data   = fromJSON(rawToChar(request$content))$locations$'-33.9345,18.4771'$values)

# Clean data

# data$datetimeStr = as.Date(data$datetimeStr)
# data$conditions  = as.factor(data$conditions)
# 
# str(data)
# names(data)
# data.clean = data[,c("datetimeStr",
#                      "mint", "maxt", "temp",
#                      "dew", "humidity", "precip",
#                      "wspd", "wdir",  "visibility",
#                      "conditions",
#                      "sunshine", "cloudcover", "wgust")]
# data.clean
# plot(data.clean)


# Historical data  Example--------

startDate   = "2011-01-01T00:00:00" # yyyy-mm-ddT00:00:00
endDate     = "2011-02-11T00:00:00" # yyyy-mm-ddT00:00:00

URL         = paste("https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/weatherdata/", 
                    type[2], "?&aggregateHours=", a.hrs, "&startDateTime=", startDate, "&endDateTime=", endDate, 
                    "&unitGroup=metric&contentType=csv&dayStartTime=0:0:00&dayEndTime=0:0:00&location=", loc, "&key=", keys[5], sep = "")
# historical  = read.csv(URL)
# str(historical) 

# Historical Automatic----------

histColNames = c("Date.time",
                 "Minimum.Temperature", "Maximum.Temperature", "Temperature",
                 "Dew.Point", "Relative.Humidity", "Precipitation",
                 "Wind.Speed", "Wind.Direction", 
                 "Visibility", "Conditions",
                 "Cloud.Cover", "Precipitation.Cover")

# Initialize storage
dataHist = c()
m                = 1

# Initialize start date
startDate        = "2011-01-01T00:00:00" # yyyy-mm-ddT00:00:00
endDate          = as.Date(startDate,format = '%Y-%m-%dT%H:%M:%S')+100
endDate          = paste(endDate, "T00:00:00", sep = "")

for(j in 1:length(keys)){
  for(i in 1:3){
    
    URL                 = paste("https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/weatherdata/", 
                            type[2], "?&aggregateHours=", a.hrs, "&startDateTime=", startDate, "&endDateTime=", endDate, 
                            "&unitGroup=metric&contentType=csv&dayStartTime=0:0:00&dayEndTime=0:0:00&location=", loc, "&key=", keys[j], sep = "")
    historical          = read.csv(URL)
    
    if (length(historical)>3) {
    
      historical          = historical[, histColNames]
      
      if (j ==1 & i == 1){
        dataHist  = historical
      }
      else{
        dataHist  = rbind(dataHist[,histColNames], historical[,histColNames])
      }
      
      if (i ==1 | i == 3){
        startDate = as.Date(endDate)+1
        endDate   = as.Date(startDate,format = '%Y-%m-%dT%H:%M:%S')+100
        
      }
      else{
        startDate = as.Date(endDate)+1
        endDate   = as.Date(startDate,format = '%Y-%m-%dT%H:%M:%S')+40
      }
    }  # end warning check
  } # end i
} # end j

str(dataHist) # Checking data is in the correct format
dataHist$Date.time  = as.Date(dataHist$Date.time, format = "%m/%d/%Y") # Convert date chr to Date objects
dataHist$Conditions = as.factor(dataHist$Conditions) # Create a conditions factor with 6 levels
str(dataHist)

# Find missing values and chnage date heading

sum(is.na(dataHist)) # Missing Values
names(dataHist)[1] = "date"

# Extract pollen values over the same period from dataGrass.csv
dataGrass        = read.csv("grassData.csv", header = TRUE)
dataGrass$date   = as.Date(dataGrass$date)
count            = dataGrass[dataGrass$date > historical.clean$date[1]-1 ,c('date','value')] # over the correct period
count            = count[dataGrass$date < historical.clean$date[nrow(historical.clean)]+1,]

# Add count value to data
setDT(dataHist)
setDT(count)
combined = historical.clean[count, on = 'date']

# save.image('countAPI.RData') #Save objects so we don't have to rerun forests
# rm(list = ls())
# load('countAPI.RData')




# Variable details ----------

# wdir = wind direction when wspd was at a max for a.hrs
# temp = mean temperature over a.hrs
# maxt = max temperature over a.hrs
# visibility = the distance than can be seen in daylight in km
# wspd = maximum wind speed over a.hrs in km/hr
# datetimeStr
# heatindex = measure of how hot it feels combining the actual air temperature with the relative humidity
# cloudcover = the amount of sky that is covered by cloud expressed as a percentage
# pop = possibility of precipitation for the given time period expressed as a percentage change from 0-100%
# mint = minimum temperature over a.hrs period
# datetime
# precip = amount of precipitation that fell or is predicted to fall over a.hrs
# snowdepth = Snow depth is the average amount of snow currently on the ground for the time period
# snow = amount of new snow that has fallen in the time period
# humidity = the amount of water vapor present in the air compared the maximum amount possible for a given temperature, expressed as a mean percentage
# wgust = maximum wind speed measures over a short amount of time
# conditions = conditions reported at a particular location such as any thunderstorms, rainfall
# windchil = measure of how cold it feels combining the actual air temperature with the wind speed
# sealevelpressure = atmospheric pressure at a location that removes reduction in pressure due to the altitude of the location. This is expressed in millibars

