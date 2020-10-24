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

setwd("/Users/chloestipinovich/Documents/2020/Thesis Project/grass-pollen/Fw__grass_pollen_data_since_2015")

# Parameters ----------

# Set forecast parameters for API request
type    = c("forecast","history")
loc     = "-33.9345,18.4771"
a.hrs   = "24"
keys    = c("D93BEE3MG1DDV9HFKD4TDZ2B3","PZSMCLY0TV8WDVR85EKPBLMYQ",
            "94PTQDZ21VSDMHHT2XVFJK40U", "EBAR2HSG4KACLUX2B3E8XEHXU",
            "YGRJRH8DV62KZ7MAW43SQL900", "EAPTA3QB308L41LWVE35DYEYL",
            "HA5E9345NW0XENSZCQC5RY4WT")

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
                    "&unitGroup=metric&contentType=csv&dayStartTime=0:0:00&dayEndTime=0:0:00&location=", loc, "&key=", keys[1], sep = "")
historical  = read.csv(URL)
str(historical)

# Historical Automatic----------

histColNames = c("Date.time",
                 "Minimum.Temperature", "Maximum.Temperature", "Temperature",
                 "Dew.Point", "Relative.Humidity", "Precipitation",
                 "Wind.Speed", "Wind.Direction", 
                 "Visibility", "Conditions",
                 "Cloud.Cover", "Precipitation.Cover")

# Initialize storage
dataHist         = c()
m                = 1

# Initialize start date
startDate        = "2018-09-11T00:00:00" # yyyy-mm-ddT00:00:00
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


# 1 ----

startDate        = "2018-09-11T00:00:00" # yyyy-mm-ddT00:00:00
endDate          = as.Date(startDate,format = '%Y-%m-%dT%H:%M:%S')+40
endDate          = paste(endDate, "T00:00:00", sep = "")
j=2

URL              = paste("https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/weatherdata/", 
                            type[2], "?&aggregateHours=", a.hrs, "&startDateTime=", startDate, "&endDateTime=", endDate, 
                            "&unitGroup=metric&contentType=csv&dayStartTime=0:0:00&dayEndTime=0:0:00&location=", loc, "&key=", keys[j], sep = "")
h1               = read.csv(URL)

# 2 ----------
startDate        = as.Date(endDate)+1
startDate        = paste(startDate, "T00:00:00", sep = "")
endDate          = as.Date(startDate,format = '%Y-%m-%dT%H:%M:%S')+100
endDate          = paste(endDate, "T00:00:00", sep = "")
j=2

URL              = paste("https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/weatherdata/", 
                         type[2], "?&aggregateHours=", a.hrs, "&startDateTime=", startDate, "&endDateTime=", endDate, 
                         "&unitGroup=metric&contentType=csv&dayStartTime=0:0:00&dayEndTime=0:0:00&location=", loc, "&key=", keys[j], sep = "")
h2               = read.csv(URL)

# 3 ----------
startDate        = as.Date(endDate)+1
startDate        = paste(startDate, "T00:00:00", sep = "")
endDate          = as.Date(startDate,format = '%Y-%m-%dT%H:%M:%S')+100
endDate          = paste(endDate, "T00:00:00", sep = "")
j=2

URL              = paste("https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/weatherdata/", 
                         type[2], "?&aggregateHours=", a.hrs, "&startDateTime=", startDate, "&endDateTime=", endDate, 
                         "&unitGroup=metric&contentType=csv&dayStartTime=0:0:00&dayEndTime=0:0:00&location=", loc, "&key=", keys[j], sep = "")
h3               = read.csv(URL)

# 4 ----------
startDate        = as.Date(endDate)+1
startDate        = paste(startDate, "T00:00:00", sep = "")
endDate          = as.Date(startDate,format = '%Y-%m-%dT%H:%M:%S')+100
endDate          = paste(endDate, "T00:00:00", sep = "")
j=3

URL              = paste("https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/weatherdata/", 
                         type[2], "?&aggregateHours=", a.hrs, "&startDateTime=", startDate, "&endDateTime=", endDate, 
                         "&unitGroup=metric&contentType=csv&dayStartTime=0:0:00&dayEndTime=0:0:00&location=", loc, "&key=", keys[j], sep = "")
h4               = read.csv(URL)

# 5 ----------
startDate        = as.Date(endDate)+1
startDate        = paste(startDate, "T00:00:00", sep = "")
endDate          = as.Date(startDate,format = '%Y-%m-%dT%H:%M:%S')+100
endDate          = paste(endDate, "T00:00:00", sep = "")
j=3

URL              = paste("https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/weatherdata/", 
                         type[2], "?&aggregateHours=", a.hrs, "&startDateTime=", startDate, "&endDateTime=", endDate, 
                         "&unitGroup=metric&contentType=csv&dayStartTime=0:0:00&dayEndTime=0:0:00&location=", loc, "&key=", keys[j], sep = "")
h5               = read.csv(URL)

# 6 ----------
startDate        = as.Date(endDate)+1
startDate        = paste(startDate, "T00:00:00", sep = "")
endDate          = as.Date(startDate,format = '%Y-%m-%dT%H:%M:%S')+40
endDate          = paste(endDate, "T00:00:00", sep = "")
j=3

URL              = paste("https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/weatherdata/", 
                         type[2], "?&aggregateHours=", a.hrs, "&startDateTime=", startDate, "&endDateTime=", endDate, 
                         "&unitGroup=metric&contentType=csv&dayStartTime=0:0:00&dayEndTime=0:0:00&location=", loc, "&key=", keys[j], sep = "")
h6               = read.csv(URL)

# 7 ----------
startDate        = as.Date(endDate)+1
startDate        = paste(startDate, "T00:00:00", sep = "")
endDate          = as.Date(startDate,format = '%Y-%m-%dT%H:%M:%S')+24
endDate          = paste(endDate, "T00:00:00", sep = "")
j=7

URL              = paste("https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/weatherdata/", 
                         type[2], "?&aggregateHours=", a.hrs, "&startDateTime=", startDate, "&endDateTime=", endDate, 
                         "&unitGroup=metric&contentType=csv&dayStartTime=0:0:00&dayEndTime=0:0:00&location=", loc, "&key=", keys[j], sep = "")
h7               = read.csv(URL)

# 8 ----------
startDate        = as.Date(endDate)+1
startDate        = paste(startDate, "T00:00:00", sep = "")
endDate          = as.Date(startDate,format = '%Y-%m-%dT%H:%M:%S')+10
endDate          = paste(endDate, "T00:00:00", sep = "")
j=7

URL              = paste("https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/weatherdata/", 
                         type[2], "?&aggregateHours=", a.hrs, "&startDateTime=", startDate, "&endDateTime=", endDate, 
                         "&unitGroup=metric&contentType=csv&dayStartTime=0:0:00&dayEndTime=0:0:00&location=", loc, "&key=", keys[j], sep = "")
h8               = read.csv(URL)

newWeather       = rbind(h1,h2,h3,h4,h5,h6,h7,h8)
newWeather       = newWeather[, histColNames]
newWeather       = newWeather[1:517,]

str(newWeather) # Checking data is in the correct format
newWeather$Date.time  = as.Date(newWeather$Date.time, format = "%m/%d/%Y") # Convert date chr to Date objects
newWeather$Conditions = as.factor(newWeather$Conditions) # Create a conditions factor with 6 levels
str(newWeather)

# Find missing values and change date heading

sum(is.na(newWeather)) # Missing Values
names(newWeather)[1] = "date"

# Extract pollen values over the same period from dataGrass.csv
dataGrass        = read.csv("all_counts.csv", header = TRUE)
dataGrass$date   = as.Date(dataGrass$date)
names(dataGrass)[2] = "value"
count            = dataGrass[dataGrass$date > newWeather$date[1]-1 ,c('date','value')] # over the correct period
count            = count[dataGrass$date < newWeather$date[nrow(newWeather)]+1,]
count            = count[1:517,]

# Add count value to data
setDT(newWeather)
setDT(count)
newCombined = newWeather[count, on = 'date']

save(newCombined, file="CountAPI_2.RData")


# save.image('countAPI.RData') #Save objects so we don't have to rerun forests
# rm(list = ls())
load('countAPI.RData') # Look for 'data' object
names(newCombined)
newCombined        =  newCombined[,-c("Cloud.Cover","Precipitation.Cover")]

totalCombined      = rbind(data, newCombined)
NAS                = which(is.na(totalCombined$value))
totalCombinedNoNAs = totalCombined[-NAS,]

save(totalCombinedNoNAs, file="newCountAPI.RData")
skim(totalCombinedNoNAs)

load("newCountAPI.RData")
dataTotal = totalCombinedNoNAs
# Adjust data to suite Birgit's layout ---------
# Create ds variable
dataTotal$day <- substr(dataTotal$date, 9, 10)
dataTotal$month <- substr(dataTotal$date, 6, 7)
dataTotal$year <- substr(dataTotal$date, 1, 4)
dataTotal$fyear <- as.factor(dataTotal$year)
head(dataTotal)
dataTotal <- mutate(dataTotal, ds = as.numeric(date - as.Date(paste(year,"-01-01", sep = ""))) + 1)

head(dataTotal)
str(dataTotal)
names(dataTotal)

# Make variable names the same as in Birgit's code for variables that are equal
names(dataTotal) <- c("date",
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
names(dataTotal)

# Create log value variable
dataTotal$logvalue <- log(dataTotal$value+1)

save(dataTotal, file="newCountAPI_BF.RData")

