# Strategy:
# Pretend we are in 2014
# Use best models for GAM and RF to 
# Forecast 7 days ahead 


URL = " https://saas.afrigis.co.za/rest/2/weather.forecast.daily.getByCoord/AUTH_PARAMS/?location=-34.1,18.4&groups=basic,extended,astronomical,tides"
URL = "https://api.met.no/weatherapi/locationforecast/2.0/complete?lat=-33.9345&lon=18.4771"
request = GET(URL)
str(request)
request$content
data  = fromJSON(rawToChar(request$content)) # $locations$'-33.9345,18.4771'$values
data2 = data$properties$timeseries$data$










