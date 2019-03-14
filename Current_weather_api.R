rm(list=ls(all=TRUE))

library(RCurl)
library(httr)
library(rjson)
library(tibble)
library(tidyverse)


#add city names here
cities = c("London","Madrid","Barcelona","Valencia","Mumbai","Granada","Salamanca","Singapore","Hong Kong","New York","Dubai","Rome","Las Vegas","Milan","Warsaw","Toronto")
#cities = c("London")


#DO NOT CHANGE THIS
api_key = "79b54045049e992fe3ae23152608b590"


#extract live forecast for next 5 days 
forecast = data.frame()
bin = data.frame()
main = data.frame() 
date_time = data.frame()
weather = data.frame()
wind = data.frame()



get_weather_forecast=function(api_key,city="",country="")
{
  url="http://api.openweathermap.org/data/2.5/forecast?"
  
  city=gsub(" ","+",city)
  country=gsub(" ","+",country)
  
  
  if(city != "")
  {
    url=paste(url,"q=",city,sep="")
    
    if(country!="")
    {
      url=paste(url,",",country,sep="")
    }
  }
  
  
  url=paste(url,"&APPID=",api_key,sep="")
  d=getURL(url)
  d=fromJSON(d)
  d
}




for(j in 1:length(cities)){
  
  data=get_weather_forecast(api_key,city=cities[j])
  
  for (i in 1:40) {
    bin = as.data.frame(data$list[[i]]$main)
    main = rbind(main,bin)    
    
    bin = as.data.frame(data$list[[i]]$dt_txt)
    date_time = rbind(date_time,bin)
    
    bin = as.data.frame(data$list[[i]]$weather)
    weather = rbind(weather,bin)
    
    bin = as.data.frame(data$list[[i]]$wind)
    wind = rbind(wind,bin)
  }
  
  temp = data.frame()
  temp = cbind(date_time,main,weather,wind)
  temp$City = data$city$name
  
  forecast = rbind(forecast,temp)
  
}

#data_raw <- enframe(unlist(data))

colnames(forecast)[1] = c("date_time")
str(forecast)
boxplot(forecast$temp~ forecast$City,xlab="",ylab="temperature",main="Temperature by City (in Kelvins)")
boxplot(forecast$humidity~ forecast$City,xlab="",ylab="Humidity",main="Humidity by City")
rm(bin,data,date_time,main,temp,weather,wind)






#extract live current 
current = data.frame()
bin = data.frame()
main = data.frame() 
date_time = data.frame()
weather = data.frame()
wind = data.frame()
visibility = data.frame()
clouds = data.frame()




get_weather_current=function(api_key,city="",country="")
{
  url="http://api.openweathermap.org/data/2.5/weather?"
  
  city=gsub(" ","+",city)
  country=gsub(" ","+",country)
  
  if(city != "")
  {
    url=paste(url,"q=",city,sep="")
    
    if(country!="")
    {
      url=paste(url,",",country,sep="")
    }
  }
  
  url=paste(url,"&APPID=",api_key,sep="")
  d=getURL(url)
  d=fromJSON(d)
  d
}

data=get_weather_current(api_key,city=cities[2])

for(j in 1:length(cities)){
  data=get_weather_current(api_key,city=cities[j])
  
  main = as.data.frame(data$main)
  
  date_time = as.data.frame(data$dt)
  
  weather = as.data.frame(data$weather)
  
  wind = as.data.frame(data$wind)
  wind = wind[,c("speed")]
  
  visibility = as.data.frame(data$visibility)
  
  clouds = as.data.frame(data$clouds)
  
  temp = data.frame()
  temp = cbind(date_time,main,weather,wind,visibility,clouds)
  temp$City = data$name
  current = rbind(current,temp) 
  
}


colnames(current)[1] = c("date_time")
current$date_time = as.POSIXct(current$date_time, origin="1970-01-01")
colnames(current)[13] = c("Visibility")

str(current)
rm(bin,data,date_time,main,temp,weather,wind,visibility,clouds)
