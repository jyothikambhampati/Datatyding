rm(list=ls(all=TRUE))

library(ROpenWeatherMap)
library(RCurl)
library(httr)
library(rjson)


get_weather_forecast=function(api_key,cityID=NA,city="",country="",coordinates=NA)
{
  url="http://api.openweathermap.org/data/2.5/forecast?"
  
  city=gsub(" ","+",city)
  country=gsub(" ","+",country)
  
  if(!is.na(cityID))
  {
    url=paste(url,"id=",cityID,sep="")
  }
  
  if(city != "")
  {
    url=paste(url,"q=",city,sep="")
    
    if(country!="")
    {
      url=paste(url,",",country,sep="")
    }
  }
  
  if(!is.na(coordinates[1]))
  {
    url=paste(url,"lat=",coordinates[1],"&lon=",coordinates[2],sep="")
  }
  
  url=paste(url,"&APPID=",api_key,sep="")
  d=getURL(url)
  d=fromJSON(d)
  d
}


api_key = "bffd46a4f5694954742745de6346493e"
data=get_weather_forecast(api_key,city="Vancouver")
library(tibble)
library(tidyverse)
data_raw <- enframe(unlist(data))
data_raw
