
library(ggplot2)
library(RCurl)
library(httr)
library(rjson)
library(tibble)
library(tidyverse)
library(shiny)
library(shinythemes)
library(plotly)
library(maps)
library(leaflet)
library(leaflet.extras)
library(dplyr)

#add city names here
cities = c("Sevilla","Bilbao","Vigo","Gijon","Zaragoza","Amsterdam","London","Lisbon","Paris","Barcelona","Madrid","Valencia","Mumbai","Salamanca","Toledo","Getafe","Singapore","Hong Kong","New York","Dubai","Rome","Las Vegas","Milan","Warsaw","Toronto","Miami","Buenos Aires","Rio de Janeiro","Santiago","Quito","Nairobi","Cairo","Istanbul","Berlin","Munich","Moscow","Tokyo","Delhi","Chennai","Shanghai","Melbourne","Sydney","Vienna","Budapest","Brussels","Nice","Marseille")
City = cities
Measure = c("temperature","speed","pressure","humidity")


#API KEY : openweathermap.org 
api_key = "79b54045049e992fe3ae23152608b590"

#functions for scrapping data 
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



#extract live forecast for next 5 days 
forecast = data.frame()

for(j in 1:length(cities)){
  
  bin = data.frame()
  main = data.frame() 
  date_time = data.frame()
  weather = data.frame()
  wind = data.frame()
  
  
  data=get_weather_forecast(api_key,city=cities[j])
  
  
  for (i in 1:length(data$list)) {
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

colnames(forecast)[1] = c("date_time")
forecast$num_forecast = forecast$date_time
levels(forecast$num_forecast) <- 1:length(data$list)  
forecast$num_forecast = as.numeric(forecast$num_forecast)
forecast$num_forecast[forecast$num_forecast<=8] = 1
forecast$num_forecast[forecast$num_forecast>8 & forecast$num_forecast<=16] = 2
forecast$num_forecast[forecast$num_forecast>16 & forecast$num_forecast<=24] = 3
forecast$num_forecast[forecast$num_forecast>24 & forecast$num_forecast<=32] = 4
forecast$num_forecast[forecast$num_forecast>32 & forecast$num_forecast<=40] = 5
forecast$temp = round(forecast$temp - 273.15)
forecast$temp_min = round(forecast$temp_min - 273.15)
forecast$temp_max = round(forecast$temp_max - 273.15)
rm(bin,data,date_time,main,temp,weather,wind)



#GEO MAP + Plot : extracting current weather for multiple cities
current = data.frame()
bin = data.frame()
main = data.frame() 
date_time = data.frame()
weather = data.frame()
wind = data.frame()
visibility = data.frame()
clouds = data.frame()
cordinates = data.frame()
sunrise_set = data.frame()


for(j in 1:length(cities)){
  data=get_weather_current(api_key,city=cities[j])
  cordinates = as.data.frame(data$coord)
  main = as.data.frame(data$main)
  date_time = as.data.frame(data$dt)
  weather = as.data.frame(data$weather)
  weather=weather[,c("main","description")]
  wind = as.data.frame(data$wind)
  wind = wind[,c("speed")]
  visibility = as.data.frame(data$visibility)
  clouds = as.data.frame(data$clouds)
  sunrise_set = as.data.frame(data$sys)
  sunrise_set = sunrise_set[,c("sunrise","sunset")]
  
  temp = data.frame()
  temp = cbind(date_time,cordinates,main,weather,wind,visibility,clouds,sunrise_set)
  temp$City = data$name
  current = rbind(current,temp) 
  
}
colnames(current)[1] = c("date_time")
current$date_time = as.POSIXct(current$date_time, origin="1970-01-01")
current$sunrise = as.POSIXct(current$sunrise, origin="1970-01-01")
current$sunset = as.POSIXct(current$sunset, origin="1970-01-01")
colnames(current)[12] = c("Visibility")
colnames(current)[13] = c("Precipitation")
current$temp = round(current$temp - 273.15)
current$temp_min = round(current$temp_min - 273.15)
current$temp_max = round(current$temp_max - 273.15)


rm(bin,data,date_time,main,temp,weather,wind,visibility,clouds,cordinates,sunrise_set)


server <- function(input, output) {
  

  output$Hour <- renderPlotly({
    
    City1 =input$City1
    measure = input$measure
    hour = input$hour
    
    
    
    if(input$measure=="temperature"){
      Date_Time = forecast$date_time[forecast$City==input$City1 & forecast$num_forecast<=input$hour[2]& forecast$num_forecast>=input$hour[1]]
      Temperature_Forecast = forecast$temp[forecast$City==input$City1 & forecast$num_forecast<=input$hour[2]& forecast$num_forecast>=input$hour[1]]
      ggplot(as.data.frame(Temperature_Forecast), aes(x=Date_Time, y=Temperature_Forecast,text="Units: °C", group=1)) +
        geom_line(color="red")+
        geom_point()+
        labs(title="Temperature Forecast (every 3 hours) - (in °C)",x="Date & Time", y = "Temperature (in °C)")+
        theme(axis.text.x = element_text(angle = 90))+theme(plot.title = element_text(hjust = 0.5))
      
    }
    
    else if(input$measure=="humidity"){
      Date_Time = forecast$date_time[forecast$City==input$City1 & forecast$num_forecast<=input$hour[2]& forecast$num_forecast>=input$hour[1]]
      Humidity_Forecast = forecast$humidity[forecast$City==input$City1 & forecast$num_forecast<=input$hour[2]& forecast$num_forecast>=input$hour[1]]
      ggplot(as.data.frame(Humidity_Forecast), aes(x=Date_Time, y=Humidity_Forecast,text="Units: Percentage(%)", group=1)) +
        geom_line(color="red")+
        geom_point()+
        labs(title="Humidity Forecast (every 3 hours) - (in %)",x="Date & Time", y = "Humidity (in %)")+
        theme(axis.text.x = element_text(angle = 90))+theme(plot.title = element_text(hjust = 0.5))
      
    }
    
    else if(input$measure=="pressure"){
      Date_Time = forecast$date_time[forecast$City==input$City1 & forecast$num_forecast<=input$hour[2]& forecast$num_forecast>=input$hour[1]]
      Pressure_Forecast =forecast$pressure[forecast$City==input$City1 & forecast$num_forecast<=input$hour[2]& forecast$num_forecast>=input$hour[1]]
      ggplot(as.data.frame(Pressure_Forecast), aes(x=Date_Time, y=Pressure_Forecast,text="Units: hpa", group=1)) +
        geom_line(color="red")+
        geom_point()+
        labs(title="Pressure Forecast (every 3 hours) - (in hpa)",x="Date & Time", y = "Pressure (in hpa)")+
        theme(axis.text.x = element_text(angle = 90))+theme(plot.title = element_text(hjust = 0.5))
      
      
    }
    
    else if(input$measure=="speed"){
      Date_Time = forecast$date_time[forecast$City==input$City1 & forecast$num_forecast<=input$hour[2]& forecast$num_forecast>=input$hour[1]]
      Wind_Speed_Forecast = round(forecast$speed[forecast$City==input$City1 & forecast$num_forecast<=input$hour[2]& forecast$num_forecast>=input$hour[1]],1)
      ggplot(as.data.frame(Wind_Speed_Forecast), aes(x=Date_Time, y=Wind_Speed_Forecast,text="Units: m/sec", group=1)) +
        geom_line(color="red")+
        geom_point()+
        labs(title="Wind Speed Forecast(every 3 hours) - (in m/sec)",x="Date & Time", y = "Wind speed (in m/sec)")+
        theme(axis.text.x = element_text(angle = 90))+theme(plot.title = element_text(hjust = 0.5))
      
      
      
    }
  })
  
  
  output$mymap <-renderLeaflet({
    
    pal <- colorNumeric(palette = c('black'),domain = current$temp)
    output$mymap <- renderLeaflet({
      leaflet(current) %>% 
        setView(lng = 10, lat =20, zoom = 3)  %>% 
        addTiles() %>% 
        addCircles(data = current, lat = ~ lat, lng = ~ lon, weight = 2, radius = ~temp*50, popup = ~as.character(temp), label = ~as.character(paste0(City," Temperature: ", sep = " ", temp)), color = ~pal(temp), fillOpacity = 0.5)
      
    })
    
    getColor <- function(current) {
      sapply(current$main, function(main) {
        if(main == "Smoke") {"red"
        }else if(main =="Haze"){"orange"
        }else if(main=="Dust"){"beige"
        }else if(main=="Sand"){"beige"
        }else if(main =="Ash"){"black"
        }else if(main =="Clear"){"lightblue"
        }else if(main =="Thunderstorm"){"purple"
        }else if(main =="Drizzle"){"lightgreen"
        }else if(main =="Rain"){"green"
        }else if(main =="Clouds"){"gray"
        }else if(main =="Mist"){"lightgray"
        }else if(main =="Fog"){"gray"
        }else{"white"}
        
      })
    }
    
    
    getpngURL = function(current){
      sapply(current$main, function(main) {
        if(main == "Clear") {"http://openweathermap.org/img/w/01d.png"
        }else if(main =="Thunderstorm"){"http://openweathermap.org/img/w/11d.png"
        }else if(main =="Drizzle"){"http://openweathermap.org/img/w/09d.png"
        }else if(main =="Rain"){"http://openweathermap.org/img/w/10d.png"
        }else if(main =="Snow"){"http://openweathermap.org/img/w/13d.png"
        }else if(main =="Mist"){"http://openweathermap.org/img/w/02d.png"
        }else if(main =="Fog"){"http://openweathermap.org/img/w/02n.png"
        }else if(main =="Clouds"){"http://openweathermap.org/img/w/04d.png"
        }else{"http://openweathermap.org/img/w/50n.png"}
        
      })
    }
    
    leafIcons <- icons(
      iconUrl = getpngURL(current),
      iconWidth = 60, iconHeight = 60,
      iconAnchorX = 0, iconAnchorY = 0)    
    
    icons <- awesomeIcons(
      icon = 'star',
      iconColor = 'white',
      library = 'glyphicon',
      markerColor = getColor(current)
      
    )
    
    observe({
      proxy <- leafletProxy("mymap", data = current)
      proxy %>% clearMarkers()
      proxy %>% addAwesomeMarkers(~lon, ~lat, icon=icons,label = ~as.character(paste("In ",City," today's weather is ",description,".   Click for details")),popup = ~as.character(paste("City Name: ",City,"<br>","Cloudiness: ",main,"<br>","<br>","Temperature: ",temp,"°C","<br>","Temparature Range:",temp_min," to ",temp_max,"°C","<br>","<br>","Wind Speed:",wind,"m/sec","<br>","Pressure: ",pressure," hpa","<br>","Humidity: ",humidity,"%","<br>","Clouds: ",Precipitation,"%","<br>","<br>","Sunrise: ",sunrise,"<br>","Sunset: ",sunset))) 
      proxy %>% addMarkers(~lon, ~lat, icon=leafIcons,label = ~as.character(paste("In ",City," today's weather is ",description,".   Click for details")),popup = ~as.character(paste("City Name: ",City,"<br>","Cloudiness: ",main,"<br>","<br>","Temperature: ",temp,"°C","<br>","Temparature Range:",temp_min," to ",temp_max,"°C","<br>","<br>","Wind Speed:",wind,"m/sec","<br>","Pressure: ",pressure," hpa","<br>","Humidity: ",humidity,"%","<br>","Clouds: ",Precipitation,"%","<br>","<br>","Sunrise: ",sunrise,"<br>","Sunset: ",sunset))) 
      
      
    })
    
  })
  
}



