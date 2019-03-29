
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



