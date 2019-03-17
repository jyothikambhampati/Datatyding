rm(list = ls())

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
#City = c("Vancouver","Portland","San Francisco","Seattle","Los Angeles",
#         "San Diego","Las Vegas","Phoenix","Albuquerque","Denver",
#         "San Antonio","Dallas","Houston","Kansas City","Minneapolis","Saint Louis",
#         "Chicago","Nashville","Indianapolis","Atlanta","Detroit",
#         "Jacksonville","Charlotte","Miami","Pittsburgh","Toronto",
#         "Philadelphia","New York","Montreal","Boston","Beersheba","Tel Aviv District",
#         "Eilat","Haifa","Nahariyya","Jerusalem","London","Madrid","Barcelona","Valencia","Mumbai",
#         "Granada","Salamanca","Singapore","Hong Kong","New York","Dubai","Rome","Las Vegas","Milan","Warsaw"
#)
#City = c("London")
#add city names here
cities = c("London","Madrid","Barcelona","Valencia","Mumbai","Granada","Salamanca","Singapore","Hong Kong","New York","Dubai","Rome","Las Vegas","Milan","Warsaw","Toronto")
#cities = c("Madrid")

City = cities
Measure = c("temperature","humidity","pressure","speed")

#DO NOT CHANGE THIS
api_key = "79b54045049e992fe3ae23152608b590"
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
colnames(forecast)[1] = c("date_time")
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
  temp = data.frame()
  temp = cbind(date_time,cordinates,main,weather,wind,visibility,clouds)
  temp$City = data$name
  current = rbind(current,temp) 
  
}
colnames(current)[1] = c("date_time")
current$date_time = as.POSIXct(current$date_time, origin="1970-01-01")
colnames(current)[13] = c("Visibility")
rm(bin,data,date_time,main,temp,weather,wind,visibility,clouds,cordinates)




# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("slate"),
  # Application title
  navbarPage(title="Weather Forecasting",
             tabPanel(title="Hourly Forecast",icon=icon("fas fa-chart-line"),
                      titlePanel("Forecast"),
                      
                      # Sidebar with a input 
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "City1",
                                      label = "Choose your City",
                                      choices = sort(unique(City))),
                          selectInput(inputId = "measure",
                                      label = "Choose your Measure",
                                      choices = sort(unique(Measure)))
                          
                        ),
                        # Show a plot of the generated distribution
                        mainPanel(plotOutput(outputId ="Hour"))
                      )
                      
             ),
             tabPanel(title="Maps",icon=icon("far fa-globe"),
                      #titlePanel("View Map"),
                      
                      mainPanel(leafletOutput(outputId = "mymap"), 
                                absolutePanel(top = 60, left = 20 
                                              #checkboxInput("markers", "Weather", FALSE)
                                )
                      )
             )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  output$Hour <- renderPlot({
    
    #This is the variable selection
    City1 =input$City1
    measure = input$measure
    
    if(input$measure=="temperature"){
      plot(forecast$temp[forecast$City==input$City1],main='Temperature',type="l")
    }
    
    #Plotting the time series
    #plot(dat_temperature, main = 'Temperature',type="l")}
    #ggplot(dat_temperature, aes(x = dat_date, y = City))+
    # geom_line(color = "#00AFBB", size = 0.5) + ggtitle('Temperature')+
    #xlab('Date') + ylab('Temperature in Kelvins')}
    
    else if(input$measure=="humidity"){
      #Plotting the time series
      plot(forecast$humidity[forecast$City==City1],main='Humidity',type="l")
    }
    
    else if(input$measure=="pressure"){
      #Plotting the time series
      plot(forecast$pressure[forecast$City==City1],main='Pressure',type="l")
    }
    
    else if(input$measure=="speed"){
      #Plotting the time series
      plot(forecast$speed[forecast$City==City1],main='Speed',type="l")
    }
  })
  
  output$mymap <-renderLeaflet({
    
    pal <- colorNumeric(
      palette = c('black'),
      domain = current$temp)
    
    pal2 <- colorFactor(
      palette = c('green', 'blue', 'pink','red','grey','black'),
      domain = current$main)
    
    output$mymap <- renderLeaflet({
      leaflet(current) %>% 
        setView(lng = 9, lat =45, zoom = 4)  %>% 
        addTiles() %>% 
        addCircles(data = current, lat = ~ lat, lng = ~ lon, weight = 2, radius = ~temp*50, popup = ~as.character(temp), label = ~as.character(paste0(City," Temperature: ", sep = " ", temp)), color = ~pal(temp), fillOpacity = 0.5)
      
    })
    
    getColor <- function(current) {
      sapply(current$main, function(main) {
        if(main == "Clear") {"blue"
        }else if(main =="Drizzle"){"orange"
        }else if(main=="Clouds"){"pink"
        }else if(main=="Smoke"){"red"
        }else if(main =="Rain"){"green"
        }else if(main =="Dust"){"black"
        }else if(main =="Mist"){"black"
          
        }else{"black"}
        
      })
    }
    
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = getColor(current)
    )
    
    
    observe({
      proxy <- leafletProxy("mymap", data = current)
      proxy %>% clearMarkers()
      # if (input$markers) {
      proxy %>% addAwesomeMarkers(~lon, ~lat, icon=icons,label = ~as.character(paste0(City," Weather: ", sep = " ", main))) %>%
        addLegend("bottomright", pal = pal2, values = current$main,
                  title = "Weather",
                  opacity = 2)
      
    })
    
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)
