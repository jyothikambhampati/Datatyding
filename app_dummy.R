rm(list = ls())

library(ggplot2)
library(RCurl)
library(httr)
library(rjson)
library(tibble)
library(tidyverse)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(markdown)
library(plotly)
library(WDI)
library(wbstats)
library(progress)
library(Hmisc)
library(maps)
library(viridis)
library(mapproj)
library(mapdata)
library(ggthemes)
library(viridisLite)
library(shiny)
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

Measure = c("temperature","humidity","pressure","speed")


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
                      titlePanel("View Map"),
                      
                      mainPanel(leafletOutput(outputId = "mymap"), 
                                absolutePanel(top = 60, left = 20, 
                                              checkboxInput("markers", "Weather", FALSE))
             )
             )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
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
  bin = data.frame()
  main = data.frame() 
  date_time = data.frame()
  weather = data.frame()
  wind = data.frame()
  
    data=get_weather_forecast(api_key,city=input$City1)
    
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
    temp$City = data$City$name
    
    forecast = rbind(forecast,temp)

  colnames(forecast)[1] = c("date_time")
  str(forecast)
  
  dat_temperature<-forecast$temp
  dat_temperature
  dat_humidity<-forecast$humidity
  dat_pressure<-forecast$pressure
  dat_speed<-forecast$speed
  dat_date<-forecast$date_time
  
  
  
  #extract current weather 
  current = data.frame()
  bin = data.frame()
  main = data.frame() 
  date_time = data.frame()
  weather = data.frame()
  wind = data.frame()
  visibility = data.frame()
  clouds = data.frame()
  cordinates = data.frame()
  
  
  data=get_weather_current(api_key,city=input$City1)
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
  
  colnames(current)[1] = c("date_time")
  current$date_time = as.POSIXct(current$date_time, origin="1970-01-01")
  colnames(current)[13] = c("Visibility")
  rm(bin,data,date_time,main,temp,weather,wind,visibility,clouds,cordinates)
  
  
  #current weather text inputs and plots
  
  
  

  
    
  output$Hour <- renderPlot({
    
    #This is the variable selection
    City1 =input$City1
    measure = input$measure
    
    if(input$measure=="temperature"){
      #dat_temperature <- ts(dat_temperature, start = c(2019, 1), freq = 5)
      
      #Plotting the time series
      
      #plot(dat_temperature, main = 'Temperature',type="l")}
      
      ggplot(dat_temperature, aes(x = dat_date, y = City))+
        geom_line(color = "#00AFBB", size = 0.5) + ggtitle('Temperature')+
        xlab('Date') + ylab('Temperature in Kelvins')}
    
    else if(input$measure=="humidity"){
      dat_humidity <- ts(dat_humidity, start = c(2019, 1), freq = 5)
      
      #Plotting the time series
      
      plot(dat_humidity, main = 'Humidity',type="l")}
    
    else if(input$measure=="pressure"){
      dat_pressure <- ts(dat_pressure, start = c(2019, 1), freq = 5)
      
      
      #Plotting the time series
      
      plot(dat_pressure, main = 'pressure',type="l")}
    
    else if(input$measure=="speed"){
      dat_speed <- ts(dat_speed, start = c(2019, 1), freq = 5)
      
      #Plotting the time series
      
      plot(dat_speed, main = 'speed',type="l")}
    })
  
  output$mymap <-renderPlot({
  
    #GEO MAP : extracting current weather for multiple cities
    current = data.frame()
    bin = data.frame()
    main = data.frame() 
    date_time = data.frame()
    weather = data.frame()
    wind = data.frame()
    visibility = data.frame()
    clouds = data.frame()
    cordinates = data.frame()
    
    #add city names here
    cities = c("London","Madrid","Barcelona","Valencia","Mumbai","Granada","Salamanca","Singapore","Hong Kong","New York","Dubai","Rome","Las Vegas","Milan","Warsaw","Toronto")
    
    
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
    
    
    
    pal <- colorNumeric(
      palette = c('black'),
      domain = current$temp)
    
    pal2 <- colorFactor(
      palette = c('orange', 'blue', 'black','red','green'),
      domain = current$main)
    
    
    
    output$mymap <- renderLeaflet({
      leaflet(current) %>% 
        setView(lng = -30, lat =45, zoom = 2)  %>% 
        addTiles() %>% 
        addCircles(data = current, lat = ~ lat, lng = ~ lon, weight = 2, radius = ~temp*200, popup = ~as.character(temp), label = ~as.character(paste0(City," Temperature: ", sep = " ", temp)), color = ~pal(temp), fillOpacity = 0.5)
      
    })
    
    
    
    getColor <- function(current) {
      sapply(current$main, function(main) {
        if(main == "Clear") {"blue"
        }else if(main =="Drizzle"){"orange"
        }else if(main=="Clouds"){"black"
        }else if(main=="Smoke"){"red"
        }else if(main =="Rain"){"green"}
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
      if (input$markers) {
        proxy %>% addAwesomeMarkers(~lon, ~lat, icon=icons,label = ~as.character(paste0(City," Weather: ", sep = " ", main))) %>%
          addLegend("bottomright", pal = pal2, values = current$main,
                    title = "Weather",
                    opacity = 2)}
      else {
        proxy %>% clearMarkers() %>% clearControls()
      }
    })
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

