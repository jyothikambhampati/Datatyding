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
City = c("London")

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

for(j in 1:length(City)){
  
  data=get_weather_forecast(api_key,city=City[j])
  
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
  
}

#data_raw <- enframe(unlist(data))

colnames(forecast)[1] = c("date_time")
str(forecast)

dat_temperature<-forecast$temp
dat_humidity<-forecast$humidity
dat_pressure<-forecast$pressure
dat_speed<-forecast$speed
dat_date<-forecast$date_time

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
      #dat_temperature <- ts(dat_temperature, start = c(2019, 1), freq = 5)
      
      #Plotting the time series
      
      #plot(dat_temperature, main = 'Temperature',type="l")}
      
      ggplot(dat_temperature, aes(x = dat_date, y = City1))+
        geom_line(color = "#00AFBB", size = 0.5) + ggtitle('temperature ')+
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
}

# Run the application 
shinyApp(ui = ui, server = server)

