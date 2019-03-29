
# Weather forecast  Application

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

#Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("slate"),
  # Application title
  navbarPage(title="Weather Report & Forecast <><><><> data:openweathermap.org",
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
                                      choices = unique(Measure)),
                          sliderInput(inputId = "hour",
                                      label = "Days to forecast (Frequency:every 3hours)",
                                      min = 01,max = 5, value = c(1,3))
                          # value is always yyyy-mm-dd, even if the display format is different
                          #dateInput("date", "Date:", value = date, format = "yyyy-mm-dd")
                          
                        ),
                        
                        mainPanel(plotlyOutput(outputId ="Hour",height = 450,width = 'auto'))
                      )
                      
             ),
             tabPanel(title="Maps",icon=icon("far fa-globe"),
                      #titlePanel("View Map"),
                      
                      mainPanel(leafletOutput(outputId = "mymap",height = 800,width = 1400), 
                                absolutePanel(top = 60, left = 20
                                              #checkboxInput("markers", "Weather", FALSE)
                                )
                      )
             )
  )
)

