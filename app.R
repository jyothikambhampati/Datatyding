rm(list = ls())
library(shiny)
library(shinythemes)
library(shinydashboard)
library(markdown)
library(plotly)
library(tidyverse)
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

City = c("Vancouver","Portland","San Francisco","Seattle","Los Angeles",
         "San Diego","Las Vegas","Phoenix","Albuquerque","Denver",
         "San Antonio","Dallas","Houston","Kansas City","Minneapolis","Saint Louis",
         "Chicago","Nashville","Indianapolis","Atlanta","Detroit",
         "Jacksonville","Charlotte","Miami","Pittsburgh","Toronto",
         "Philadelphia","New York","Montreal","Boston","Beersheba","Tel Aviv District",
         "Eilat","Haifa","Nahariyya","Jerusalem")

Country<-c("USA","Canada","Israel")

USA <-c("Portland","San Francisco","Seattle","Los Angeles",
        "San Diego","Las Vegas","Phoenix","Albuquerque","Denver",
        "San Antonio","Dallas","Houston","Kansas City","Minneapolis","Saint Louis",
        "Chicago","Nashville","Indianapolis","Atlanta","Detroit",
        "Jacksonville","Charlotte","Miami","Pittsburgh",
        "Philadelphia","New York","Montreal","Boston")
Canada<-c("Vancouver","Toronto","Montreal")
Israel<-c("Beersheba","Tel Aviv District",
          "Eilat","Haifa","Nahariyya","Jerusalem")

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("slate"),
   # Application title
  navbarPage(title="Weather Forecasting",
             tabPanel(title="Hourly Forecast",icon=icon("fas fa-chart-line"),
                      titlePanel("Forecast"),
                      
                      # Sidebar with a slider input for number of bins 
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "City1",
                                      label = "Choose your City",
                                      choices = sort(unique(City))),
                          # value is always yyyy-mm-dd, even if the display format is different
                          dateInput("date3", "Date:", value = "2012-02-29", format = "mm/dd/yy"),
                          
                          sliderInput(inputId = "Hours1",
                                      label = "Hourly Forecast",
                                      min = 1,max = 24, value = 1
                          )
                        ),
                        # Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("Hour_FC")
                          
                        )
                      )
             ),
             
             
             tabPanel(title="7 Day Forecast",icon=icon("fas fa-chart-line"),
                      titlePanel("Forecast"),
                      
                      # Sidebar with a slider input for number of bins 
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "City2",
                                      label = "Choose your City",
                                      choices = sort(unique(City))),
                          # value is always yyyy-mm-dd, even if the display format is different
                          dateInput("date3", "Date:", value = "2012-02-29", format = "mm/dd/yy"),
                          sliderInput(inputId = "7_Day",
                                      label = "7 Day Forecast",
                                      min = 1,max = 7, value = 1
                          )
                        ),
                        # Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("7_FC")
                          
                        )
                      )
             ),
             
             
             tabPanel(title="Yearly Forecast",icon=icon("fas fa-chart-line"),
                      titlePanel("Forecast"),
   
                # Sidebar with a slider input for number of bins 
                    sidebarLayout(
                    sidebarPanel(
              selectInput(inputId = "City3",
                          label = "Choose your City",
                          choices = sort(unique(City))),
              # value is always yyyy-mm-dd, even if the display format is different
              dateInput("date3", "Date:", value = "2012-02-29", format = "mm/dd/yy"),
              sliderInput(inputId = "Year3",
                          label = "Yearly Forecast",
                          min = 1,max = 5, value = 1
                          )
      ),
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("Year_FC")
         
      )
   )
),

tabPanel(title="Maps",icon=icon("far fa-globe"),
         titlePanel("View Map"),
         
         sidebarPanel(
           
           # Input: Selector for choosing dataset ----
           selectInput(inputId = "Country4",
                       label = "Choose your Country",
                       choices = sort(unique(Country)))
         ),
         
          mainPanel
         (plotOutput("map"))
         ),

         tabPanel(title="Insights",icon=icon("bar-chart-o"),
         titlePanel("Weather Report"),
         
         sidebarPanel(
           
           # Input: Selector for choosing dataset ----
           selectInput(inputId = "City5",
                       label = "Choose your City",
                       choices = sort(unique(City))),
           sliderInput(inputId = "Year5",
                       label = "Yearly Forecast",
                       min = 1,max = 5, value = 1),
           sliderInput(inputId = "Month5",
                      label = "Monthly Forecast",
                       min = 1,max = 12, value = 1),
           sliderInput(inputId = "Date5",
                                  label = "Daily Forecast",
                                  min = 1,max = 30, value = 1
           )
           
         ),
         
         mainPanel(plotOutput("stats"))),

navbarMenu(title="Manage Info",icon=icon("far fa-info"),
           tabPanel(title="DATASET",icon=icon("fas fa-database")),
           tabPanel(title="UsERS",icon=icon("fas fa-user")),
           tabPanel(title="LOGS" , icon=icon("fas fa-Logs")),
           tabPanel(title="SETTINGS" , icon=icon("fas fa-cogs"))
           ),
    navbarMenu("Help", icon = icon("question-circle")),
         tabPanel("Sign out", icon = icon("power-off"))

))


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

