  ![UC3M_logo](https://www.uc3m.es/ss/Satellite?blobcol=urldata&blobkey=id&blobtable=MungoBlobs&blobwhere=1371552353583&ssbinary=true)
                                 
                                 ## Weather Report & Forecast Project


### Course Details : 2019 Data Tyding and Reporting
 This project is implemented as part of "Masters in Statistics for Data Science" course curriculum.  
      Course Name: Data Tyding and Reporting     
      Year  : 2019

##### Authors : Kambhampati Jyothi and Luis Gonzalez Conde Sanchez Crespo

#### Check out the app @ https://weatherdata.shinyapps.io/WeatherForecast/

#### Introduction
   This is an Rshiny based application which provides information about Current weather and five days forecast with a frequency of every 3 hours for defined set of cities. The data for this application is extracted via API provided by openweathermap.org

Forecast for Weather metrics is shown automatically for the choosen city from the dropdowns. Weather metrics includes Temperature, atmospheric pressure, relative humidity, precipitation, wind speed. Current Weather report information can be explored using geographical Maps. 

#### Data Source
Data is extracted from https://openweathermap.org/ using Current & forecast weather API and then processed according to the application requirement.

#### App Features

- Real-Time updates for Today Weather and forecast for next 5 days with frequency of every 3 hours.

- Weather Metrics considered are Temperature,Temperature range,Wind speed, Atmospheric Pressure, Relative Humidity, Clouds,Cloudiness,  Sunrise and Sunset timings.

- Hourly forecast tab provides line chart visualizing the forecast trend for choosen city & metric for every 3 hour for next 5 days and On hover popup box displays accurate timings and selected measure along with its units.

- Maps tab provides Real time weather map with your personal blizzard & hurricane tracker: Rain, Snow, Clouds, Fog, Clear sky,
Thunderstorm, Drizzle etc.

- On hover & Click Weather Description pops up showing City name, Cloudiness, Temperature,Temperature range,Wind speed, Atmospheric Pressure, Relative Humidity, Clouds, Sunrise and Sunset timings.

- Units for different Weather Metrics:
      - Temperature(in Celsius)
      - Time format (YYYY/MM/DD HH:MM:SS)
      - Precipitation (%)
      - Wind speed (m/s)
      - Pressure (hpa)
      - Humidity(%)
   
   
- Weather forecast for cities which include: Sevilla,Bilbao,Vigo,Gijon,Zaragoza,Amsterdam,London,Lisbon,Paris,Barcelona,Madrid,Valencia,Mumbai,Salamanca,Toledo,Getafe,Singapore,Hong Kong,New York,Dubai,Rome,Las Vegas,Milan,Warsaw,Toronto,Miami,Buenos Aires,Rio de Janeiro,Santiago,Quito,Nairobi,Cairo,Istanbul,Berlin,Munich,Moscow,Tokyo,Delhi,Chennai,Shanghai,Melbourne,Sydney,Vienna,Budapest,Brussels,Nice,Marseille.

#### Prerequisite Packages


#### Deployment
This App is published on https://www.shinyapps.io/ 
demo link : https://weatherdata.shinyapps.io/WeatherForecast/
