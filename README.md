# Uber Data

This Shinny App allows you to view the raw data Uber has provided in the Summer of 2014., 

## Starting off

I loaded the following libraries before we began:

```R
library(dplyr)
library(tidyr)
library(shiny)
library(vroom)
library(ggplot2)
library(DT)
library(leaflet)
library(leaflet.extras)

# Clear Variables
rm(list = ls())
```

First, once we had gotten the data, I first vroomed it all into objects, before then `rbind` the data into one big data frame we could work with.

```R
apr_data <- vroom::vroom("data/uber-raw-data-apr14.csv")
aug_data <- vroom::vroom("data/uber-raw-data-aug14.csv")
jul_data <- vroom::vroom("data/uber-raw-data-jul14.csv")
jun_data <- vroom::vroom("data/uber-raw-data-jun14.csv")
may_data <- vroom::vroom("data/uber-raw-data-may14.csv")
sep_data <- vroom::vroom("data/uber-raw-data-sep14.csv")

df <- rbind(apr_data, aug_data, jul_data, jun_data, may_data, sep_data)

# Unload all this data since it's been combined (helps save memory)
rm(apr_data, aug_data, jul_data, jun_data, may_data, sep_data)
```

## Cleaning the data

In order to get insights out of this data, I needed to separate the data out into different columns. Such as if we have the question, how many rides happen on the first of the month, we need to separate the date so that we can just look at the day. 

```R
# Seperate the date from the time
df <- df %>% separate(`Date/Time`, c('Date', 'Time'), sep = " ")
# Handle seperating all of the time
df <- df %>% separate(Time, c('Hour', 'Minute', 'Second'), sep = ":")
# Handle seperating all of the dates
df <- df %>% separate(Date, c('Month', 'Day', 'Year'), sep = "/")

# Set numbers to numbers so that arranging can be done properly
df$Hour <- as.numeric(df$Hour)
df$Day <- as.numeric(df$Day)
```

## Pivot Tables

We also want to use Pivot Tables (with the help of `dplyr`) in order to help create graphs. I also added commas for some the plots, in order to make them look better. 

```R
# Trips for each month
ggplot(df_monthly_hour, aes(Month, count)) + 
  geom_bar(stat = "identity", fill = "skyblue")
# Trips for each hour
ggplot(df_by_the_hour, aes(Hour, count)) +
  geom_bar(stat = "identity", fill = "skyblue") + 
  scale_y_continuous(labels = scales::comma) 
# Trips for each day in a month (so every first day in a month)
ggplot(df_days, aes(Day, count)) +
  geom_bar(stat = "identity", fill = "skyblue") + 
  scale_y_continuous(labels = scales::comma)
```

## Creating the Leaflet Map

In order to create the leaflet map, we first need to drastically reduce the amount of data that is going to be rendered. In this, I found `100,000` to be the sweet spot. Since I have multiple times I need to re-render it, I made it a function to be called at anytime. 

```R
createLeaflet <- function() {
  sample_df <- df %>% sample_n(100000, replace = FALSE)
  
  leaflet(sample_df) %>%
    addTiles() %>%
    addMarkers(
      lng = ~Lon,
      lat = ~Lat,
      popup = ~paste("Date:", Month, "/", Day, "/", Year, "<br>Time:", Hour, ":", Minute, ":", Second, "<br>Base: ", Base),
      clusterOptions = markerClusterOptions
    )
}
```

Finally, I also added a button in the shinny app. An example of the above is:

```R
  # Handle the rendering in leaflets.
  # First render it at least once
  output$leaflet <- renderLeaflet({createLeaflet()})
  # Handle the refresh button
  observeEvent(input$refreshButton, {
    output$leaflet <- renderLeaflet({
    createLeaflet()
  })
  })
```

## Shinny App

To put it all together, I set up a Shinny App to showcase the data, which you can view it [here](https://lojosho.shinyapps.io/Uber-Data/)!
