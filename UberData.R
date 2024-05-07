library(dplyr)
library(tidyr)
library(shiny)
library(vroom)
library(ggplot2)
library(DT)
library(leaflet)
library(leaflet.extras)
library(randomForest)
# Uber Data

# Sets working directory
#setwd('C:/Users/stick/Documents/GitHub/Uber-Data')
# Clear Variables
rm(list = ls())

if (!file.exists("data/merged.rds")) {
# Read all of the raw data
apr_data <- vroom::vroom("data/uber-raw-data-apr14.csv")
aug_data <- vroom::vroom("data/uber-raw-data-aug14.csv")
jul_data <- vroom::vroom("data/uber-raw-data-jul14.csv")
jun_data <- vroom::vroom("data/uber-raw-data-jun14.csv")
may_data <- vroom::vroom("data/uber-raw-data-may14.csv")
sep_data <- vroom::vroom("data/uber-raw-data-sep14.csv")

# Bind them all together into one data frame for easier access
df <- rbind(apr_data, aug_data, jul_data, jun_data, may_data, sep_data)

# Unload all this data since it's been combined (helps save memory)
rm(apr_data, aug_data, jul_data, jun_data, may_data, sep_data)

# Seperate the date from the time
df <- df %>% separate(`Date/Time`, c('Date', 'Time'), sep = " ")
# Handle seperating all of the time
df <- df %>% separate(Time, c('Hour', 'Minute', 'Second'), sep = ":")
df$Second <- NULL # They are all 00 in our dataset, so we can just remove em
# Handle seperating all of the dates
df <- df %>% separate(Date, c('Month', 'Day', 'Year'), sep = "/")
#fwrite(df, "data/mergedData.csv")
saveRDS(df, "data/merged.rds")
} else {
  df <- readRDS("data/merged.rds")
}
# For future loading
#write.csv(df, "data/mergedData.csv")

# Get column names
col_names <- colnames(df)

# Set numbers to numbers so that arranging can be done properly
df$Hour <- as.numeric(df$Hour)
df$Day <- as.numeric(df$Day)

df_by_the_hour <- df %>% 
  group_by(Hour) %>%
  summarize(count = n()) %>%
  arrange(Hour)

df_monthly_hour <- df %>%
  group_by(Month, Hour) %>%
  summarize(count = n())

df_days <- df %>%
  group_by(Day) %>%
  summarize(count = n())

df_bases <- df %>%
  group_by(Base, Month) %>%
  summarize(count = n())
df_bases$Month <- as.character(df_bases$Month)

# Trips for each month
#ggplot(df_monthly_hour, aes(Month, count)) + 
#  geom_bar(stat = "identity", fill = "skyblue")
# Trips for each hour
#ggplot(df_by_the_hour, aes(Hour, count)) +
#  geom_bar(stat = "identity", fill = "skyblue") + 
#  scale_y_continuous(labels = scales::comma) 
# Trips for each day in a month (so every first day in a month)
#ggplot(df_days, aes(Day, count)) +
#  geom_bar(stat = "identity", fill = "skyblue") + 
#  scale_y_continuous(labels = scales::comma) 

# Functions
get_sample <- function(amount) {
  return(df %>% sample_n(amount, replace = FALSE))
}

createLeaflet <- function() {
  sample_df <- get_sample(100000)
  
  leaflet(sample_df) %>%
    addTiles() %>%
    addMarkers(
      lng = ~Lon,
      lat = ~Lat,
      popup = ~paste("Date:", Month, "/", Day, "/", Year, "<br>Time:", Hour, ":", Minute, ":", Second, "<br>Base: ", Base),
      clusterOptions = markerClusterOptions
    )
}

# Prediction Engine:
train_data <- get_sample(750) # I know this is small, but there isn't a lot we can do with our computting power
#test_data <- anti_join(df, train_data)

model_lat <- randomForest(Lat ~ Hour + Minute + Month + Day, data = train_data)
model_lon <- randomForest(Lon ~ Hour + Minute + Month + Day, data = train_data)

# Beware of Shinny Beginnin here!

# Create Leaflet to be used in Shinny App

ui <- fluidPage(
  
  # Application title
  titlePanel("Uber Data"),
  tabsetPanel(
    # Setup the sandbox
    tabPanel("Sandbox", 
             fluidPage(
               titlePanel("The Data Sandbox"), 
               mainPanel(
                 fluidRow(
                   p("There would typically be my sandbox here... however, I do not have the ram to have it run!")
                   # Tried doing a show graph yes/no here, but didn't work out the way I hoped to 
                   #column(1, selectInput("sandboxPlotShow", "Show Plot", c("No", "Yes"), "No")),
                   #column(2,
                   #       selectInput("X", "Choose X", col_names, col_names[4]),
                   #       selectInput("Y", "Choose Y", col_names, col_names[3]),
                   #       selectInput("Splitby", "Split By", col_names, col_names[3])),
                   #column(4, plotOutput("plot_01")),
                   #column(6, DT::dataTableOutput("table_01", width = "100%"))
                 ),
               )
             )
    ),
    # Setup the "Boss View" with graphs
    tabPanel("Boss View", 
             fluidPage(
               titlePanel("Boss View"),
               mainPanel(
                 h1("Monthly Rides - 2014"),
                 fluidRow(
                   column(10, plotOutput("plot_monthly_hours")),
                   column(12, DT::dataTableOutput("table_02", width = "100%"))
                 ),
                 h1("Trips for each hour"),
                 fluidRow(
                   column(10, plotOutput("trips_each_hour")),
                   column(12, DT::dataTableOutput("table_03", width = "100%"))
                 ),
                 h1("Total for each day"),
                 fluidRow(
                   column(10, plotOutput("daily_trips")),
                   column(12, DT::dataTableOutput("table_04", width = "100%"))
                 ),
                 h1("Bases by Month"),
                 fluidRow(
                   column(10, plotOutput("df_bases")),
                   column(12, DT::dataTableOutput("table_df_bases", width = "100%"))
                 )
               )
             )),
    # Setup a leaflet/Interactive map to play around with
    tabPanel("Leaflet", 
             fluidPage(
               titlePanel("Leaflet Heatmap"),
               h3("This may take a second"),
               p("This takes 100,000 randomly selected rows and inputs them to help browsers render the map!"),
               p("If after 30 seconds it does not load, try hitting refresh"),
               p("You can also use refresh to load another sample of data. "),
               mainPanel(
                 actionButton("refreshButton", "Refresh"),
                 # Ughhh, I can't get the resize to work >:(
                 fluidRow(column(4, leafletOutput("leaflet")))),
               ),
             ),
    tabPanel("Prediction Engine", 
             fluidPage(
               titlePanel("Prediction"), 
               p("There was a prediction engine here, however, ShinnyApp kept OOM. It was either Leaflet or this, see code for more details."),
               sidebarLayout(
                 sidebarPanel(
                   # Input panel for time
                   numericInput("hour", "Enter Hour of the Day (0-23):", value = 12, min = 0, max = 23),
                   numericInput("minute", "Enter Minute of the Hour (0-59):", value = 30, min = 0, max = 59),
                   numericInput("month", "Enter Month of the year (0-12):", value = 0, min = 0, max = 12),
                   numericInput("day", "Enter Day of the month (0-31):", value = 15, min = 0, max = 31),
               ),
               mainPanel(
                 textOutput("result_lat"),
                 textOutput("result_lon"),
                 textOutput("result_address"),
                 leafletOutput("predict_leaflet"),
               )
             )))
))


# Define server logic
server <- function(input, output) {
  output$table_02 <- DT::renderDataTable(df_monthly_hour)
  output$table_03 <- DT::renderDataTable(df_by_the_hour)
  output$table_04 <- DT::renderDataTable(df_days)
  output$table_df_bases <- DT::renderDataTable(df_bases)
  
  calculate_position <- reactive({
    # Predict latitude and longitude
    lat <- predict(model_lat, newdata = data.frame(Hour = input$hour, Minute = input$minute, Month = input$month, Day = input$day))
    lon <- predict(model_lon, newdata = data.frame(Hour = input$hour, Minute = input$minute, Month = input$month, Day = input$day))
    
    # Get Address
    url <- paste0("https://nominatim.openstreetmap.org/reverse?format=json&lat=", lat, "&lon=", lon, "&zoom=18&addressdetails=1")
    result <- jsonlite::fromJSON(url)
    
    # Extract address
    address <- ifelse(!is.null(result$display_name), result$display_name, "Address not available")
    
    # Return position and address
    return(list(lat = lat, lon = lon, address = address))
  })
  
  output$result_lat <- renderText({
    pos <- calculate_position()
    result_text <- paste("Latitude: ", pos$lat)
  })
  output$result_lon <- renderText({
    pos <- calculate_position()
    result_text <- paste("Long: ", pos$lon)
  })
  output$result_address <- renderText({
    pos <- calculate_position()
    result_text <- pos$address
  })
  #output$predict_leaflet <- renderLeaflet({
    # Predict latitude and longitude
  #  position <- calculate_position()
    
    # Create leaflet map
  #  leaflet(position) %>%
  #    addTiles() %>%
  #    addMarkers(
  #      lng = ~lon,
  #      lat = ~lat,
  #     popup = ~paste("Address: ", address, "<br>Lat:", lat, "<br>Lon:", lon),
  #      clusterOptions = markerClusterOptions
  #    )
  #})
  
  # Render all the plots in "Boss View"
  output$plot_monthly_hours <- renderPlot({
    ggplot(df_monthly_hour, aes(Month, count)) + 
      geom_bar(stat = "identity", fill = "skyblue")
  })
  
  output$trips_each_hour <- renderPlot({
    ggplot(df_by_the_hour, aes(Hour, count)) +
      geom_bar(stat = "identity", fill = "skyblue") + 
      scale_y_continuous(labels = scales::comma) 
  })
  
  output$daily_trips <- renderPlot(({
    ggplot(df_days, aes(Day, count)) +
      geom_bar(stat = "identity", fill = "skyblue") + 
      scale_y_continuous(labels = scales::comma) 
  }))
  
  output$df_bases <- renderPlot({
    ggplot(df_bases, aes(x = Base, y = count, fill = Month)) +
      geom_bar(stat = "identity", position = "dodge") + 
      theme_minimal() +
      scale_y_continuous(labels = scales::comma) 
  })
  
  # Handle the rendering in leaflets.
  # First render it at least once
  output$leaflet <- renderLeaflet({createLeaflet()})
  # Handle the refresh button
  observeEvent(input$refreshButton, {
    output$leaflet <- renderLeaflet({
    createLeaflet()
  })
  })
  
  
}

# Run the application
shinyApp(ui, server)
 