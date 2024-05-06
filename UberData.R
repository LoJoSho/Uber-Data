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

local <- F # Set to true when loading on a machine, setting to T rebuilds all of the data
if (local == T) {
# Read all of the raw data
apr_data <- vroom::vroom("data/uber-raw-data-apr14.csv")
aug_data <- vroom::vroom("data/uber-raw-data-aug14.csv")
jul_data <- vroom::vroom("data/uber-raw-data-jul14.csv")
jun_data <- vroom::vroom("data/uber-raw-data-jun14.csv")
may_data <- vroom::vroom("data/uber-raw-data-may14.csv")
sep_data <- vroom::vroom("data/uber-raw-data-sep14.csv")

df <- rbind(apr_data, aug_data, jul_data, jun_data, may_data, sep_data)

# Unload all this data since it's been combined (helps save memory)
rm(apr_data, aug_data, jul_data, jun_data, may_data, sep_data)

# Seperate the date from the time
df <- df %>% separate(`Date/Time`, c('Date', 'Time'), sep = " ")
# Handle seperating all of the time
df <- df %>% separate(Time, c('Hour', 'Minute', 'Second'), sep = ":")
# Handle seperating all of the dates
df <- df %>% separate(Date, c('Month', 'Day', 'Year'), sep = "/")
write.csv(df, "data/mergedData.csv")
} else {
  df <- vroom::vroom("data/mergedData.csv")
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

# Prediction Engine:
train_data <- get_sample(50)
test_data <- anti_join(df, train_data)

model <- randomForest(Lat ~ Hour + Minute + Second + Month + Day + Year, data = train_data)
predictions <- predict(model, test_data)

# Evaluate model performance
mae <- mean(abs(predictions - test_data$Position))
rmse <- sqrt(mean((predictions - test_data$Position)^2))

# Print evaluation metrics
print(paste("Mean Absolute Error:", mae))
print(paste("Root Mean Squared Error:", rmse))

# Beware of Shinny Beginnin here!

# Create Leaflet to be used in Shinny App
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
                 fluidRow(column(4, div(style = "height: 600px !important; width: 100% !important;", leafletOutput("leaflet")))),
               ),
             ))
)
)


# Define server logic
server <- function(input, output) {
  #output$table_01 <- DT::renderDataTable(df[, c(input$X, input$Y, input$Splitby)], 
  #                                       options = list(pageLength = 25))
  
  
  output$table_02 <- DT::renderDataTable(df_monthly_hour)
  output$table_03 <- DT::renderDataTable(df_by_the_hour)
  output$table_04 <- DT::renderDataTable(df_days)
  output$table_df_bases <- DT::renderDataTable(df_bases)
  
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
 