library(shiny)
library(readr)
library(shinyWidgets)
library(lubridate)
library(ggplot2)
library(shinythemes)


# load data in
TRANSIT_DATA <- read.csv(
  "data/prt-transit.csv",
)

TRANSIT_DATA <- data.frame(TRANSIT_DATA)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # theme = shinytheme("darkly"),
  title = "PRT",
  # displays the title of the app
  titlePanel(
    "PRT Ridership, 2017-2022",
  ),
  
  # sidebar layout for user input and download link
  sidebarLayout(
    
    sidebarPanel(
      
      # enables the user to input a date range
      # dates correspond with the min and max dates found in the dataset
      dateRangeInput(
        inputId = "dates",
        label = "Date Range:",
        start = "2017-01-01",
        end = "2022-11-01",
        min = "2017-01-01",
        max = "2022-11-01",
        startview = "year"
      ),
      
      # enables the user to view the most popular routes
      sliderInput(
        inputId = "top",
        label = "Top Routes:",
        min = 1,
        max = 10,
        value = 1
      ),
      
      # enables the user to select which routes to view
      selectInput(
        inputId = "route_name",
        label = "Route Name",
        choices = unique(TRANSIT_DATA$route_full_name),
        multiple = TRUE,
        selectize = TRUE
      ),
      
      # filter by day
      # enables the user to select which days of data to view
      checkboxGroupInput(
        inputId = "day_type",
        label = "Select a Day Type:",
        choiceNames = list(
          "Weekday",
          "Saturday",
          "Sunday/Holiday"
        ),
        choiceValues = list(
          "WEEKDAY",
          "SAT.",
          "SUN."
        ),
      ),
      checkboxInput(
        inputId = "allDayTypes",
        label = "Select all day types"
      ),
      
      
  
      # enables the user to view ridership details
      radioButtons(
        inputId = "statType", 
        label = "Statistic:",
        c(
          "Average Riders" = "avg_riders",
          "Current Garage" = "current_garage"
        ),
        inline = TRUE
      ),
      
      # dataframe stuff
      DT::dataTableOutput(outputId = "topRoutesDT"),
      
      # # enables the user to download the data
      downloadButton(
        outputId = "prt_ridership",
        label = "Download Data Table"
      )
    
      
      
      
      # # enables the user to select how many modes to display
      # checkboxGroupInput(
      #   inputId = "modes",
      #   label = "Select mode(s) of transportation:",
      #   choices = c(
      #     "Bus",
      #     "Light Rail",
      #     "Incline"
      #   )
      # ),
      
    ),
    
    # this is the part that actually holds the plotted data
    mainPanel(
      
      # okay, so what do I want to display?
      
      # 1)  histogram showing the ridership of the (up to) top 10 most ridden routes, user can select based on the date range
      # 2)  line graph? 
      plotOutput(outputId = "mostPopularRoutes")
        
    )
  )
  
)

# server logic goes in here
server <- function(input, output) {
  
  # Filter on user input for date, day type, and top routes.
  user_filtered <- reactive({
    if (input$allDayTypes) {
      TRANSIT_DATA %>% filter(between(month_start, input$dates[1], input$dates[2])) %>%
        filter(TRANSIT_DATA <= input$top)
    } else {
      TRANSIT_DATA %>% filter(between(month_start, input$dates[1], input$dates[2])) %>% 
        filter(avg_riders <= input$top) %>% filter(day_type %in% input$day_type)
    }
  })
  user_filtered
  
  # filter on individual route stats
  route_stats <- reactive({
    TRANSIT_DATA %>% filter(route_full_name == input$route)
  })
  
  output$mostPopularRoutes <- renderPlot({
    
  })
  
  
  
  
  
  
  
  # # Render plot for average ridership for top x routes
  # output$mostPopularRoutes <- renderPlot({
  #   ggplot(data = user_filtered(), aes(x = route, y = avg_riders, color=route)) +
  #     geom_line() + geom_point() +
  #     ggtitle(paste("Average Ridership for Top", input$top, "PRT Routes", sep = " ")) +
  #     # Referenced for how to center plot title
  #     # https://stackoverflow.com/questions/40675778/center-plot-title-in-ggplot2
  #     theme(plot.title = element_text(hjust = 0.5))
  # })
  
}

# runs the app
shinyApp(ui = ui, server = server)