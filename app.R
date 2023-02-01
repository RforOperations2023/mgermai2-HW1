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

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # theme = shinytheme("darkly"),
  
  # ui stuff goes in here
  
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
      
      # enables the user to select which days of data to view
      checkboxGroupInput(
        inputId = "day_of_the_week",
        label = "Select which days:",
        choices = c(
          "Sunday",
          "Monday",
          "Tuesday",
          "Wednesday",
          "Thursday",
          "Friday",
          "Saturday"
        )
      ),
      
      # enables the user to view the most popular routes
      sliderInput(
        inputId = "top",
        label = "Top Routes:",
        min = 1,
        max = 10,
        value = 1
      ),
      
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
      
      # enables the user to select which routes to view
      selectInput(
        inputId = "route_name",
        label = "Route Name",
        choices = unique(TRANSIT_DATA$route_full_name),
        multiple = TRUE,
        selectize = TRUE
      ),
      
      # enables the user to view ridership details
      radioButtons(
        inputId = "statType", 
        label = "Statistic:",
        c(
          "Average Riders" = "avg_riders",
          "Current Garage" = "Average.Points"
          ),
        inline = TRUE
      ),
      
      # selectInput(
      #   inputId = "ridership",
      #   label = "Ridership",
      #   choices = c(
      #     "avg_riders",
      #     "day_count"
      #   ),
      #   selected = "avg_riders"
      # ),
      
      # # dataframe stuff
      DT::dataTableOutput(
        outputId = "topRoutesDT",
      ),
      
      # # enables the user to download the data
      downloadButton(
        outputId = "prt_ridership",
        label = "Download Data Table"
      )
      
    ),
    
    # this is the part that actually holds the plotted data
    mainPanel(
      
      # okay, so what do I want to display?
      
      # 1)  histogram showing the ridership of the (up to) top 10 most ridden routes, user can select based on the date range
      plotOutput(outputId = "routeStats")
        
    )
  )
  
)

# server logic goes in here
server <- function(input, output) {
  
  # Filter on user input for date, top n players, and country
  user_filtered <- reactive({
      # Referenced this to blog for how to use dplyr to filter dataframe based on date range:
      # https://www.r-bloggers.com/2022/06/what-is-the-best-way-to-filter-by-date-in-r/
      TRANSIT_DATA %>% filter(between(month_start, input$dates[1], input$dates[2])) %>%
        filter(avg_riders <= input$top)
  })
  # Filter on individual player stats
  player_stats <- reactive({
    OWGR_Historical %>% filter(Player == toupper(input$player))
  })
  
  output$routeStats <- renderPlot({
    ggplot(
      data=user_filtered(),
      aes(
        x = input$dates, 
        y = input$statType, 
        # fill = x,
        # color = input$statType
      )
    ) + geom_bar(stat = "identity")
    
    # + theme(plot.title = element_text(hjust = 0.5)) 
    # + ylab("Average Points")
    
  })
  
  
  
  # output$scatterplot <- renderPlot({
  #   
  #   ggplot(
  #     data = TRANSIT_DATA,
  #     aes(
  #       x = input$dates,
  #       y = input$ridership
  #     )
  #   )
  #   
  # })
  
}

# runs the app
shinyApp(ui = ui, server = server)