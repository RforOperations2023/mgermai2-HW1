library(shiny)
library(readr)
library(shinyWidgets)
library(lubridate)
library(ggplot2)
library(shinythemes)


# load data in
TRANSIT_DATA <- read.csv(
  "data/prt-transit.csv",
) %>%
  mutate(month_start = as.Date(month_start))

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
        selected = "61A - NORTH BRADDOCK",
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
      
      # dataframe stuff
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
      
      plotOutput(outputId = "mostPopularRoutes")
      
    )
  )
  
)

# server logic goes in here
server <- function(input, output) {
  
  # Filter on user input for date, top n players, and country
  user_filtered <- reactive({
    
    df <- TRANSIT_DATA %>% filter(between(month_start, input$dates[1], input$dates[2])) %>% 
      filter(avg_riders <= input$top)
    
    print(summary(df))
    
    return(df)
  })
  
  output$mostPopularRoutes <- renderPlot({
    ggplot(data = user_filtered(), aes(x = month_start, y = avg_riders, color=route)) +
      geom_line() + geom_point() +
      ggtitle(paste("Average Ridership for Top", input$top, "PRT Routes", sep = " ")) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
}

# runs the app
shinyApp(ui = ui, server = server)