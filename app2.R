library(shiny)
library(readr)
library(shinyWidgets)
library(lubridate)
library(ggplot2)
library(shinythemes)
library(dplyr)
library(stringr)


# readr uses read_csv() - it formats the data as a tibble(), which lets you see format
d <- read_csv("data/prt-transit.csv") %>%
  mutate(month_start = as.Date(month_start))

# Pipe or Pipeline %>%


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  theme = shinytheme("darkly"),
  
  # ui stuff goes in here
  
  # displays the title of the app
  titlePanel(
    "Pittsburgh Regional Transit Ridership, 2017-2022",
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
        inputId = "day",
        label = "Select which days:",
        choices = c(
          "Saturdays" = "SAT.", 
          "Sundays/Holidays" = "SUN.", 
          "Weekdays" = "WEEKDAY"
        ), 
        selected = c("WEEKDAY") 
      ),
      
      # enables the user to view the most popular routes
      sliderInput(
        inputId = "ridership",
        label = "Total Monthly Ridership:",
        min = 0,
        max = max(
          d$avg_riders, # could be a whole number instead of the max
          na.rm = TRUE
        ),
        value = c(
          0, 
          max(
            d$avg_riders, # could be a whole number instead of the max
            na.rm = TRUE
          )
        )
      ),
      
      # enables the user to select which routes to view
      selectInput(
        inputId = "route",
        label = "Route Name",
        choices = unique(d$route_full_name),
        selected = "61A - NORTH BRADDOCK",
        multiple = TRUE,
        selectize = TRUE
      ),
      
      numericInput(
        inputId = "top",
        label = "Top X Routes",
        value = 10,
        min = 0,
        max = 50
      ),
      
      # dataframe stuff
      DT::dataTableOutput(
        outputId = "topRoutesDT",
      ),
      
      # enables the user to download the data
      downloadButton(
        outputId = "prt_ridership",
        label = "Download Data Table"
      )
      
    ),
    
    # this is the part that actually holds the plotted data
    mainPanel(
      
      plotOutput(outputId = "monthly_ridership"),
      
      plotOutput(outputId = "popular_routes")
      
    )
  )
  
)

# server logic goes in here
server <- function(input, output) {
  
  # HELPER FUNCTION TO ADD AN "S" IN THE LINE PLOT TITLE WHEN NEEDED
  add_s <- function(number) {
    if (length(number) > 1 || number > 1) {
      return("s")
    } else {
      return("")
    }
  }
  
  # FILTERS ON USER INPUT FOR MONTH_START, AVG_RIDERS, DAY_TYPE, AND ROUTE_FULL_NAME
  dhat <- reactive({
    # MAKE THE SUBSET
    # THE MEANING OF A ROW IS A ROUTE-MONTH-DAYTYPE
    result = d %>%
      filter(month_start >= input$dates[1], month_start < input$dates[2] ) %>%
      filter(avg_riders >= input$ridership[1] & avg_riders <= input$ridership[2]) %>%
      filter(day_type %in% input$day) %>%
      filter(route_full_name %in% input$route) %>%
      mutate(id = paste(route, day_type, sep = "-"))%>%
      select(month_start, avg_riders, id, route, day_type) 
    print("---dhat"); return(result)
  })
  
  # FILTERS ON USER INPUT FOR MONTH_START, AVG_RIDERS, DAY_TYPE, AND ROUTE_FULL_NAME
  dhat2 <- reactive({
    # MAKE THE SUBSET
    # THE MEANING OF A ROW IS A ROUTE-MONTH-DAYTYPE
    result = d %>%
      filter(month_start >= input$dates[1], month_start < input$dates[2] ) %>%
      filter(day_type %in% input$day) %>%
      mutate(id = paste(route, day_type, month_start, sep = "-")) %>%
      mutate(total_monthly_riders = paste(round(avg_riders * day_count))) %>%
      slice_max(avg_riders, n = input$top) %>%
      select(month_start, mode, total_monthly_riders, id, route, day_type)
    print("---dhat2"); return(result)
  })
  
  output$monthly_ridership <- renderPlot({
  
    dhat() %>%
      ggplot(mapping = aes(
        x = month_start, 
        y = avg_riders, 
        group = id, 
        color = id, 
        fill = id
      )) +
      geom_line(alpha = 0.5) + 
      geom_point(alpha = 0.5) +
      guides(color = "none") +
      ggtitle(paste(str_interp("Average Monthly Ridership for ${length(input$route)} Selected PRT Route${add_s(input$route)} Over Time"))) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  
  output$popular_routes <- renderPlot({
    
    dhat2() %>%
      ggplot(mapping = aes(
        x = reorder(id, desc(total_monthly_riders)), 
        y = total_monthly_riders, 
        fill = mode
      )) +
      geom_bar(alpha = 0.5, stat = "identity") +
      guides(color = "none") +
      ggtitle(paste(str_interp("Top ${input$top} Most Popular Route${add_s(input$top)} Based on Monthly Average Ridership"))) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  
  
  # BAR CHART THAT SHOWS THE TOP X ROUTES IN TERMS OF AVERAGE MONTHLY RIDERSHIP
  
  
}



# runs the app
shinyApp(ui = ui, server = server)