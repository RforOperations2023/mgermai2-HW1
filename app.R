# -----------------------------------#
# Matthew Germaine                   #
# R Shiny for Operations Management  #
# HW #1                              #
# Basic Shiny App                    #
# -----------------------------------#

# libraries to load
library(shiny)
library(readr)
library(shinyWidgets)
library(lubridate)
library(ggplot2)
library(shinythemes)
library(dplyr)
library(stringr)
library(DT)

# readr uses read_csv() - it formats the data as a tibble(), which lets you see format
# also removes the "NA" forms of transportation from the data to be interpreted
d <- read_csv("data/prt-transit.csv") %>%
  mutate(month_start = as.Date(month_start)) %>%
  filter(mode != "NA")

# ui part begins here
ui <- fluidPage(
  
  # sets the theme/coloring of the app
  theme = shinytheme("cerulean"),
  
  # displays the title of the app
  titlePanel(
    "Pittsburgh Regional Transit Ridership, 2017-2022",
  ),
  
  h5("By Matt Germaine"),
  
  # user input part
  sidebarLayout(
    
    sidebarPanel(
      
      p("
        The Pittsburgh Regional Transit (PRT) system has a variety of routes that transport residents of the city
        and the surrounding area in three main ways:  bus, rail, and incline/funicular.  Use the inputs below
        to view a variety of ridership patterns over the course of the past five years.
      "),
      
      p("
        Data is courtesy of the Western Pennsylvania Regional Data Center.  The specific data set used for this
        project (as well as its helpful data dictionary) may be found: 
      "),
      
      a(
        href="https://data.wprdc.org/dataset/prt-monthly-average-ridership-by-route/resource/12bb84ed-397e-435c-8d1b-8ce543108698",
        "HERE"
      ),
      
      br(),
      br(),
      
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
      
      # enables the user to select which routes to view
      selectInput(
        inputId = "route",
        label = "Route Name",
        choices = unique(d$route_full_name),
        selected = "61A - NORTH BRADDOCK",
        multiple = TRUE,
        selectize = TRUE
      ),
      
      # enables the user to select how many routes ("Top X Routes") they want to view
      numericInput(
        inputId = "top",
        label = "Top X Most-Ridden Routes",
        value = 10,
        min = 0,
        max = 50,
        step = 1
      ),
      
      # enables the user to select routes based on the mode of transportation
      checkboxGroupInput(
        inputId = "modes",
        label = "Mode of Transportation:",
        choices = unique(d$mode),
        selected = "Bus"
      ),
      
      br(),
      
      # enables the user to download the data
      # https://shiny.rstudio.com/reference/shiny/1.0.5/downloadbutton
      downloadButton(
        outputId = "downloadData",
        label = "Download Data Table"
      )
      
    ),
    
    # this is the part that actually holds the plotted data
    mainPanel(
      
      plotOutput(outputId = "monthly_ridership"),
      br(),
      
      plotOutput(outputId = "popular_routes_all_time_by_month"),
      br(),
      
      plotOutput(outputId = "popular_routes_all_time_by_day"),
      br(),
      
      br(),
      br(),
      
      # data table stuff
      DT::dataTableOutput(
        outputId = "mytable",
      ),
      
      br(),
      br()
      
    )
  )
)

# server logic goes in here
server <- function(input, output, session) {
  
  ### HELPER FUNCTION TO ADD AN "S" IN THE LINE PLOT TITLE WHEN NEEDED ###
  add_s <- function(number) {
    if (number > 1) {
      return("s")
    } else {
      return("")
    }
  }
  
  ### CREATE THE DATA SUBSETS FIRST ###
  
  # creates the first subset to be plotted in the first (line) plot
  dhat <- reactive({
    # the meaning of a row is route-month-type
    result = d %>%
      # filter out by the user-input date range
      filter(month_start >= input$dates[1], month_start < input$dates[2] ) %>%
      # filter out by the user-input day type
      filter(day_type %in% input$day) %>%
      # filter out by the user-input route names
      filter(route_full_name %in% input$route) %>%
      # create a new field, id, to serve as what's displayed to the user
      mutate(id = paste(route, day_type, sep = "-"))%>%
      # select the data
      select(month_start, avg_riders, id, route, day_type) 
    print("---dhat"); return(result)
  })
  
  # creates the second subset to be plotted in the second plot
  dhat2 <- reactive({
    # the meaning of a row is route-month-type
    result = d %>%
      # filter out by the user-input date range
      filter(month_start >= input$dates[1], month_start < input$dates[2] ) %>%
      # filter out by the user-input day type
      filter(day_type %in% input$day) %>%
      # filter out by the user-input mode of transportation
      filter(mode %in% input$modes) %>%
      # create a new field, id, to serve as what's displayed to the user
      mutate(id = paste(route, day_type, month_start, sep = "-")) %>%
      # create a new field, total_monthly_riders, to serve as the value plotted against the y-axis
      mutate(total_monthly_riders = paste(round(avg_riders * day_count))) %>%
      # grab the top "X" values, as input by the user
      slice_max(avg_riders, n = input$top) %>%
      # select the actual data
      select(month_start, mode, total_monthly_riders, id, route, day_type)
    print("---dhat2"); return(result)
  })
  
  # creates the third subset to be plotted in the third plot
  dhat3 <- reactive({
    result = d %>%
      # filter out by the user-input date range
      filter(month_start >= input$dates[1], month_start < input$dates[2] ) %>%
      # filter out by the user-input day type
      filter(day_type %in% input$day) %>%
      # filter out by the user-input mode of transportation
      filter(mode %in% input$modes) %>%
      # create a new field, id, to serve as what's displayed to the user
      mutate(id = paste(route, day_type, month_start, sep = "-")) %>%
      # mutate(avg_riders = paste(round(avg_riders))) %>%
      slice_max(avg_riders, n = input$top) %>%
      # select the actual data
      select(month_start, avg_riders, id, mode, route, day_type)
    print("---dhat3"); return(result)
  })
  
  
  ### ACTUALLY PLOT THE FIGURES USING THE SUBSETS DEFINED ABOVE ###
  
  # first plot
  output$monthly_ridership <- renderPlot({
    
    dhat() %>%
      ggplot(mapping = aes(
        x = month_start, 
        y = avg_riders, 
        color = id,
      )) +
      xlab("Start of the Month") +
      ylab("Average Number of Riders for the Month") +
      labs(color = "Full Name of Route:") +
      geom_line(alpha = 0.5) + 
      geom_point(alpha = 0.5) +
      ggtitle(paste(str_interp("Average Monthly Ridership for ${length(input$route)} Selected PRT Route${add_s(length(input$route))} Over Time"))) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  # second plot
  output$popular_routes_all_time_by_month <- renderPlot({
    
    dhat2() %>%
      ggplot(mapping = aes(
        x = id,
        y = total_monthly_riders, 
        fill = id,
      )) +
      xlab("Route Name and Day") +
      ylab("Average Number of Riders for the Month") +
      labs(fill="Full Name of Route:") +
      geom_bar(
        alpha = 0.5, 
        stat = "identity"
      ) +
      ggtitle(paste(str_interp("Top ${input$top} All-Time Most Popular Route${add_s(input$top)} Based on Monthly Average Ridership"))) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  # third plot
  output$popular_routes_all_time_by_day <- renderPlot({
    
    dhat3() %>%
      ggplot(mapping = aes(
        x = id, 
        y = avg_riders,
        fill = id
      )) +
      xlab("Route Name and Day") +
      ylab("Number of Riders in A Single Day") +
      labs(fill="Full Name of Route:") +
      geom_bar(alpha = 0.5, stat = "identity") +
      ggtitle(paste(str_interp("Top ${input$top} All-Time Most Popular Route${add_s(input$top)} Based on Single-Day Ridership"))) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  # outputs the data table corresponding to the third plot
  output$mytable = DT::renderDataTable({
    DT::datatable(data = dhat3())
  })
  
  # enables the user to download the datatable created above.
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('prt-transit-data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(dhat3(), con)
    }
  )
  
}

# runs the app
shinyApp(ui = ui, server = server)
