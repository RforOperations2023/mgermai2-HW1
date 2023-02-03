# Things I still want to add:
# Name/Date, etc. (both to the app itself and to these comments)  -- DONE
# Clean up axis labels.
# Clean up plot scales (use scales library?)
# Edit wording, grammar, etc.
# Try again to figure out the ordering of the legend stuff so that it's easier to read.
# Try and get rid of "NA" as a mode of transportation to select.
# Make sure that the add_s() function works as expected (since it currently doesn't)
# Clean up legend titles/make them easier to understand
# Update README
# Perhaps choose a different theme so that the data table is more easily readable.
# Add comments.
# Clean up variable names.
# Push up to R Shiny Apps.
# Send shiny apps link and github repo to Geoffrey.
# BONUS:  try and make data table dynamically incorporate user's input. -- DONE


# -----------------------------------#
# Matthew Germaine                   #
# R Shiny for Operations Management  #
# HW #1                              #
# Basic Shiny App                    #
# -----------------------------------#
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
  theme = shinytheme("darkly"),

  # displays the title of the app
  titlePanel(
    "Pittsburgh Regional Transit Ridership, 2017-2022",
  ),
  
  h5("By Matt Germaine"),
  
  # user input part
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
      
      # enables the user to view routes by total monthly ridership
      sliderInput(
        inputId = "ridership",
        label = "Total Monthly Ridership:",
        min = 0,
        max = max(
          12000 # could change later if needed when dataset is updated...
          # if so, it would look like this:
          # d$avg_riders, # could be a whole number instead of the max
          # na.rm = TRUE
        ),
        value = c(
          0, 
          max(
            12000 # could change later if needed when dataset is updated...
            # if so, it would look like this:
            # d$avg_riders, # could be a whole number instead of the max
            # na.rm = TRUE
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
      
      # enables the user to select how many routes ("Top X Routes") they want to view
      numericInput(
        inputId = "top",
        label = "Top X Routes",
        value = 10,
        min = 0,
        max = 50
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
      
      plotOutput(outputId = "popular_routes_all_time"),
      br(),
      
      plotOutput(outputId = "popular_specific_by_type"),
      br(),
      
      br(),
      br(),
      
      # dataframe stuff
      DT::dataTableOutput(
        outputId = "mytable",
      )
      
    )
  )
  
)

# server logic goes in here
server <- function(input, output, session) {
  
  # HELPER FUNCTION TO ADD AN "S" IN THE LINE PLOT TITLE WHEN NEEDED
  add_s <- function(number) {
    if (number > 1) {
      return("s")
    } else {
      return("")
    }
  }
  
  # creates the first subset to be plotted in the first (line) plot
  dhat <- reactive({
    # the meaning of a row is route-month-type
    result = d %>%
      # filter out by the user-input date range
      filter(month_start >= input$dates[1], month_start < input$dates[2] ) %>%
      # filter out by the user-input ridership
      filter(avg_riders >= input$ridership[1] & avg_riders <= input$ridership[2]) %>%
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
      # create a new field, id, serve as what's displayed to the user
      mutate(id = paste(route, day_type, month_start, sep = "-")) %>%
      # create a new field, total_monthly_riders, to serve as the value plotted against the y-axis
      mutate(total_monthly_riders = paste(round(avg_riders * day_count))) %>%
      # grab the top "X" values, as input by the user
      slice_max(avg_riders, n = input$top) %>%
      # select the actual data
      select(month_start, mode, total_monthly_riders, id, route, day_type)
    print("---dhat2"); return(result)
  })
  
  # need to figure this out...
  dhat3 <- reactive({
    result = d %>%
      filter(month_start >= input$dates[1], month_start < input$dates[2] ) %>%
      filter(day_type %in% input$day) %>%
      filter(mode %in% input$modes) %>%
      mutate(id = paste(route, day_type, month_start, sep = "-")) %>%
      mutate(total_monthly_riders = paste(round(avg_riders))) %>%
      slice_max(avg_riders, n = input$top) %>%
      select(month_start, total_monthly_riders, id, mode, route, day_type)
    print("---dhat3"); return(result)
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
      xlab("Start of the Month") +
      ylab("Average Number of Riders for the Month") +
      geom_line(alpha = 0.5) + 
      geom_point(alpha = 0.5) +
      guides(
        title = "Title",
        color = "none"
      ) +
      ggtitle(paste(str_interp("Average Monthly Ridership for ${length(input$route)} Selected PRT Route${add_s(length(input$route))} Over Time"))) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  
  output$popular_routes_all_time <- renderPlot({
    
    dhat2() %>%
      ggplot(mapping = aes(
        # x = reorder(id, desc(total_monthly_riders)), 
        x = id,
        y = total_monthly_riders, 
        fill = id,
        color = id
      )) +
      xlab("Route Name and Day") +
      ylab("Average Number of Riders for the Month") +
      geom_bar(
        alpha = 0.5, 
        stat = "identity"
      ) +
      # https://stackoverflow.com/questions/64848319/how-to-synchronise-the-legend-the-with-order-of-the-bars-in-ggplot2
      guides(
        title = "Title",
        color = "none", 
        guide_legend(reverse = FALSE)
      ) +
      ggtitle(paste(str_interp("Top ${input$top} All-Time Most Popular Route${add_s(input$top)} Based on Monthly Average Ridership"))) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  
  output$popular_specific_by_type <- renderPlot({
    
    dhat3() %>%
      ggplot(mapping = aes(
        x = reorder(id, desc(total_monthly_riders)), 
        y = total_monthly_riders,
        fill = id
      )) +
      xlab("Route Name and Day") +
      ylab("Number of Riders in A Single Day") +
      geom_bar(alpha = 0.5, stat = "identity") +
      # https://stackoverflow.com/questions/64848319/how-to-synchronise-the-legend-the-with-order-of-the-bars-in-ggplot2
      guides(
        title = "Title",
        color = "none", 
        guide_legend(reverse = FALSE)
      ) +
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