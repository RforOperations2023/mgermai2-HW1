# Things I still want to add:
# Name/Date, etc.
# Clean up axis labels.
# Clean up plot scales (use scales library?)
# Edit wording, grammar, etc.
# Try again to figure out the ordering of the legend stuff so that it's easier to read.
# Try and get rid of "NA" as a mode of transportation to select.
# Make sure that the add_s() function works as expected (since it currently doesn't)
# Clean up legend titles/make them easier to understand
# Update README
# Perhaps choose a different theme so that the data table is more easily readable.
# BONUS:  try and make data table dynamically incorporate user's input.

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
  
  h5("By Matt Germaine"),
  
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
          12000
          # d$avg_riders, # could be a whole number instead of the max
          # na.rm = TRUE
        ),
        value = c(
          0, 
          max(
            12000
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
      
      numericInput(
        inputId = "top",
        label = "Top X Routes",
        value = 10,
        min = 0,
        max = 50
      ),
      
      checkboxGroupInput(
        inputId = "modes",
        label = "Mode of Transportation:",
        choices = unique(d$mode),
        selected = "Bus"
      ),
      
      # # https://stackoverflow.com/questions/28829682/r-shiny-checkboxgroupinput-select-all-checkboxes-by-click
      # actionLink(
      #   inputId = "allModes",
      #   label = "Select All Modes of Transportation"
      # ),
      
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
  
  # https://stackoverflow.com/questions/28829682/r-shiny-checkboxgroupinput-select-all-checkboxes-by-click
  # observe({
  #   if(input$allModes == 0) return(NULL) 
  #   else if (input$allModes%%2 == 0)
  #   {
  #     updateCheckboxGroupInput(session, "modes","Mode of Transportation:", choices=unique(d$mode))
  #   }
  #   else
  #   {
  #     updateCheckboxGroupInput(session,"modes","Mode of Transportation:", choices=unique(d$mode),selected=unique(d$mode))
  #   }
  # })
  
  # HELPER FUNCTION TO ADD AN "S" IN THE LINE PLOT TITLE WHEN NEEDED
  add_s <- function(number) {
    if (length(number) > 1) {
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
  
  dhat3 <- reactive({
    result = d %>%
      filter(month_start >= input$dates[1], month_start < input$dates[2] ) %>%
      filter(day_type %in% input$day) %>%
      mutate(id = paste(route, day_type, month_start, sep = "-")) %>%
      mutate(total_monthly_riders = paste(round(avg_riders))) %>%
      filter(mode %in% input$modes) %>%
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
      geom_line(alpha = 0.5) + 
      geom_point(alpha = 0.5) +
      guides(
        title = "Title",
        color = "none"
      ) +
      ggtitle(paste(str_interp("Average Monthly Ridership for ${length(input$route)} Selected PRT Route${add_s(input$route)} Over Time"))) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  
  output$popular_routes_all_time <- renderPlot({
    
    dhat2() %>%
      ggplot(mapping = aes(
        x = reorder(id, desc(total_monthly_riders)), 
        y = total_monthly_riders, 
        fill = id,
        color = id
      )) +
      geom_bar(alpha = 0.5, stat = "identity") +
      # https://stackoverflow.com/questions/64848319/how-to-synchronise-the-legend-the-with-order-of-the-bars-in-ggplot2
      guides(
        title = "Title",
        color = "none", 
        guide_legend(reverse = FALSE)
      ) +
      ggtitle(paste(str_interp("Top ${input$top} Most Popular Route${add_s(input$top)} Based on Monthly Average Ridership"))) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  
  output$popular_specific_by_type <- renderPlot({
    
    dhat3() %>%
      ggplot(mapping = aes(
        x = reorder(id, desc(total_monthly_riders)), 
        y = total_monthly_riders,
        fill = id
      )) +
      geom_bar(alpha = 0.5, stat = "identity") +
      # https://stackoverflow.com/questions/64848319/how-to-synchronise-the-legend-the-with-order-of-the-bars-in-ggplot2
      guides(
        title = "Title",
        color = "none", 
        guide_legend(reverse = FALSE)
      ) +
      ggtitle(paste(str_interp("Top ${input$top} Most Popular ${input$mode} Route${add_s(input$top)} Based on Single-Day Ridership"))) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  
  output$mytable = DT::renderDataTable({
    d
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('prt-transit-data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(d, con)
    }
  )
  
  
  
  # BAR CHART THAT SHOWS THE TOP X ROUTES IN TERMS OF AVERAGE MONTHLY RIDERSHIP
  
  
}



# runs the app
shinyApp(ui = ui, server = server)