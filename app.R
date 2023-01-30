library(shiny)
library(readr)
library(shinyWidgets)
library(lubridate)

# load data in
nyt_books <- read_csv(
  "data/nyt_bestsellers_2010_2019 - bestsellers.csv"
)

# date choices
#date choices
# choices <- seq.Date(date("2021-01-01", today() - 1, by = 1))
#                     choices <- choices[!wday(choices) %in% c(1, 7)] #removes weekends
#                     default <- seq.Date(today() - 182, today() - 180, by = 1) 
#                     default <- default[!wday(default) %in% c(1, 7)]
#                     default <- max(default) #most recent weekday


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # ui stuff goes in here
  
  # displays the title of the app
  titlePanel(
    "Exploring NYT Bestsellers, 2010-2019",
  ),
  
  # Sidebar Layout for user input and dateframe display
  sidebarLayout(
    
    sidebarPanel(
      
      # enables the user to input a date range
      # reference for later:  https://stackoverflow.com/questions/66977704/restrict-sliderinput-in-r-shiny-date-range-to-weekdays
      dateRangeInput(
        inputId = "dates",
        label = "Date Range:",
        start = "2010-01-03",
        end = "2019-12-29",
        min = "2010-01-03",
        max = "2019-12-29",
        startview = "year"
      ),
      
      # enables the user to input how many books they want to display
      sliderInput(
        inputId = "number_of_books",
        label = "Number of Books to Display:",
        min = 1, 
        max = 5,
        value = 5,
        step = 1
      ),
      
      # enables the user to select how many genres to display
      checkboxGroupInput(
        inputId = "genres",
        label = "Genres:",
        choices = c(
          "Chapter Books",
          "Hardcover Advice",
          "Hardcover Fiction",
          "Hardcover Graphic Books",
          "Hardcover Nonfiction",
          "Manga",
          "Mass Market Paperback",
          "Paperback Advice",
          "Paperback Books",
          "Paperback Graphic Books",
          "Paperback Nonfiction",
          "Picture Books",
          "Series Books",
          "Trade Fiction Paperback"
        )
      ),
      
      # dataframe stuff
      DT::dataTableOutput(
        outputId = "nyt_bestsellers_dt"
      ),
      
      # enables the user to download the data
      downloadButton(
        outputId = "nyt_bestsellers",
        label = "NYT Bestsellers Data"
      )
      
    ),
    
    # this is the part that actually holds the plotted data
    mainPanel(
      
      # TBD
      plotOutput(outputId = "something"),
      
      plotOutput(outputId = "something else"),
      
      plotOutput(outputId = "something else else")
        
    )
  )
  
)

server <- function(input, output) {
  
  # server logic goes in here
  
}

# runs the app
shinyApp(ui = ui, server = server)