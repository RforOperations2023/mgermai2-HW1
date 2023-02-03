# load data in
TRANSIT_DATA <- read.csv(
  "data/prt-transit.csv",
)

head(TRANSIT_DATA)

par(mfrow = c(3, 1))

hist(TRANSIT_DATA$avg_riders [TRANSIT_DATA$mode == "Light Rail"],
     xlim = c(0,3),
     breaks = 9,
     main = "Blah de blah",
     xlab = "",
     col= "red"
     )

hist(TRANSIT_DATA$avg_riders [TRANSIT_DATA$mode == "Bus"],
     xlim = c(0,3),
     breaks = 9,
     main = "Blah de blah",
     xlab = "",
     col= "purple"
)

hist(TRANSIT_DATA$avg_riders [TRANSIT_DATA$mode == "Incline"],
     xlim = c(0,3),
     breaks = 9,
     main = "Blah de blah",
     xlab = "",
     col= "blue"
)


# NEW_DATA <- head(TRANSIT_DATA)
# 
# # summary table
# average_riders <- table(NEW_DATA$avg_riders)
# barplot(average_riders)

# plot(TRANSIT_DATA$route_full_name[1:3], TRANSIT_DATA$avg_riders[1:3],
#      col = "#cc0000",
#      pch = 19,
#      main = "Something here",
#      xlab = "Route Name",
#      ylab = "Average Riders"
#      )






# # okay, so these are all the libraries he uses
# 
# require(pacman)
# library(pacman)
# 
# pacman::p_load(shiny, readr, ggplot2, dplyr, DT, tools, strinr, shinythemes)
# # library(shiny)
# # library(readr)
# # library(ggplot2)
# # library(dplyr)
# # library(DT)
# # library(tools)
# # library(stringr)
# # library(shinythemes)
# 
# # I think this just reads in the CSV data and makes sure that the dates are properly formatted
# OWGR_Historical <- read_csv("OWGR Historical.csv", 
#                             col_types = cols(Date = col_date(format = "%Y-%m-%d")))
# 
# # pretty sure this gets all of the unique countries?
# unique(OWGR_Historical$Country)
# 
# # okay, pretty sure I don't need this part because my column names do not have spaces in them
# colnames(OWGR_Historical) <- make.names(colnames(OWGR_Historical))
# 
# # Define UI for application
# ui <- fluidPage(
#   theme = shinytheme("cyborg"),
#   
#   title = "OWGR",
#   # Application title
#   titlePanel(
#     h1("Official World Golf Rankings since 1997", align = "center")
#   ),
#   
#   # Sidebar Layout for user input and dateframe display
#   sidebarLayout(
#     sidebarPanel(
#       # Date range user input
#       dateRangeInput(inputId = "dates",
#                      label = "Date Range:",
#                      start = "1997-01-01",
#                      end = "2004-04-03",
#                      min = "1997-01-01",
#                      max = "2022-04-03",
#                      startview = "year"),
#       # Slider input for number of top players to display
#       sliderInput(inputId = "top",
#                   label = "Top _ Players:",
#                   min = 1, max = 10,
#                   value = 1),
#       # Multiple select input for country
#       selectInput(inputId = "country",
#                   label = "Country:",
#                   choices = unique(OWGR_Historical$Country),
#                   selected = "UNITED STATES",
#                   multiple = TRUE,
#                   selectize = TRUE),
#       checkboxInput(inputId = "allCountries",
#                     label = "Select all Countries"),
#       # Dataframe display underneath first set of user inputs
#       DT::dataTableOutput(outputId = "topPlayersDT"),
#       downloadLink('downloadData', "Download Data Table"),
#       hr(),
#       hr(),
#       # Text input for player lifetime world ranking
#       textInput(inputId = "player",
#                 label = "Search Player Lifetime Stats:",
#                 value = "TIGER WOODS"),
#       radioButtons("statType", 
#                    "Statistic:",
#                    c("World Ranking" = "OWGR",
#                      "Average Points" = "Average.Points"),
#                    inline = TRUE)
#     ),
#     
#     # Show a plot of the generated distribution
#     mainPanel(
#       # Plot output for player scoring vs time
#       plotOutput("num1plot"),
#       # Plot number of weeks in top x
#       plotOutput("topPlayers"),
#       # Break for visualization
#       br(),
#       br(),
#       plotOutput("playerStats"),
#       br(),
#       br()
#     )
#   )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
#   # Reactive objects - filter and group data for use in plots and dataframe
#   
#   # Filter on user input for date, top n players, and country
#   user_filtered <- reactive({
#     if (input$allCountries) {
#       OWGR_Historical %>% filter(between(Date, input$dates[1], input$dates[2])) %>%
#         filter(OWGR <= input$top)
#     } else {
#       # Referenced this to blog for how to use dplyr to filter dataframe based on date range:
#       # https://www.r-bloggers.com/2022/06/what-is-the-best-way-to-filter-by-date-in-r/
#       OWGR_Historical %>% filter(between(Date, input$dates[1], input$dates[2])) %>%
#         filter(OWGR <= input$top) %>% filter(Country %in% input$country)
#     }
#   })
#   # Filter on individual player stats
#   player_stats <- reactive({
#     OWGR_Historical %>% filter(Player == toupper(input$player))
#   })
#   # Group by player and count number of weeks in top n
#   weeks_in_top <- reactive({
#     user_filtered() %>%
#       group_by(Player) %>%
#       count(Player) %>%
#       arrange(desc(n))
#   })
#   # Render plot for Average Points for top x players
#   output$num1plot <- renderPlot({
#     ggplot(data = user_filtered(), aes(x = Date, y = Average.Points, color = Player)) +
#       geom_line() + geom_point() +
#       ggtitle(paste("Average Points for Top", input$top, "Players", sep = " ")) +
#       # Referenced for how to center plot title
#       # https://stackoverflow.com/questions/40675778/center-plot-title-in-ggplot2
#       theme(plot.title = element_text(hjust = 0.5))
#   })
#   # Render plot for total number of weeks in top x
#   output$topPlayers <- renderPlot({
#     ggplot(data = weeks_in_top(), aes(x = reorder(Player, -n), y = n, fill = Player)) +
#       geom_bar(stat="identity") +
#       guides(color = "none") +
#       theme(axis.text.x = element_text(angle = 90)) +
#       ggtitle(paste("Total Number of Weeks in Top", input$top, sep = " ")) +
#       theme(plot.title = element_text(hjust = 0.5)) +
#       xlab("Player") +
#       ylab("Number of Weeks")
#     
#   })
#   # Render plot for Player Lifetime Stats
#   output$playerStats <- renderPlot({
#     if (input$statType == "OWGR") {
#       ggplot(data = player_stats(), aes_string(x = "Date", y = input$statType, color = input$statType)) +
#         geom_line() + geom_point() +
#         scale_y_continuous(trans = "reverse") +
#         ggtitle(paste("Lifetime World Ranking for", stringr::str_to_title(input$player), sep = " ")) +
#         theme(plot.title = element_text(hjust = 0.5)) +
#         ylab("Official World Golf Ranking")
#     } else {
#       ggplot(data = player_stats(), aes_string(x = "Date", y = input$statType, color = input$statType)) +
#         geom_line() + geom_point() +
#         ggtitle(paste("Lifetime Average Points for", stringr::str_to_title(input$player), sep = " ")) +
#         theme(plot.title = element_text(hjust = 0.5)) +
#         ylab("Average Points")
#     }
#     
#   })
#   # Render Dataframe for total number of weeks in top x
#   output$topPlayersDT <- DT::renderDataTable({
#     DT::datatable(data = weeks_in_top(),
#                   options = list(pagelength = 15),
#                   rownames = FALSE,
#                   colnames = list("Player", paste("# Weeks in Top", input$top, sep = " ")))
#   })
#   # Download handler for download data table
#   output$downloadData <- downloadHandler(
#     filename = function() {
#       paste("WeeksInTop_", input$top, ".csv", sep = "")
#     },
#     content = function(file) {
#       write.csv(weeks_in_top(), file)
#     }
#   )
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)