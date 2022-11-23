#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(DT)

# global variables
# Feature 0: download directly from the source. In this case, the app will always 
# show the most recent dataset from the source.
data <- read_csv("https://health-infobase.canada.ca/src/data/covidLive/covid19-download.csv")
min_week <- min(data$date)
max_week <- max(data$date)

provinces <- list("Canada",
                  "Alberta", 
                  "British Columbia",
                  "Ontario",
                  "Quebec",
                  "Manitoba",
                  "New Brunswick",
                  "Newfoundland and Labrador",
                  "Northwest Territories",
                  "Nova Scotia",
                  "Nunavut",
                  "Prince Edward Island",
                  "Saskatchewan",
                  "Yukon")

ui <- navbarPage("Canada Covid Dashboard",
  # Feature 1: navbar and tab
  # demonstration of navbar and tab features:
  # navbar and tabs make the app pages organized
  tabPanel("Province Detail",
           titlePanel("Covid statistics"),
           sidebarLayout(
             sidebarPanel(
               # Feature 2: collapsed menu,
               # demonstration of collapsed menu for user to select a single 
               # province to plot its data
               selectInput("select", label = h3("Select province or Canada"), 
                           choices = provinces,
                           selected = "Canada"),
               
               # Feature 3: slider
               # demonstration of slider feature for user to select a range of 
               # date for plots. User can focus on specific range of dates for 
               # the covid cases
               sliderInput("weeks",
                           "Range of dates:",
                           min = min_week,
                           max = max_week,
                           value = c(min_week, max_week)),
               
               # Feature 4: radio button
               # demonstration of radio button feature for user to select what data
               # to display
               radioButtons("value", "Show value for: ",
                            choices = c("Confirmed Cases" = "c", 
                                        "Deaths" = "d", 
                                        "Both" = "b"),
                            selected = "c")
             ),
             
             mainPanel(
               plotOutput("distPlot")
             )
           ),
  ),
  
  tabPanel("Comparison",
           titlePanel("Compare covid statistics within Canada"),
           sidebarLayout(
             sidebarPanel(
               sliderInput("weeks0",
                           "Range of dates:",
                           min = min_week,
                           max = max_week,
                           value = c(min_week, max_week)),
               radioButtons("value0", "Show value for: ",
                            choices = c("Confirmed Cases" = "c", 
                                        "Deaths" = "d"),
                            selected = "c"),
               
               # Feature 5: check box
               # demonstration of check box feature for user to select one or more
               # provinces for comparison. User can compare multiple the data in
               # multiple provinces
               checkboxGroupInput("provinces", label = "Choose provinces to compare:", 
                                  choices = provinces,
                                  selected = c("Canada",
                                               "Alberta", 
                                               "British Columbia",
                                               "Ontario",
                                               "Quebec",
                                               "Manitoba")
                                  )
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               plotOutput("distPlot0")
             )
           )
  ),
  
  tabPanel("Data",
           titlePanel("Covid statistics data within Canada"),
           # Feature 6: data download
           # demonstration of data download function such that user can make a
           # copy of the data used in this app
           downloadButton("downloadData", "Download Dataset"),
           
           # show acknowledgement
           uiOutput("url"),
           
           # Feature 7: table display
           # demonstration of table display for user to preview the data within 
           # the app
           DT::dataTableOutput("data"))
  
  )

# Define server logic required to draw a histogram
server <- function(input, output, clientData, session) {
  # Comparison 
  # Feature 8: plots
  # demonstration of plot features. User can visually compare the data in different
  # provinces
  output$distPlot0 <- renderPlot({
    if (input$value0 == "c") {
      data %>% 
        filter(prname %in% input$provinces) %>%
        filter(date >= input$weeks0[1] & date <= input$weeks0[2]) %>%
        ggplot(aes(x = date, y = totalcases, color = prname)) + 
        geom_line(size = 1)
    } else if (input$value0 == "d") {
      data %>% 
        filter(prname %in% input$provinces) %>%
        filter(date >= input$weeks0[1] & date <= input$weeks0[2]) %>%
        ggplot(aes(x = date, y = numdeaths, color = prname)) + 
        geom_line(size = 1)
    }
  })
  
    # Canada or province plot
    output$distPlot <- renderPlot({
        # generate weeks based on input$weeks from ui.R
        if (input$value == "c") {
          data %>% 
            filter(prname == input$select) %>%
            filter(date >= input$weeks[1] & date <= input$weeks[2]) %>%
            ggplot(aes(x = date, y = totalcases)) + 
            geom_line(size=1)
        } else if (input$value == "d") {
          data %>% 
            filter(prname == input$select) %>%
            filter(date >= input$weeks[1] & date <= input$weeks[2]) %>%
            ggplot(aes(x = date, y = numdeaths)) + 
            geom_line(size=1)
        } else if (input$value == "b") {
          data %>% 
            filter(prname == input$select) %>%
            filter(date >= input$weeks[1] & date <= input$weeks[2]) %>% 
            pivot_longer(cols=c("totalcases", "numdeaths"),
                         names_to = "status",
                         values_to = "value") %>%
            ggplot(aes(x = date, y = value, color=status)) +
            geom_line(size=1)
        }
    })
    
    # produce data table
    output$data <- DT::renderDataTable(
      DT::datatable(data, options = list(pageLength = 15))
    )
    
    # download data button functions
    output$downloadData <- downloadHandler(
      filename = "covid19-canada.csv",
      content = function(file) {
        write.csv(data, file, row.names = FALSE)
      }
    )
    
    # acknowledgement display
    url <- h3(a("https://www.canada.ca/en/public-health/services/diseases/2019-novel-coronavirus-infection.html", 
                href="https://www.canada.ca/en/public-health/services/diseases/2019-novel-coronavirus-infection.html"))
    ack <- h3("Acknowledgement: The dataset is created by Government of Canada and downloaded from: ")
    output$url <- renderUI({
      tagList(ack, url)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
