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

data <- read_csv("covid19-download.csv")
min_week <- min(data$date)
max_week <- max(data$date)
sliderRange <- reactiveValues(r = c(min_week, max_week))
value <- reactiveValues(s = "c") # c for confirmed cases and d for deaths

ui <- navbarPage("Canada Covid Dashboard",
  # demonstration of navbar and tab features
  tabPanel("Canada",
           titlePanel("Canada covid statistics"),
           sidebarLayout(
             sidebarPanel(
               sliderInput("weeks",
                           "Range of dates:",
                           min = min_week,
                           max = max_week,
                           value = c(min_week, max_week)),
               radioButtons("value", "Show value for: ",
                            choices = c("Confirmed Cases" = "c", 
                                        "Deaths" = "d"),
                            selected = "c")
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               plotOutput("distPlot")
             )
           )
  ),
  tabPanel("Alberta",
           titlePanel("Alberta covid statistics"),
           sidebarLayout(
             sidebarPanel(
               sliderInput("weeks1",
                           "Range of dates:",
                           min = min_week,
                           max = max_week,
                           value = c(min_week, max_week)),
               radioButtons("value", "Show value for: ",
                            choices = c("Confirmed Cases" = "c", 
                                        "Deaths" = "d"),
                            selected = "c")
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               plotOutput("distPlot1")
             )
           )
  ),
  tabPanel("British Columbia",
           titlePanel("British Columbia covid statistics"),
           sidebarLayout(
             sidebarPanel(
               sliderInput("weeks2",
                           "Range of dates:",
                           min = min_week,
                           max = max_week,
                           value = c(min_week, max_week)),
               radioButtons("value", "Show value for: ",
                            choices = c("Confirmed Cases" = "c", 
                                        "Deaths" = "d"),
                            selected = "c")
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               plotOutput("distPlot2")
             )
           )
  ),
  tabPanel("Ontario",
           titlePanel("Ontario covid statistics"),
           sidebarLayout(
             sidebarPanel(
               sliderInput("weeks3",
                           "Range of dates:",
                           min = min_week,
                           max = max_week,
                           value = c(min_week, max_week)),
               radioButtons("value", "Show value for: ",
                            choices = c("Confirmed Cases" = "c", 
                                        "Deaths" = "d"),
                            selected = "c")
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               plotOutput("distPlot3")
             )
           )
  ),
  tabPanel("Quebec",
           titlePanel("Quebec covid statistics"),
           sidebarLayout(
             sidebarPanel(
               sliderInput("weeks4",
                           "Range of dates:",
                           min = min_week,
                           max = max_week,
                           value = c(min_week, max_week)),
               radioButtons("value", "Show value for: ",
                            choices = c("Confirmed Cases" = "c", 
                                        "Deaths" = "d"),
                            selected = "c")
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               plotOutput("distPlot4")
             )
           )
  ),
  navbarMenu("More",
             # only 1 tab is implemented for demonstration of navbar menu feature
             tabPanel("Manitoba",
                      titlePanel("Manitoba covid statistics"),
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("weeks5",
                                      "Range of dates:",
                                      min = min_week,
                                      max = max_week,
                                      value = c(min_week, max_week)),
                          radioButtons("value", "Show value for: ",
                                       choices = c("Confirmed Cases" = "c", 
                                                   "Deaths" = "d"),
                                       selected = "c")
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("distPlot5")
                        )
                      )
             ),
             tabPanel("New Brunswick",
                      titlePanel("New Brunswick covid statistics"),
                      h3("Not implemented because duplication of code")
             ),
             tabPanel("Newfoundland and Labrador",
                      titlePanel("Newfoundland and Labrador covid statistics"),
                      h3("Not implemented because duplication of code")
             ),
             tabPanel("Northwest Territories",
                      titlePanel("Northwest Territories covid statistics"),
                      h3("Not implemented because duplication of code")
             ),
             tabPanel("Nova Scotia",
                      titlePanel("Nova Scotia covid statistics"),
                      h3("Not implemented because duplication of code")
             ),
             tabPanel("Nunavut",
                      titlePanel("Nunavut covid statistics"),
                      h3("Not implemented because duplication of code")
             ),
             tabPanel("Prince Edward Island",
                      titlePanel("Prince Edward Island covid statistics"),
                      h3("Not implemented because duplication of code")
             ),
             tabPanel("Saskatchewan",
                      titlePanel("Saskatchewan covid statistics"),
                      h3("Not implemented because duplication of code")
             ),
             tabPanel("Yukon",
                      titlePanel("Yukon covid statistics"),
                      h3("Not implemented because duplication of code")
             )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, clientData, session) {
    observe({
      ## sync the slider for all provinces (same as canada)
      ## demonstration of reactive values 
      if (!all(sliderRange$r %in% input$weeks)) {
        updateSliderInput(session, "weeks1", value=input$weeks)
        updateSliderInput(session, "weeks2", value=input$weeks)
        updateSliderInput(session, "weeks3", value=input$weeks)
        updateSliderInput(session, "weeks4", value=input$weeks)
        updateSliderInput(session, "weeks5", value=input$weeks)
        sliderRange$r <- input$weeks
      }
      if (value$s != input$value) {
        updateRadioButtons(session, "value1", selected=input$value)
        updateRadioButtons(session, "value2", selected=input$value)
        updateRadioButtons(session, "value3", selected=input$value)
        updateRadioButtons(session, "value4", selected=input$value)
        updateRadioButtons(session, "value5", selected=input$value)
        value$s <- input$value
      }
    })
  
    output$distPlot <- renderPlot({
        # generate weeks based on input$weeks from ui.R
        if (input$value == "c") {
          data %>% 
            filter(prname == "Canada") %>%
            ggplot(aes(x = date, y = totalcases)) + 
            geom_line()
            
        }

        # draw the histogram with the specified Range of dates
        # hist(x, breaks = weeks, col = 'darkgray', border = 'white',
        #      xlab = 'Waiting time to next eruption (in mins)',
        #      main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
