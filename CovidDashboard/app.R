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
                                        "Deaths" = "d", 
                                        "Both" = "b"),
                            selected = "c")
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               plotOutput("distPlot")
             )
           )
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
                                        "Deaths" = "d", 
                                        "Both" = "b"),
                            selected = "c"),
               checkboxGroupInput("provinces", label = "Choose provinces to compare:", 
                                  choices = list("Canada" = 1, "Alberta" = 2, "British Columbia" = 3,
                                                 "Ontario" = 4, "Quebec" = 5),
                                  selected = c(1,2,3))
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               plotOutput("distPlot0")
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
               radioButtons("value1", "Show value for: ",
                            choices = c("Confirmed Cases" = "c", 
                                        "Deaths" = "d", 
                                        "Both" = "b"),
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
               radioButtons("value2", "Show value for: ",
                            choices = c("Confirmed Cases" = "c", 
                                        "Deaths" = "d", 
                                        "Both" = "b"),
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
               radioButtons("value3", "Show value for: ",
                            choices = c("Confirmed Cases" = "c", 
                                        "Deaths" = "d", 
                                        "Both" = "b"),
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
               radioButtons("value4", "Show value for: ",
                            choices = c("Confirmed Cases" = "c", 
                                        "Deaths" = "d", 
                                        "Both" = "b"),
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
                          radioButtons("value5", "Show value for: ",
                                       choices = c("Confirmed Cases" = "c", 
                                                   "Deaths" = "d", 
                                                   "Both" = "b"),
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
  
    # Canada plot
    output$distPlot <- renderPlot({
        # generate weeks based on input$weeks from ui.R
        if (input$value == "c") {
          data %>% 
            filter(prname == "Canada") %>%
            filter(date >= input$weeks[1] & date <= input$weeks[2]) %>%
            ggplot(aes(x = date, y = totalcases)) + 
            geom_line()
        } else if (input$value == "d") {
          data %>% 
            filter(prname == "Canada") %>%
            filter(date >= input$weeks[1] & date <= input$weeks[2]) %>%
            ggplot(aes(x = date, y = numdeaths)) + 
            geom_line()
        } else if (input$value == "b") {
          data %>% 
            filter(prname == "Canada") %>%
            filter(date >= input$weeks[1] & date <= input$weeks[2]) %>% 
            pivot_longer(cols=c("totalcases", "numdeaths"),
                         names_to = "status",
                         values_to = "value") %>%
            ggplot(aes(x = date, y = value, color=status)) +
            geom_line()
        }
    })
    
    # Comparison 
    # Manitoba plot
    output$distPlot0 <- renderPlot({
      # generate weeks based on input$weeks from ui.R
      if (input$value0 == "c") {
        data %>% 
          filter(prname %in% input$provinces) %>%
          filter(date >= input$weeks0[1] & date <= input$weeks0[2]) %>%
          ggplot(aes(x = date, y = totalcases)) + 
          geom_line()
      } else if (input$value5 == "d") {
        data %>% 
          filter(prname %in% input$provinces) %>%
          filter(date >= input$weeks0[1] & date <= input$weeks0[2]) %>%
          ggplot(aes(x = date, y = numdeaths)) + 
          geom_line()
      } else if (input$value5 == "b") {
        data %>% 
          filter(prname %in% input$provinces) %>%
          filter(date >= input$weeks0[1] & date <= input$weeks0[2]) %>% 
          pivot_longer(cols=c("totalcases", "numdeaths"),
                       names_to = "status",
                       values_to = "value") %>%
          ggplot(aes(x = date, y = value, color=status)) +
          geom_line()
      }
    })
    
    # Alberta plot
    output$distPlot1 <- renderPlot({
      # generate weeks based on input$weeks from ui.R
      if (input$value1 == "c") {
        data %>% 
          filter(prname == "Alberta") %>%
          filter(date >= input$weeks1[1] & date <= input$weeks1[2]) %>%
          ggplot(aes(x = date, y = totalcases)) + 
          geom_line()
      } else if (input$value1 == "d") {
        data %>% 
          filter(prname == "Alberta") %>%
          filter(date >= input$weeks1[1] & date <= input$weeks1[2]) %>%
          ggplot(aes(x = date, y = numdeaths)) + 
          geom_line()
      } else if (input$value1 == "b") {
        data %>% 
          filter(prname == "Alberta") %>%
          filter(date >= input$weeks1[1] & date <= input$weeks1[2]) %>% 
          pivot_longer(cols=c("totalcases", "numdeaths"),
                       names_to = "status",
                       values_to = "value") %>%
          ggplot(aes(x = date, y = value, color=status)) +
          geom_line()
      }
    })
    
    # British Columbia plot
    output$distPlot2 <- renderPlot({
      # generate weeks based on input$weeks from ui.R
      if (input$value2 == "c") {
        data %>% 
          filter(prname == "British Columbia") %>%
          filter(date >= input$weeks2[1] & date <= input$weeks2[2]) %>%
          ggplot(aes(x = date, y = totalcases)) + 
          geom_line()
      } else if (input$value2 == "d") {
        data %>% 
          filter(prname == "British Columbia") %>%
          filter(date >= input$weeks2[1] & date <= input$weeks2[2]) %>%
          ggplot(aes(x = date, y = numdeaths)) + 
          geom_line()
      } else if (input$value2 == "b") {
        data %>% 
          filter(prname == "British Columbia") %>%
          filter(date >= input$weeks2[1] & date <= input$weeks2[2]) %>% 
          pivot_longer(cols=c("totalcases", "numdeaths"),
                       names_to = "status",
                       values_to = "value") %>%
          ggplot(aes(x = date, y = value, color=status)) +
          geom_line()
      }
    })
    
    # Ontario plot
    output$distPlot3 <- renderPlot({
      # generate weeks based on input$weeks from ui.R
      if (input$value3 == "c") {
        data %>% 
          filter(prname == "Ontario") %>%
          filter(date >= input$weeks3[1] & date <= input$weeks3[2]) %>%
          ggplot(aes(x = date, y = totalcases)) + 
          geom_line()
      } else if (input$value3 == "d") {
        data %>% 
          filter(prname == "Ontario") %>%
          filter(date >= input$weeks3[1] & date <= input$weeks3[2]) %>%
          ggplot(aes(x = date, y = numdeaths)) + 
          geom_line()
      } else if (input$value3 == "b") {
        data %>% 
          filter(prname == "Ontario") %>%
          filter(date >= input$weeks3[1] & date <= input$weeks3[2]) %>% 
          pivot_longer(cols=c("totalcases", "numdeaths"),
                       names_to = "status",
                       values_to = "value") %>%
          ggplot(aes(x = date, y = value, color=status)) +
          geom_line()
      }
    })
    
    # Quebec plot
    output$distPlot4 <- renderPlot({
      # generate weeks based on input$weeks from ui.R
      if (input$value4 == "c") {
        data %>% 
          filter(prname == "Quebec") %>%
          filter(date >= input$weeks4[1] & date <= input$weeks4[2]) %>%
          ggplot(aes(x = date, y = totalcases)) + 
          geom_line()
      } else if (input$value4 == "d") {
        data %>% 
          filter(prname == "Quebec") %>%
          filter(date >= input$weeks4[1] & date <= input$weeks4[2]) %>%
          ggplot(aes(x = date, y = numdeaths)) + 
          geom_line()
      } else if (input$value4 == "b") {
        data %>% 
          filter(prname == "Quebec") %>%
          filter(date >= input$weeks4[1] & date <= input$weeks4[2]) %>% 
          pivot_longer(cols=c("totalcases", "numdeaths"),
                       names_to = "status",
                       values_to = "value") %>%
          ggplot(aes(x = date, y = value, color=status)) +
          geom_line()
      }
    })
    
    # Manitoba plot
    output$distPlot5 <- renderPlot({
      # generate weeks based on input$weeks from ui.R
      if (input$value5 == "c") {
        data %>% 
          filter(prname == "Manitoba") %>%
          filter(date >= input$weeks5[1] & date <= input$weeks5[2]) %>%
          ggplot(aes(x = date, y = totalcases)) + 
          geom_line()
      } else if (input$value5 == "d") {
        data %>% 
          filter(prname == "Manitoba") %>%
          filter(date >= input$weeks5[1] & date <= input$weeks5[2]) %>%
          ggplot(aes(x = date, y = numdeaths)) + 
          geom_line()
      } else if (input$value5 == "b") {
        data %>% 
          filter(prname == "Manitoba") %>%
          filter(date >= input$weeks5[1] & date <= input$weeks5[2]) %>% 
          pivot_longer(cols=c("totalcases", "numdeaths"),
                       names_to = "status",
                       values_to = "value") %>%
          ggplot(aes(x = date, y = value, color=status)) +
          geom_line()
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
