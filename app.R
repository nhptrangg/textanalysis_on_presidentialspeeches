#load packages
library(shiny)
library(shinyWidgets)
library(tidyverse)

#Import data
public_approval <- readRDS("public_approval.rds")
speech_Data <- readRDS("SpeechData.rds")
top_terms_after <- readRDS("top_terms_after.rds")
top_terms_before <- readRDS("top_terms_before.rds")

sentiment_choices <- c("Approving", "Disapproving", "Unsure/NoData")

# Define UI for application
ui <- navbarPage(
  
    title = "Topic and Sentiment Analysis of George W. Bush’s Speeches",

    tabPanel(
      title = "George W. Bush's Public Approval",
      sidebarPanel(
        sliderInput(
          "date_range",
          "Select time range:",
          min = min(public_approval$Date, na.rm = TRUE),
          max   = max(public_approval$Date, na.rm = TRUE),
          value = c(min(public_approval$Date),max(public_approval$Date, na.rm = TRUE)),
          step = 1
        ),
        checkboxGroupButtons(
          inputId = "sentiment",
          label = "Select one or more sentiments:",
          choices = sentiment_choices,
          selected = "Approving"
        )
      ),
      mainPanel(
        plotOutput("line_graph", 
                   height = 500,
                   hover = hoverOpts(id = "hover_effect", delay = 100)),
        verbatimTextOutput("hover_effect")
      )
    ),
    
    tabPanel(
      title = "Text Analysis on George W. Bush's speeches",
      h2("Topic Model Before and After Pivot"),
      sidebarPanel(
        selectInput(
          inputId = "period",
          label = "Select period:",
          choices = c("Pre-Pivot", "Post-Pivot"),
          selected = "Pre-Pivot"
        )
        
      ),
      mainPanel(
        plotOutput("topicPlot")
      )
    )

)

# Define server logic required to draw a histogram
server <- function(input, output) {
    #Tab 1: Approval Rating
    filtered_data <- reactive({
      req(input$date_range, input$sentiment)  # wait until inputs exist
    
      public_approval |>
        filter(
          Sentiment %in% input$sentiment,
          Date >= input$date_range[1],
          Date <= input$date_range[2]
        )
    })
    output$line_graph <- renderPlot({
      ggplot(filtered_data(),
             aes(x = Date, y = Percentage_Share, colors = Sentiment)) +
        geom_line(aes(color = Sentiment)) +
        scale_color_manual(values = c("Approving" = "green2", 
                                      "Disapproving" = "red", "Unsure/NoData" = "brown")) +
        labs(title = "George W. Bush Rating Over Time",
             y = "Percentage Share",
             x = "Period of Time") +
        theme_minimal() +
        theme(
          text = element_text(family = "Arial", color = "darkblue"),
          plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)
        )
    })
    
    #hover effect
    #output$hover_effect <- renderText({
      #paste0("Date: ", input$hover_effect, 
             #"\nPercentage Shared: ", input$hover_effect)
    #})
    
    #Tab 2: Text Analysis
    
    max_beta <- max(
      top_terms_before$beta,
      top_terms_after$beta
    )
    
    selected_data <- reactive({
      if (input$period == "Pre-Pivot") {
        top_terms_before
      } else {
        top_terms_after
      }
    })
    
    output$topicPlot <- renderPlot({
      selected_data() |> 
        mutate(term = reorder_within(term, beta, topic)) |>
        ggplot(aes(beta, term, fill = factor(topic))) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~ topic, scales = "free") +
        scale_y_reordered() +
        labs(
          title = paste(input$period, "Top 10 Words for Each Topic"),
          x = "Probability of the Word in Topic (Beta)",
          y = "Word (Term)"
        ) +
        coord_cartesian(xlim = c(0, max_beta))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
