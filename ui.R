library(shiny)
library(tm)
library(wordcloud)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
    
    # Application title
    headerPanel("Taylor Swift"),
    
    sidebarPanel(
        selectInput("Pilihan", "Pilihan:",
                    list("Word Cloud" = "1", 
                         "Sentiment Graph" = "2", 
                         "Sentiment Word" = "3",
                         "Positive/Negative Word " = "4")),
    ),
    
    mainPanel(
        h3(textOutput("caption")),
        plotOutput("plot"),
    )
))