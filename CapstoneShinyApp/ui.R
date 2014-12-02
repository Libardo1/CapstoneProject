# code file for the shiny user interface for the capstone project

library(shiny)

shinyUI(fluidPage(
    titlePanel("Word sequence prediction algorithm"),
    
    fluidRow(
        p("Please input a sequence of words, e.g. an incomplete sentence, and click the \"Predict\" button to get 
            a prediction for the next word in the sequence.")
    ),
    
    fluidRow(
        p("\n")
    ),
    
    sidebarLayout(
        
        sidebarPanel(
            h4("input"),
            textInput("inputString", "Input word sequence:",value = ""),
            submitButton("Predict")
        ),
        
        mainPanel(
            h4("word prediction:"),
            verbatimTextOutput("prediction"),
            p("The underlying prediction algorithm uses a distribution of n-grams up to n=4 which was extracted from a set of newspaper articles, 
            blog posts and tweets on twitter.\n")
        )
    ),
    
    fluidRow(
        p("\n"),
        p("(This Shiny application has been made as part of the Capstone Project of the Data Science Specialization on coursera.org. Author: bursi)")
    )
))
