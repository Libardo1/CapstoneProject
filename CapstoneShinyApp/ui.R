# code file for the shiny user interface for the capstone project

library(shiny)

shinyUI(pageWithSidebar(
    headerPanel("word prediction"),
    sidebarPanel(
        textInput("inputString", "Incomplete phrase",value = ""),
        submitButton("Submit")
    ),
    mainPanel(
        verbatimTextOutput("prediction")
    )
))
