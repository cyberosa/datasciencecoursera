#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# This app shows a small prediction algorithm to find out the last word of a sentence.
# The algorithm learns from the input data and tries to make better predictions in
# future requests
#

library(shiny)
library(DT)
library(tm)
library(SnowballC)
library(tidyr)
library(qdap)
library(dplyr)
library(stringr)
library(stringi)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  titlePanel("NLP prediction experiment"),
  h4('This is a experimental natural language processing algorithm 
           that will try to make a prediction based on the provided sentence'),
  img(src = 'algoBg.png', align = "center"),
  hr(),
  # Sidebar with a slider input for number of bins 
  fluidRow(
    column(4,
      wellPanel(
        h3('Step1'),
        h4('Please enter an incomplete sentence(*):'),
        textInput("inputSentence", label = "", value = "",width = '100%'),
        h5('(*)Keep in mind that numbers and special characters will be ignored'),
        fluidRow(
            column(4, actionButton("goButton", "Predict")),
            column(4, actionButton("clearSentence","Clear"))
        )
      )
    ),
    
    # Show the predicted words
    #mainPanel(
    column(8,
        h4('Suggested words (word stemming is used):'),
       # dataTableOutput("predictedWords"),
        dataTableOutput("predictedWords"),
        verbatimTextOutput("computationTime"),
        #br(),
        # keep feedback for the algorithm
        h3('Step2'),
        textInput("finalWord",label = h4('Please provide your chosen word or an alternative one:'),value = ""),
       fluidRow(
           column(8,actionButton("correctWord", "Send feedback")),
           column(8,verbatimTextOutput("thank"))
       )
    ) 
  )
))
