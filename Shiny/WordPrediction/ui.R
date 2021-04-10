library(shiny)
shinyUI(fluidPage(
    titlePanel("Word Prediction Application"),
    
    sidebarLayout(
        sidebarPanel(
            h3("Type Here ===>"),
            br(),
            h5("For further information please refer to the Documentation tab")
        ),
        
        
        mainPanel(
            
            tabsetPanel(type = "tabs",
                        tabPanel("Word Predictor",
                                 textInput("inputId", label = "Text Box"),
                                 textOutput("outputId1"),
                                 textOutput("outputId2"),
                                 textOutput("outputId3"),
                                 textOutput("outputId4"),
                                 textOutput("outputId5"),
                                 br(),
                                 br(),
                                 plotOutput("plot1")),
                        tabPanel("Documentation",
                                 h3("Application Documentation"),
                                 br(),
                                 h4("Please use this document to understand the implementation and functions of this application"),
                                 h5("   ->This Shiny data product is a smart keyboard which predicts the next word based on the words typed"),
                                 h5("   ->Word predictions are given as the word is being entered. If there are no predictions to provide, NA is populated."),
                                 h5("   ->The predictions are based on the probabilities derived from text data from multiple sources. Please refer to data source tab for more information on the data source."),
                                 h5("   ->For further information regarding the backend implementation please refer to this document"),
                                 uiOutput("tab1"),
                                 uiOutput("tab2"),
                                 br(),
                                 h4("Known Issues"),
                                 h5("   ->The product rarely does provide predictions which are gibberish (for ex: ujefsd) or multiple repetetions of a word (for ex: youyou)"),
                                 h5("   ->This product does not support the prediction of Names, punctuation marks, Capital letters and website urls."),
                                 h5("   ->The math to build the predictions takes about 10-15 seconds to build. However, this is not observed once the application is initiated and running"),
                                 br(),
                                 h4("Improvements planned"),
                                 h5("   ->Enhance the word predictions by improving the model to predict words in multiple categories including Names, punctuation marks, Capital letters, website urls, places, countries etc."),
                                 h5("   ->Consider necessary elements and try to improve the speed of the product"),
                                 br(),
                                 h4("Steps to deploy"),
                                 h5("In order to replicate this smart keyboard in your local machine. Please follow the below steps"),
                                 h5("Required tools: RStudio, Internet connection"),
                                 h5("   ->This model is easy to deploy. Please use the below github link to clone the repository"),
                                 uiOutput("tab3"),
                                 h5("   ->This includes the raw data, the processed data and the supporting R code to process and build the prediction model"),
                                 h5("   ->This application consumes around 1 GB of physical memory"),
                                 h5("   ->Supporting packages may need to be installed. Supporting code is provided"),
                                 br(),
                                 h4("Data Source"),
                                 h5("   ->The data for this model is taken from multiple online sources which are consolidated by SwiftKey"),
                                 h5("   ->The corpora are collected from publicly available sources by a web crawler. The crawler checks for language, so as to mainly get texts consisting of the desired language"),
                                 h5("   ->data is collected from three major sources, Twitter, Blogs and News"),
                                 h5("   ->Overall corpus size was 0.6 GB. 1% of this data is used to design this smart keyboard"),
                                 h5("Please find below link to the raw data source"),
                                 uiOutput("tab"))
                                 
                        
            )
        )
    )
))
