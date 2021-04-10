library(shiny)
library(ggplot2)
library(dplyr)
source("model.R")


shinyServer(function(input, output) {
    pred<-reactive({
        req(input$inputId)
        temps<-predictor.WordPredict(input$inputId)
        if(nrow(temps)>5){
            temps<-head(temps, 5)
        }
        temps
        })
    url<-a("Data Source", href = "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip")
    output$tab<-renderUI({
        tagList(url)
    })
    
    url1<-a("RMarkdown", href = "https://rpubs.com/bharath2925/textanalysis")
    url2<-a("Presentation", href = "https://rpubs.com/bharath2925/smartkeybrd")
    output$tab1<-renderUI({
        tagList(url1)
    })
    
    output$tab2<-renderUI({
        tagList(url2)
    })
    
    url3<-a("Github Repository Link", href = "https://github.com/bharath2925/text-prediction-project-nlp")
    output$tab3<-renderUI({
        tagList(url3)
    })

    output$outputId <- renderText({
                        req(pred())
                        input$inputId})
    output$outputId1 <- renderText({
                        req(pred()$predword[1])
                        paste(input$inputId, " ", pred()$predword[1])})
    output$outputId2 <- renderText({
                        req(pred()$predword[2])
                        paste(input$inputId, " ", pred()$predword[2])})
    output$outputId3 <- renderText({
                        req(pred()$predword[3])
                        paste(input$inputId, " ", pred()$predword[3])})
    output$outputId4 <- renderText({paste(input$inputId, " ", pred()$predword[4])})
    output$outputId5 <- renderText({paste(input$inputId, " ", pred()$predword[5])})
    
    output$plot1 <- renderPlot({
        options(scipen = 999)
        req(pred())
        rows<-nrow(pred())
        if(rows>0){
            pred()[order(-pred()$Probability),] %>%
                ggplot(aes(x = paste(input$inputId, predword), y = Probability*10000)) +
                geom_point() +
                coord_flip() +
                labs(title = "Top 5 Word Predictions", y = "Probabilities", x = "Words")+
                theme_minimal()
        }
    })

})
