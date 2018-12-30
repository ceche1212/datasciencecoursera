#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(hflights)
library(dplyr)
library(rsconnect)

data("hflights")
datavuelos<-select(hflights,c(2:5),c(7),c(12:16),c(19),c(21))
mediandeptime<-median(datavuelos$DepTime,na.rm = TRUE)
datavuelos[is.na(datavuelos$DepTime),4]<-mediandeptime
medianArrDelay<-median(datavuelos$ArrDelay,na.rm = TRUE)
datavuelos[is.na(datavuelos$ArrDelay),6]<-medianArrDelay
medianDepDelay<--median(datavuelos$DepDelay,na.rm = TRUE)
datavuelos[is.na(datavuelos$DepDelay),7]<-medianDepDelay
datavuelos[datavuelos$ArrDelay>0,"Status"]="DELAYED"
datavuelos[datavuelos$ArrDelay<=0,"Status"]="ON TIME"
datavuelos$Status<-as.factor(datavuelos$Status)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
        
        output$distPlot1 <- renderPlot({
                
                
                
                # generate bins based on input$bins from ui.R
                x    <- datavuelos[datavuelos$UniqueCarrier==input$airline1, 6]
                
                bins <- seq(min(x), max(x), length.out = input$bins + 1)
                
                # draw the histogram with the specified number of bins
                hist(x, breaks = bins, col = 'steelblue', border = 'white',main = "Histogram of Arrival Delays per Airline",xlab = "Delay (minutes)")
                
        })
        
        output$distPlot2 <- renderPlot({
                
                # generate bins based on input$bins from ui.R
                
                y    <- datavuelos[datavuelos$UniqueCarrier==input$airline2, 6]
                bins <- seq(min(y), max(y), length.out = input$bins + 1)
                
                # draw the histogram with the specified number of bins
                hist(y, breaks = bins, col = 'red', border = 'white',main = "Histogram of Arrival Delays per Airline",xlab = "Delay (minutes)")
                
        })
        
        
        output$meanairline1 <-renderText({
                
                mean(datavuelos[datavuelos$UniqueCarrier==input$airline1, 6])
        })
        
        output$meanairline2 <-renderText({
                
                mean(datavuelos[datavuelos$UniqueCarrier==input$airline2, 6])
        })
        
        output$confidenceint1 <-renderText({
                
                t.test(datavuelos[datavuelos$UniqueCarrier==input$airline1, 6],datavuelos[datavuelos$UniqueCarrier==input$airline2, 6])$conf.int[2]
                
                
        })
        
        output$confidenceint2 <-renderText({
                
                t.test(datavuelos[datavuelos$UniqueCarrier==input$airline1, 6],datavuelos[datavuelos$UniqueCarrier==input$airline2, 6])$conf.int[1]
                
                
        })
        
        
        
        
})
