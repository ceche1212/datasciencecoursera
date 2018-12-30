#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(hflights)
library(dplyr)
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

# Define UI for application that draws a histogram
shinyUI(fluidPage(
        
        # Application title
        titlePanel("Flights Data made by Luis Fernando Perez Armas on 12/30/2018"),
        
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
                sidebarPanel(h4("Please Select the number of bins to display the histograms"),
                             sliderInput("bins",
                                         "Number of bins:",
                                         min = 1,
                                         max = 200,
                                         value = 10),
                             h4("Please Select the desired airline company"),
                             selectInput("airline1","Select your desired Airline:",c(unique(datavuelos$UniqueCarrier))),
                             h4("Please Select airline you want to compare to"),
                             selectInput("airline2","Select Airline you want to compare:",c(unique(datavuelos$UniqueCarrier))),
                             
                             h4("mean of arrival delay of airline 1"),
                             textOutput("meanairline1"),
                             h4("mean of arrival delay of airline 2"),
                             textOutput("meanairline2"),
                             h4("t.test between both airlines"),
                             textOutput("confidenceint1"),
                             textOutput("confidenceint2")
                             
                             
                             
                ),
                
                
                
                # Show a plot of the generated distribution
                mainPanel(
                        h3("Comparisson of delay times for two airlines"),
                        em("This dataset contains all flights departing from Houston airports IAH (George Bush Intercontinental) and HOU (Houston Hobby). The data comes from the Research and Innovation Technology Administration at the Bureau of Transporation statistics: http://www.transtats.bts.gov/DatabaseInfo.asp?DB_ID=120&Link=0"),
                        
                        
                        plotOutput("distPlot1"),
                        plotOutput("distPlot2")
                        
                        
                        
                        
                )
        )
))
