


UG<-readRDS("unigram.RData")
BG<-readRDS("bigram.RData")
TG<-readRDS("trigram.RData")
QG<-readRDS("quadgram.RData")
TOP<-readRDS("topworduni.Rdata")
default<-as.character(TOP[1])

library(shiny)
library(ggplot2)
library(quanteda)
library(data.table)
library(dplyr)

cleanInput <- function(x){
        x <- tolower(x)
        x <- gsub("\\S*[0-9]+\\S*", " ", x)
        x <- gsub("[^[:alnum:][:space:]'-]", " ", x)
        x <- gsub("(\\w['-]\\w)|[[:punct:]]", "\\1", x)
        x <- gsub("\\s+"," ",x)
        x <- gsub("^\\s+|\\s+$", "", x)
        return(x) 
}

splitwordfinal <- function(x){
        
        
        x<-cleanInput(x)
        x<-unlist(strsplit(x, " "))
        m <- length(x)
        return(x[m])

}

splitwordpenultima <- function(x){
        
        
        x<-cleanInput(x)
        x<-unlist(strsplit(x, " "))
        m <- length(x)
        return(x[m-1])
        
}

pastewords<-function(x){
        
        x<-cleanInput(x)
        x<-unlist(strsplit(x, " "))
        m <- length(x)
        
        if (m==0){
                
                y<-"NA"
        }
        
        if (m>=3){
                
                y<-paste0(x[m-2]," ",x[m-1]," ",x[m])    
                
        }
        
        if (m==2){
                
                y<-paste0(x[m-1]," ",x[m])    
                
        }
        
        if (m==1){
                
                y<-x[m]
        }
        
        return(y)
}

Quantity_of_matches<-function(x){
        
        x<-pastewords(x)
        m<-length(unlist(strsplit(x, " ")))
        
        if(m==1){
                
                intermedia<-BG[grepl(as.character(x),BG$word),]
                intermedia<-arrange(intermedia,desc(intermedia$count))
                w<-nrow(intermedia)
                
                y<-intermedia[1,1]
                y<-unlist(strsplit(y, " "))
                
                
                return(w)
                
                
        }
        if(m==2){
                
                intermedia<-TG[grepl(as.character(x),TG$word),]
                intermedia<-arrange(intermedia,desc(intermedia$count))
                w<-nrow(intermedia)
                
                y<-intermedia[1,1]
                y<-unlist(strsplit(y, " "))
                
                
                return(w)
                
                
        }
        if(m>=3){
                
                intermedia<-QG[grepl(as.character(x),QG$word),]
                intermedia<-arrange(intermedia,desc(intermedia$count))
                
                w<-nrow(intermedia)
                
                y<-intermedia[1,1]
                y<-unlist(strsplit(y, " "))
                return(w)
                
                
        }

}

Predicted_word<-function(x){
        
        x<-pastewords(x)
        m<-length(unlist(strsplit(x, " ")))
        
        if(m==0){
                
                word<-default
                return(word)
        }
        
        if(m==1){
                
                intermedia<-BG[grepl(as.character(x),BG$word),]
                intermedia<-arrange(intermedia,desc(intermedia$count))
                w<-nrow(intermedia)
                
                y<-intermedia[1,1]
                y<-unlist(strsplit(y, " "))
                word<-y[2]
                prueba5<-is.na(word)
                
                if(prueba5){
                        
                        return(default)
                }
                
                
                return(word)
                
                
        }
        if(m==2){
                
                intermedia<-TG[grepl(as.character(x),TG$word),]
                intermedia<-arrange(intermedia,desc(intermedia$count))
                w<-nrow(intermedia)
                
                y<-intermedia[1,1]
                y<-unlist(strsplit(y, " "))
                word<-y[3]
                prueba3<-is.na(word)
                
                if (prueba3){
                        
                        x4<-unlist(strsplit(x, " "))
                        m4<-length(x4)
                        new_conv4<-x4[m4]
                        
                        intermedia4<-BG[grepl(as.character(new_conv4),BG$word),]
                        intermedia4<-arrange(intermedia4,desc(intermedia4$count))
                        y4<-intermedia4[1,1]
                        y4<-unlist(strsplit(y4, " "))
                        word4<-y4[2]
                        return(word4)
                        
                        prueba4<-is.na(word4)
                        
                        if (prueba4){
                                
                                
                                return(default)  
                                
                        }
                        
                }
                
                
                
                
                
                return(word)
                
                
        }
        if(m>=3){
                
                intermedia<-QG[grepl(as.character(x),QG$word),]
                intermedia<-arrange(intermedia,desc(intermedia$count))
                
                w<-nrow(intermedia)
                
                y<-intermedia[1,1]
                y<-unlist(strsplit(y, " "))
                word<-y[4]
                prueba<-is.na(word)
                
                if(prueba){
                        
                        x1<-unlist(strsplit(x, " "))
                        
                        new_conv<-paste0(x1[2]," ",x1[3])
                        
                        intermedia1<-TG[grepl(as.character(new_conv),TG$word),]
                        intermedia1<-arrange(intermedia1,desc(intermedia1$count))
                        y1<-intermedia1[1,1]
                        y1<-unlist(strsplit(y1, " "))
                        word1<-y1[3]
                        return(word1)
                        
                        prueba1<-is.na(word1)
                        
                        if(prueba1){
                                
                                x2<-unlist(strsplit(new_conv, " "))
                                
                                new_conv2<-x2[2]
                                
                                intermedia2<-BG[grepl(as.character(new_conv2),BG$word),]
                                intermedia2<-arrange(intermedia2,desc(intermedia2$count))
                                y2<-intermedia2[1,1]
                                y2<-unlist(strsplit(y2, " "))
                                word2<-y2[2]
                                return(word2)
                                
                                prueba2<-is.na(word2)
                                
                                if(prueba2){
                                        
                                        
                                        return(default)
                                }
                                
                                
                                
                                
                        }
                        
                        
                        
                }
                
                else{
                        return(word)   
                }
                
                
                
        }
        
}

secondary_predictions<-function(x){
        
        x<-pastewords(x)
        m<-length(unlist(strsplit(x, " ")))
        
        if(m==1){
                
                intermedia<-BG[grepl(as.character(x),BG$word),]
                intermedia<-arrange(intermedia,desc(intermedia$count))
                
                y<-intermedia[-1,]
                y<-head(intermedia,10)
                
                return(y)
                
                
        }
        if(m==2){
                
                intermedia<-TG[grepl(as.character(x),TG$word),]
                intermedia<-arrange(intermedia,desc(intermedia$count))
                
                y<-intermedia[-1,]
                y<-head(intermedia,10)
                
                return(y)
                
                
        }
        if(m>=3){
                
                intermedia<-QG[grepl(as.character(x),QG$word),]
                intermedia<-arrange(intermedia,desc(intermedia$count))
                
        
                y<-intermedia[-1,]
                y<-head(intermedia,10)
                
                return(y)
                
                
        }
        
}


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
        
        
   
  output$pretexto <- renderText({cleanInput(input$inputText)
    
    
    
    
  })
  
  
  
  output$searchterm <- renderText({pastewords(input$inputText)
          
          
          
          
  })
  
  output$prueba1 <- renderText({Quantity_of_matches(input$inputText)
          
          
          
          
  })
  
  output$prueba2 <- renderText({Predicted_word(input$inputText)
          
          
          
          
  })
  
  output$prueba3 <- renderTable({secondary_predictions(input$inputText)
          
          
          
          
  })
 
  
})
