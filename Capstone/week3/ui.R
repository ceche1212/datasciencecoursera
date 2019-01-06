


library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("slate"),
shinythemes::themeSelector(),
  # Application title
  titlePanel("Capstone Prediction LFPA"),br(),
  
("Project made for the Jhon Hopkins data science specialization"),br(),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       
            textInput("inputText", "ENTER THE TEXT / WORD / SENTENCE HERE",value = ""),
            br(),
            tableOutput("prueba3")
      
    ),
    
    
    
    # Show a plot of the generated distribution
    mainPanel(
            
            h6(em("clean input")),
            textOutput("pretexto"),
            h6(em("output pastewords function")),
            textOutput("searchterm"),
            h4(em("number of predictions in N-grams")),
            h6(textOutput("prueba1")),
            h6(("Predicted word")),
            br(),
            h1(textOutput("prueba2")),
            br(),
            tags$img(src="its-something.jpg",width = 300, height = 200)
            
            
    )
  )
))
