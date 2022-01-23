#
# This is a Shiny web application about the swiss Fertility (1888) data set. You can run the application by clicking
# the 'Run App' button above.
#
#

library(shiny)
library(ggplot2)
swiss <- datasets::swiss
# Define UI for application 
ui <- fluidPage(
  
  # Application title
  titlePanel("Correlations with Fertility rate (Swiss 1888)"),
  
  sidebarLayout(
    sidebarPanel(
      # select variables (without fertility rate itself)
      selectInput("variable", label= "Choose a variable to plot against Fertility rate", 
                  choices = colnames(swiss)[-1])
    ),
    
      mainPanel(
      p("The R dataset 'swiss' contains the standardized fertility measure and socio-economic indicators for each of 47 French-speaking provinces of Switzerland at about 1888."),
      textOutput("headerSum"),
      verbatimTextOutput("Sum"),
      h3(strong("Scatter plot"), align="center"),
      plotOutput("FertilityCor"),
      verbatimTextOutput("caption")
      
    )
  )
)

server <- function(input, output) {
  
  # Return the formula text for printing as a caption 
  output$headerSum <- renderText({paste("Summary statistics of chosen variable:",  variab())
  })
  output$Sum <- renderPrint({
   summary(swiss[,(variab())])
  })
  
  variab <- reactive({ input$variable
  })

  

    # Data #(possibility to make subset)
    plotdata <- swiss


  #Correlation plot of fertility with other variable
  formulaText <- reactive({
    paste("Fertility ~", input$variable)
  })
  
  # Return the formula text for printing as a caption 
  output$caption <- renderText({
    formulaText()
  })
  
  # Generate a plot of the requested variable against fertility
  output$FertilityCor <- renderPlot({
    plot(as.formula(formulaText()),
         data = swiss,
         type= "p", pch = 10)
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)

