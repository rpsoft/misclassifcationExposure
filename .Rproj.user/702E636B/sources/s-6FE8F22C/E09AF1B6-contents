#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)


source("misclassification_bias_2019.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Non-differential misclassifcation of exposure"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
      
        # exp.perc and unexp.perc 
           sliderInput("exp",
                     "exp.perc:",
                     min = 0,
                     max = 1,
                     value = 0,
                     step = 0.05),
           sliderInput("unexp",
                       "unexp.perc",
                       min = 0,
                       max = 1,
                       value = 0,
                       step = 0.05)
      ),
      
      mainPanel(
          dataTableOutput('table'),
          textOutput("odds"),
          textOutput("fisher")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   # output$distPlot <- renderPlot({
   #    # generate bins based on input$bins from ui.R
   #    x    <- faithful[, 2] 
   #    bins <- seq(min(x), max(x), length.out = input$bins + 1)
   #    
   #    # draw the histogram with the specified number of bins
   #    hist(x, breaks = bins, col = 'darkgray', border = 'white')
   # })
  
  

    
    output$table <- renderDataTable({

      a <- input$exp
      b <- input$unexp
      
      data <- miscl.exposure(true.dist, a,b)
      
      table <- data[[1]] %>% as.table()
      
    }, options = list(paging = FALSE,searching = FALSE) )
    
    output$odds <- renderText({ 
      a <- input$exp
      b <- input$unexp
      
      data <- miscl.exposure(true.dist, a,b)
      
      paste0("Odds ratio: ",data[[2]])
    })
    
    output$fisher <- renderText({ 
      a <- input$exp
      b <- input$unexp
      
      data <- miscl.exposure(true.dist, a,b)
      
      paste0("Fisher test values : ", data[[3]][1], " , ", data[[3]][2])
    })
  
}
# Run the application 
shinyApp(ui = ui, server = server)

