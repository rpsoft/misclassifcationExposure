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
library(DT)


source("misclassification_bias_2019.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
  h2("Non-differential misclassification of exposure status in a hypothetical case-control study"),
  p("Rothman, K.J., 2012", em("Eidemiology: an introduction"), ", Oxford University Press. (p. 135)"),
  p("This table reprents hypothetical data for a Case-Control study investigating the effects of a high fat diet on the odds of having a subsequent Myocardial infarction (MI). 
    The participant's diets are recorded, and classified as 'high fat' (exposed) or 'not' (unexposed). As there will be an arbitrary cut-off point at which a diet is regarded as being 'high fat',
    it is plausible that some of the participants will have their exposure status misclassified. "),
      
    sidebarPanel(

        # exp.perc and unexp.perc 
           sliderInput("exp",
                     "% of exposed persons misclassified as unexposed:",
                     min = 0,
                     max = 100,
                     value = 0,
                     step = 5),
           sliderInput("unexp",
                       "% of unexposed persons misclassified as exposed",
                       min = 0,
                       max = 100,
                       value = 0,
                       step = 5)
      ),
      
      mainPanel(
          textOutput("explan"),
          DT::dataTableOutput('table'),
          textOutput("odds"),
          textOutput("fisher")
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
  

    output$explan <- renderText({
      a <- input$exp / 100
      b <- input$unexp / 100
      if (a== 0 & b == 0) {
        paste0("This first table shows the situation where all subjects are correctly classified with respect 
            to the outcome (i.e. MI), and the exposure (i.e. eating a high fat diet).
            In reality, this distribtion is almost never truely known.")
      } else {
        paste0("")
      }
    })
    
    
    output$table <- DT::renderDataTable({
      a <- input$exp / 100
      b <- input$unexp / 100
      data <- miscl.exposure(true.dist, a,b)
      DT::datatable(data[[1]], 
                    rownames=TRUE, 
                    filter='none', 
                    options = list(dom = 't'))
      
    })
    
    output$odds <- renderText({ 
      a <- input$exp / 100
      b <- input$unexp / 100
      data <- miscl.exposure(true.dist, a,b)
      paste0("Odds ratio: ",data[[2]])
    })
    
    output$fisher <- renderText({ 
      a <- input$exp / 100
      b <- input$unexp /100
      data <- miscl.exposure(true.dist, a,b)
      paste0("95% Confidence Intervals : (", data[[3]][1], " , ", data[[3]][2],")")
    })
  
}
# Run the application 
shinyApp(ui = ui, server = server)

