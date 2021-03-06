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
  p("Rothman, K.J., 2012", em("Epidemiology: an introduction"), ", Oxford University Press. (p. 135)"),
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
        conditionalPanel(
          condition = "input.exp == 0 && input.unexp  == 0",
          p("This first table shows the situation where all subjects are correctly classified with respect 
            to the outcome (i.e. MI), and the exposure (i.e. eating a high fat diet). In reality, this distribtion is almost never truely known.",
            br(),
            "Change the sliders on the left to adjust the amount of misclassification there is from high fat diet to non, and vice versa.")
          ),
        conditionalPanel(
          condition = "input$exp != 0 || input$unexp  != 0",
          p("Note that when you change the misclassification sliders, only the row totals change - the column totals of those classified as an MI Case or Control do not change.")
        ),
        

          DT::dataTableOutput('table'),
          textOutput("odds"),
          textOutput("fisher"),
          p(br(),
            br(),
            "See how the following situations changes the odds ratio:",
            br(),
            "- 20% of unexposed misclassified as exposed",
            br(),
            "- 20% of exposed misclassified as unexposed",
            br(),
            "- 20% misclassified in both directions")
      )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
    
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

