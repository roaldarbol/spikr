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
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
  titlePanel("SpikeID"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      fileInput("files", "Choose CSV File",
                multiple = TRUE,
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")),
      conditionalPanel(condition="output.fileUploaded",
                       textInput("xvar", "X Variable", "Time"),
                       textInput("yvar", "Y Variable", "Kurt"),
                       numericInput("rate", "Frame Rate:", value = 23.7, min = 1, max = 10000, step = 10),
                       
                       conditionalPanel(
                         condition = "input.manual == false",
                         
                         # Smoothing
                         numericInput("smooth", "Smoothing Constant:", value = 0.06, min = 0, max = 1, step = 0.01),
                         checkboxInput("thresholdinclude", "Include Threshold", TRUE),
                         
                         # Thresholding
                         conditionalPanel(
                           condition = "input.thresholdinclude",
                           selectInput(
                             "thresholdtype", "Threshold Type",
                             c("Minimum", "Mean")
                           ),
                           numericInput("threshold", "Threshold:", value = 0.1, min = 0, max = 1, step = 0.01),
                         ),
                         
                         # Inverting
                         checkboxInput("invert", "Needs Inverting", FALSE),
                         conditionalPanel(
                           condition = "input.invert",
                           checkboxGroupInput("which", "Which to invert?",
                                              c("1" = 1,
                                                "2" = 2,
                                                "3" = 3,
                                                "4" = 4,
                                                "5" = 5,
                                                "6" = 6),
                                              inline = TRUE),
                         ),
                       ),
                       checkboxInput("manual", "Manual Identification", FALSE),
                       conditionalPanel(
                         condition = "input.manual",
                         selectInput(
                           "breaks", "Breaks",
                           c("Sturges", "Scott", "Freedman-Diaconis", "[Custom]" = "custom")
                         ),
                       ),
                       actionButton("goButton", "Plot", class = "btn-success")
      ),
    ),
    # Show a plot of the generated distribution
    mainPanel(
      conditionalPanel(condition="output.fileUploaded",
      plotOutput("rawPlot"),
      plotOutput("smoothPlot")
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$fileUploaded <- reactive({
    if(!is.null(input$files)) return(TRUE)
  })

  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  # Raw plot ----
  output$rawPlot <- renderPlot({
    spike_plot(input, 'raw')
  
    })
  
  # Smooth plot ----
  output$smoothPlot <- renderPlot({
    
    spike_plot(input, 'smooth')
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
