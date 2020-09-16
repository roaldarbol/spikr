#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)


# Top panel ----
top <- fluidRow(
    column(width = 6,
           splitLayout(
               cellWidths = c(300, 400),
               cellArgs = list(style = "padding: 20px"),
               fileInput("files", "Choose CSV File",
                         multiple = TRUE,
                         accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv", '.xlsx')
                         ),
               textInput('expID', 'Experiment ID', value = NA, placeholder = 'Seperate with commas. E.g. date, exp, treatment')
           )
    )
)

# Middle ----
middle <- fluidRow(
    column(width = 6,
           conditionalPanel(condition="output.fileUploaded",
                            
                            # Settings for automated spike selection
                            # Split layout 1
                            splitLayout(
                                cellWidths = 150,
                                cellArgs = list(style = "padding: 6px"),
                                
                                textInput("xvar", "X Variable", "Time"),
                                textInput("yvar", "Y Variable", "Kurt"),
                                numericInput("rate", "Frame Rate:", value = 23.7, min = 1, max = 10000, step = 10)
                                ),
                            
                            # Split layout 2
                                splitLayout(
                                    cellWidths = 150,
                                    cellArgs = list(style = "padding: 6px"),
                                    

                                    numericInput("smooth", "Smoothing Constant:", value = 0.1, min = 0, max = 1, step = 0.01),
                                    numericInput("threshold", "Threshold:", value = 0.01, min = 0, max = 1, step = 0.01),
                                    selectInput(
                                        "thresholdtype", "Threshold Type",
                                        choices = c("Minimum", "Mean"),
                                        selected = 'Mean'
                                    )
                            ),
                            
                            splitLayout(
                                cellWidths = c(150, 150, 175, 300),
                                cellArgs = list(style = "padding: 6px"),
                                checkboxInput("invert", "Needs Inverting", FALSE),
                                checkboxInput("manual", "Manual Removal", FALSE)
                                
                                
                            ),
                            
                            # Inverting
                            conditionalPanel(
                                condition = "input.invert",
                                checkboxGroupInput("which", "Which to invert?",
                                                   c("1" = 1,
                                                     "2" = 2,
                                                     "3" = 3,
                                                     "4" = 4,
                                                     "5" = 5,
                                                     "6" = 6),
                                                   selected = c(1:6),
                                                   inline = TRUE)
                            ),
                            
                            # Manual selection
                            conditionalPanel(
                                condition = "input.manual",
                                textInput('manual.remove', 'Remove Spikes', value = NA, placeholder = 'Type spikes to remove, ie. 1.2, 1.5')
                            )
           ),
           conditionalPanel(condition="output.fileUploaded",
                            splitLayout(
                                cellWidths = c(100, 170, 150, 170, 170),
                                cellArgs = list(style = "padding: 2px"),
                                actionButton("goButton", "Submit", class = "btn-success"),
                                downloadButton("exportSummary", "Export Summary", class = "btn-success"),
                                downloadButton("exportSpikes", "Export Spikes", class = "btn-success"),
                                downloadButton("exportMeta", "Export Metadata", class = "btn-success"),
                                downloadButton("exportFig", "Export Figure", class = "btn-success")
                                
                            )
           )
    ),
    
    column(width = 6,
           conditionalPanel(condition="input.goButton",
                            tableOutput('summary'))),
)


# Bottom ----
bottom <- fluidRow(
    column(width = 12,
           conditionalPanel(condition="input.goButton",
                            
                            # Action button
                            plotOutput("doublePlot", height = '400px')
           )
    )
)

# UI ----
shinyUI(
    navbarPage(title = 'spikr',
               theme = shinytheme("flatly"), 
               collapsible = TRUE,
               id="nav",
               tabPanel("Analyze", 
                        bottom,
                        middle,
                        top),
               tabPanel('Merge')
    )
)

