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
library(shinyalert)


# Top panel ----
top <- fluidRow(
    useShinyalert(),
    column(width = 11,
           conditionalPanel(condition="input.goButton",
                            tabsetPanel(
                                tabPanel("Plot",
                                         column(width = 12,
                                                conditionalPanel(condition="input.goButton",
                                                                 plotOutput("plot")
                                                                 
                                                )
                                         )
                                ),
                                tabPanel('Interactive Plot',
                                         column(width = 12,
                                                plotlyOutput('plotly'))
                                ),
                                tabPanel("Summary",
                                         column(width = 12,
                                                offset = 1,
                                                conditionalPanel(condition="input.goButton",
                                                                 tableOutput("summary")
                                                )
                                         )
                                )
                            )
           )
    )
)

# Middle ----
middle <- fluidRow(
    column(width = 6,
           offset = 2,
           conditionalPanel(condition="output.fileUploaded",
                            
                            # Settings for automated spike selection
                            # Split layout 1
                            splitLayout(
                                cellWidths = 170,
                                cellArgs = list(style = "padding: 6px"),
                                
                                textInput("xvar", "X Variable", "Time"),
                                textInput("yvar", "Y Variable", "Kurt"),
                                numericInput("rate", "Frame Rate:", value = 23.7, min = 1, max = 10000, step = 10),
                                numericInput("min.height", "Outlier criteria, SD:", value = 2, min = 0, max = 10, step = 0.1)
                                # numericInput("threshold", "Threshold:", value = 0.01, min = 0, max = 1, step = 0.01),
                                # selectInput(
                                #     "thresholdtype", "Threshold Type",
                                #     choices = c("Minimum", "Mean"),
                                #     selected = 'Mean'
                                # )
                                
                            ),
                            
                            splitLayout(
                                cellWidths = c(170, 150, 150),
                                cellArgs = list(style = "padding: 6px"),
                                
                                checkboxInput("duplicates", "Remove duplicates?", TRUE),
                                checkboxInput("smooth", "Smooth data?", TRUE),
                                checkboxInput("invert", "Needs Inverting", FALSE),
                                checkboxInput("manual", "Manual Removal", FALSE),
                                checkboxInput("best", "Best 10 sec?", FALSE)
                                
                                
                            ),
                            
                            # Inverting
                            conditionalPanel(
                                condition = "input.invert",
                                uiOutput('which')
                            ),
                            
                            # Manual selection
                            conditionalPanel(
                                condition = "input.manual",
                                textInput('manual.remove', 'Remove Spikes', value = NA, placeholder = 'Type spikes to remove, ie. 1.2, 1.5')
                            ),
                            
                            # Smoothing constant
                            conditionalPanel(
                                condition = 'input.smooth',
                                numericInput("smooth.cons", "Smoothing Constant:", value = 0.01, min = 0, max = 1, step = 0.01)
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
    )
)


# Bottom ----
bottom <- fluidRow(
    column(width = 6,
           offset = 2,
           splitLayout(
               cellWidths = c(300, 400),
               cellArgs = list(style = "padding: 20px"),
               fileInput("files", "Choose Data Files",
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

# Merge Tab ----
merger <- fluidRow(
    column(width = 6,
           offset = 4,
           cellWidths = 300,
           cellArgs = list(style = "padding: 20px"),
           fileInput("allFiles", "Choose Data Files",
                     multiple = TRUE,
                     accept = c(
                         "text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv", '.xlsx')
           ),
           selectInput(
               "datatype", "Data Type",
               c("Summary" = 'summary',
                 "Spike Timing" = 'spike_timing',
                 "Metadata" = 'metadata')
           ),
           downloadButton("exportAll", "Export Data", class = "btn-success")
    )
)


# UI ----
shinyUI(
    navbarPage(title = 'spikr',
               theme = shinytheme("flatly"), 
               collapsible = TRUE,
               id="nav",
               tabPanel("Introduction", uiOutput("introduction")),
               tabPanel("Analyze", 
                        top,
                        middle,
                        bottom),
               tabPanel('Merge',
                        merger)
    )
)

