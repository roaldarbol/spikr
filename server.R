# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # UI elements ----
  output$which <- renderUI({
    choice <-  seq(1:length(input_data()))
    checkboxGroupInput("which", "Which to invert?",
                       choices = choice,
                       selected = c(1:length(choice)),
                       inline = TRUE)
  })
  
  output$introduction <- renderUI({
    includeMarkdown("Rmd/Introduction.Rmd")
  })
  
  # Detect uploaded files ----
  output$fileUploaded <- reactive({
    if(!is.null(input$files)) 
      return(TRUE)
  })
  
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  output$smoothed <- reactive({
    smoothed <- input
    return(smoothed)
  })

  
  
  # Import the uploaded data ----
  input_data <- reactive({
    ext <- as.character(tools::file_ext(input$files[[1, 'datapath']]))
    
    # Something here gives the error: Warning: Error in if: argument is of length zero
    # However, it works just fine.
    if (ext == 'csv'){
      dataFiles <- lapply(input$files[['datapath']], read.csv)
    } else if (ext == 'xlsx'){
      dataFiles <- lapply(input$files[['datapath']], readxl::read_xlsx)
    }
    return(dataFiles)
  })
  
  allData <- reactive({
    ext <- as.character(tools::file_ext(input$allFiles[[1, 'datapath']]))
    
    if (ext == 'csv'){
      dataFiles <- lapply(input$allFiles[['datapath']], read.csv)
    } else if (ext == 'xlsx'){
      dataFiles <- lapply(input$allFiles[['datapath']], readxl::read_xlsx)
    }
    
    dataFilesAll <- data.frame()
    for (i in 1:length(dataFiles)){
      dataFilesAll <- rbind(dataFilesAll, dataFiles[[i]])
    }
    
    return(dataFilesAll)
  })

  # Wrangle the uploaded data ----
  wrangled_data <- reactive({
    input$goButton
    
    isolate(
      wrangle_data(data = input_data(), 
                 x = input$xvar, 
                 y = input$yvar,
                 smooth = input$smooth,
                 smooth.cons = input$smooth.cons,
                 fps = input$rate,
                 invert = input$invert,
                 which = input$which,
                 rm.duplicates = input$duplicates,
                 outlier.sd = input$min.height,
                 # threshold = input$threshold,
                 # thres.type = input$thresholdtype,
                 remove = input$manual.remove,
                 info = input$expID)
      )
  })
  
  # Generated output elements ----
  output$summary <- renderTable({
    wrangled_data()[[3]]
  })
  
  figure <- reactive({
    raw_fig <- spike_plot(data = wrangled_data()[1], 
                      xvar = input$xvar,
                      yvar = input$yvar)
    if (isolate(input$smooth) == TRUE){
      smooth_fig <- spike_plot(data = wrangled_data()[2], 
                          xvar = input$xvar,
                          yvar = input$yvar)
      figure <- ggpubr::ggarrange(raw_fig, smooth_fig, nrow = 2, ncol = 1)
    } else {
      figure <- ggpubr::ggarrange(raw_fig, nrow = 1, ncol = 1)
    }
    return(figure)
  })
  
  # power <- reactive({
  #   power <- power_plot(data = wrangled_data()[2],
  #                       xvar = ,
  #                       yvar = 'yvar',
  #                       fps = input$rate)
  #   return(power)
  # })
  
  output$plot <- renderPlot({
    figure()
  })
  
  # output$power <- renderPlot({
  #   power()
  # })
  
  # Elements generated for export with downloadButton ----
  name <- reactive({
    name <- unlist(strsplit(input$expID, ', '))
    name <- paste(name, collapse="_")
    return(name)
  })
  
  output$exportSummary <- downloadHandler(
    filename = function() {
      paste('summary_', name(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(wrangled_data()[[3]], file)
    }
  )
  
  output$exportSpikes <- downloadHandler(
    filename = function() {
      paste('spikes_', name(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(wrangled_data()[[4]], file)
    }
  )
  
  output$exportMeta <- downloadHandler(
    filename = function() {
      paste('metadata_', name(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(wrangled_data()[[5]], file)
    }
  )
  
  output$exportFig <- downloadHandler(
    filename = function() {
      paste('figure_', name(), ".png", sep="")
    },
    content = function(file) {
      ggsave(file, figure(), width = 40, height = 15, units = 'cm', device = 'png')
    }
  )
  
  output$exportAll <- downloadHandler(
    filename = function() {
      paste(input$datatype, 'all.csv', sep="_")
    },
    content = function(file) {
      write.csv(allData(), file)
    }
  )
  
})