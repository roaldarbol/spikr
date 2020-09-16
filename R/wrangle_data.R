smooth_it <- function(data, xvar, yvar, smooth){
  df <- list()
  df.Smooth <- list()
  
  for (i in 1:length(data)){
    df[[i]] <- smooth.spline(data[[i]][[yvar]], spar = smooth)
    df.Smooth[[i]] <- tibble(.rows = length(df[[i]]$y))
    df.Smooth[[i]]$yvar <- df[[i]]$y
    df.Smooth[[i]]$xvar <- data[[i]]$xvar
    if (!is.null(data[[i]]$minute)){
      df.Smooth[[i]]$minute <- data[[i]]$minute
    }
  }
  
  # Potential for Loess filter instead
  # for (i in 1:length(data)){
  #   df[[i]] <- loess(yvar ~ xvar, data[[i]], span = smooth)
  #   df.Smooth[[i]] <- tibble(.rows = length(df[[i]]$fitted))
  #   df.Smooth[[i]]$y_fitted <- df[[i]]$fitted
  #   df.Smooth[[i]]$xvar <- df[[i]]$x
  #   if (!is.null(data[[i]]$minute)){
  #     df.Smooth[[i]]$minute <- data[[i]]$minute
  #   }
  # }
  return(df.Smooth)
}


wrangle_data <- function(data, x, y, fps, smooth, invert, which, threshold, thres.type, remove, info) {
  
  dataFiles <- data
  
  # workdir <- '/Users/roaldarbol/Documents/nivenlab/drosophila/Data/15-03-20_exp6_part1_WT_CyD_min'
  # setwd(workdir)
  # dataFiles <- lapply(Sys.glob(sprintf("*.csv")), read.csv)
  
  
  if (class(dataFiles) != 'list'){
    dataFiles <- list(dataFiles)
  }

  for (i in 1:length(dataFiles)){
    dataFiles[[i]] <- dataFiles[[i]] %>%
      mutate(Time = row_number()/fps,
             minute = i) 
    dataFiles[[i]]$yvar <- dataFiles[[i]][[y]]
    dataFiles[[i]]$xvar <- dataFiles[[i]][[x]]
    dataFiles[[i]] <- na.exclude(dataFiles[[i]])
  }
  
  # Raw summary - used in inverting
  raw_summary <- list()
  for (i in 1:length(dataFiles)){
    raw_summary[[i]] <- tibble()  
    raw_summary[[i]] <- dataFiles[[i]] %>%
      summarise(value.max = max(yvar))
  }
  
  # Inverting
  if (invert == TRUE){
    for (i in as.numeric(which)){
      dataFiles[[i]]$yvar <- -dataFiles[[i]]$yvar + abs(raw_summary[[i]]$value.max)
    }
  }
  
  # Thresholding prereq.
  for (i in 1:length(dataFiles)){
    raw_summary[[i]] <- tibble()  
    raw_summary[[i]] <- dataFiles[[i]] %>%
      summarise(value.max = max(yvar),
                value.min = min(yvar),
                value.mean = mean(yvar))
  }
  
  # Data smoothing
  df.Smooth <- smooth_it(dataFiles, 'xvar', 'yvar', smooth)
  
  # 4.Find spike timing and rate ----
  peaks <- list()
  troughs <- list()
  spike_timing <- list()
  spike_summary.list <- list()
  spike_summary <- tibble()
  
  for (i in 1:length(dataFiles)){
    peaks[[i]] <- tibble()
    peaks[[i]] <- localMaxima(df.Smooth[[i]]$yvar)
    troughs[[i]] <- tibble()
    troughs[[i]] <- localMaxima(-df.Smooth[[i]]$yvar)
    spike_timing[[i]] <- tibble(.rows = length(peaks[[i]]))
    spike_timing[[i]]$peak_time <- df.Smooth[[i]]$xvar[peaks[[i]]]
    spike_timing[[i]]$peak <- df.Smooth[[i]]$yvar[peaks[[i]]]
    spike_timing[[i]]$trough_time_before <- NA
    spike_timing[[i]]$trough_time_after <- NA
    
    # Long piece for finding the troughs on either side of a peak
    for (j in 1:length(peaks[[i]])){
      if (!is_empty(troughs[[i]][troughs[[i]]<peaks[[i]][j]])){
        l.sel <- troughs[[i]][troughs[[i]]<peaks[[i]][j]]
        l <- closest(l.sel, peaks[[i]][j])
        spike_timing[[i]]$trough_time_before[j] <- df.Smooth[[i]]$xvar[l]
      } else {
        spike_timing[[i]]$trough_time_before[j] <- NA
      }
      
      if (!is_empty(troughs[[i]][troughs[[i]]>peaks[[i]][j]])){
        u.sel <- troughs[[i]][troughs[[i]]>peaks[[i]][j]]
        u <- closest(u.sel, peaks[[i]][j])
        spike_timing[[i]]$trough_time_after[j] <- df.Smooth[[i]]$xvar[u]
      } else {
        spike_timing[[i]]$trough_time_after[j] <- NA
      }
    }
    
    if (thres.type == "Mean"){
      spike_timing[[i]] <- spike_timing[[i]] %>%
        filter(spike_timing[[i]]$peak > (raw_summary[[i]]$value.mean + threshold)) %>%
        mutate(spike_interval = peak_time - lag(peak_time),
               spike_rate = 1/spike_interval)
    } else if (thres.type == "Minimum"){
      spike_timing[[i]] <- spike_timing[[i]] %>%
        filter(spike_timing[[i]]$peak > (raw_summary[[i]]$value.min + threshold)) %>%
        mutate(spike_interval = peak_time - lag(peak_time),
               spike_rate = 1/spike_interval)
    }
    
    spike_timing[[i]] <- spike_timing[[i]] %>%
      mutate(half_time = trough_time_before + (peak_time - trough_time_before)/2)
    
    spike_timing[[i]]$half_height <- NA
    spike_timing[[i]]$half_width <- NA
    for (j in 1:nrow(spike_timing[[i]])){
      if (!is.na(spike_timing[[i]]$half_time[[j]])){
        suppressWarnings(spike_timing[[i]]$half_time[j] <- closest(df.Smooth[[i]]$xvar, spike_timing[[i]]$half_time[j]))
        spike_timing[[i]]$half_height[j] <- df.Smooth[[i]]$yvar[which(df.Smooth[[i]]$xvar == spike_timing[[i]]$half_time[j])]
        l <- which(df.Smooth[[i]]$xvar == spike_timing[[i]]$peak_time[j])
        u <- which(df.Smooth[[i]]$xvar == spike_timing[[i]]$trough_time_after[j])
        if (!is_empty(l) & !is_empty(u)){
          sel <- df.Smooth[[i]][l:u,]
          close <- sel$yvar[smaller(sel$yvar, spike_timing[[i]]$half_height[[j]])]
          half_after_time <- sel$xvar[sel$yvar == close]
          spike_timing[[i]]$half_width[j] <- (half_after_time - spike_timing[[i]]$half_time[[j]])
        } else {
          spike_timing[[i]]$half_width[j] <- NA
        }      
      } else {
        spike_timing[[i]]$half_height[j] <- NA
        spike_timing[[i]]$half_width[j] <- NA
      }
    }
    
    get_rid_of <- c('half_time', 'half_height')
    spike_timing[[i]] <- spike_timing[[i]] %>%
      select(!all_of(get_rid_of)) %>%
      mutate(minute = i)
  }
  
  # Remove false peaks
  need.remove <- list()
  j <- as.numeric(unlist(regmatches(remove, gregexpr("(?>-)*[[:digit:]]+\\.*[[:digit:]]*", remove, perl=TRUE))))
  j <- unique(unlist(lapply(j, function(x) as.integer(x))))
  for (i in j){
    # Modified solution from https://stackoverflow.com/questions/19252663/extracting-decimal-numbers-from-a-string
    need.remove[[i]] <- unlist(regmatches(remove,gregexpr(paste0(i, "+\\.[[:digit:]]*"), remove)))
    vector.temp <- vector()
    for (j in 1:length(need.remove[[i]])){
      df.temp <- unlist(strsplit(need.remove[[i]][[j]], split='.', fixed=TRUE))
      vector.temp <- c(vector.temp, as.numeric(df.temp[2]))
    }
    need.remove[[i]] <- sort(vector.temp)
    spike_timing[[i]] <- spike_timing[[i]] %>%
      filter(!row_number() %in% need.remove[[i]])
  }
  
  # Spike summary list
  for (i in 1:length(dataFiles)){    
    spike_summary.list[[i]] <- spike_timing[[i]] %>%
      summarise(SpikeRate_mean = mean(spike_rate, na.rm = TRUE),
                SpikeRate_SD = sd(spike_rate, na.rm = TRUE),
                HalfWidth_mean = mean(half_width, na.rm = TRUE),
                HalfWidth_SD = sd(half_width, na.rm = TRUE)
      ) %>%
      mutate(minute = i)
    spike_summary <- bind_rows(spike_summary, spike_summary.list[[i]])
  }
  
  for (i in 1:length(dataFiles)){
    dataFiles[[i]] <- dataFiles[[i]] %>%
      mutate(peak = if_else(xvar %in% spike_timing[[i]]$peak_time, 
                            xvar, 
                            0))
  }
  
  # Prepare a smooth file for plotting
  dataSmooth <- list()
  for (i in 1:length(dataFiles)){
    dataSmooth[[i]] <- dataFiles[[i]] %>%
      mutate(xvar = df.Smooth[[i]]$xvar,
             yvar = df.Smooth[[i]]$yvar)
  }
  
  # Make spike timing file
  spike_timing_all <- data.frame()
  spike_timing_all <- bind_rows(spike_timing_all, spike_timing)
  
  # Make metadata file
  if (is.null(dataFiles[[1]]$BX)){
    metadata <- data.frame()
  } else {
    metadata <- data.frame()
    for (i in 1:length(dataFiles)){
      current.metadata <- tibble()
      current.metadata <- dataFiles[[i]] %>%
        select(BX, BY, Width, Height) %>%
        slice(1) %>%
        mutate(minute = i,
               smooth = smooth,
               threshold = threshold,
               inverted = if_else(i %in% invert, "Yes", "No", missing="No"),
               # removed = if_else(length(need.remove[[i]]) > 0, paste(need.remove[[i]], sep = "", collapse = ", "), "none")
        )
    metadata <- bind_rows(metadata, current.metadata)
    }
  }
  
  output <- list(dataFiles, dataSmooth, spike_summary, spike_timing_all, metadata)
  
  if (info != ''){
    # Add experimental info to all data sets ----
    descript <- unlist(strsplit(info, ',')) # First extract individual strings from expression
    
    # dataFiles
    for (i in 1:length(dataFiles)){
      for (j in 1:length(descript)){
        name <- paste0('info', j)
        dataFiles[[i]] <- dataFiles[[i]] %>%
          mutate({{name}} := descript[[j]]) # Double curly braces from {{rlang}} paired with := for looping variable names
      }
    }
    
    # dataSmooth
    for (i in 1:length(dataSmooth)){
      for (j in 1:length(descript)){
        name <- paste0('info', j)
        dataSmooth[[i]] <- dataSmooth[[i]] %>%
          mutate({{name}} := descript[[j]]) # Double curly braces from {{rlang}} paired with := for looping variable names
      }
    }
    
    # Remaining data.frames
    for (i in 3:length(output)){
      for (j in 1:length(descript)){
        name <- paste0('info', j)
        output[[i]] <- output[[i]] %>%
          mutate({{name}} := descript[[j]]) # Double curly braces from {{rlang}} paired with := for looping variable names
      }
    }
  }

  
  return(output)
}