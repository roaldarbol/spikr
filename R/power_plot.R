power_plot <- function(data, xvar, yvar, fps){
  
  spectr <- data.frame()
  for (i in 1:length(data)){
    x <- data[[i]]$yvar
    del <- 1/fps # sampling interval
    x.spec <- spectrum(as.ts(x),log="no",span=10,plot=FALSE)
    spx <- x.spec$freq/del
    spy <- 2*x.spec$spec
    spectr.temp <- data.frame(spx, spy)
    spectr.temp <- spectr.temp %>%
      mutate(trial = as.factor(i))
    spectr <- rbind(spectr, spectr.temp)
  }
  
  power_plot <- ggplot(data = spectr, aes(x=spx, y=spy, group = trial, color = trial)) +
    geom_smooth(span = 0.2) +
    scale_x_continuous(limits = c(0,4)) +
    scale_colour_viridis_d() +
    theme_minimal()
  
  return(power_plot)
}
