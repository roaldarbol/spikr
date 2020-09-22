spike_plot <- function(data, xvar, yvar, threshold){

  df.All <- tibble()
  for (i in 1:length(data)){
    df.All <- bind_rows(df.All, data[[i]])
  }
  
  df.All <- df.All %>%
    mutate(peak = if_else(peak == 0, NA_real_, peak))
  
  xmax <- max(df.All$xvar)
  ymax <- max(df.All$yvar)
  
    ggplot(df.All, aes(x=xvar, y = yvar)) +
      geom_line() +
      #geom_rug(aes(peak), sides = "b") +
      geom_vline(data = df.All, aes(xintercept = peak),
                 alpha = .5,
                 colour = "orange",
                 linetype = 2,
                 show.legend = FALSE) +
      scale_x_continuous(limits = c(0,xmax),
                         breaks = seq(0,xmax,1)
      ) +
      # scale_colour_distiller(palette = 'Spectral', guide = FALSE) +
      labs(x = "Time (s)",
           y = paste("Image", yvar, "/ Cardiac contraction"),
           title = "") +
      facet_wrap(~ minute, nrow = 1) + #, scales = "free_y") +
      theme(panel.background = element_blank(),
            panel.grid = element_blank(),
            strip.background = element_rect(fill="#18bc9c"),
            legend.key = element_rect(colour = "black"),
            legend.justification = c(1,0),
            legend.position = c(.27,.84),
            axis.line.x.bottom = element_line(colour = "black", size = .7),
            axis.line.y.left = element_line(colour = "black", size = .7),
            text = element_text(family = "serif", size = 10),
            aspect.ratio = 4/5
      ) -> plot
  return(plot)
}