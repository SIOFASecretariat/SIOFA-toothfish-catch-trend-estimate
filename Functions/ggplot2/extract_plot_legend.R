extract_plot_legend <- function(plot, Save = FALSE){
  
  g <- ggplotGrob(plot + guides(shape = FALSE,
                                      size = guide_legend(order = 2),
                                      fill = guide_legend(name = 'CV', order = 3),
                                      byrow = TRUE))$grobs
  
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  
  if(Save){
    pdf(paste0(path_figure,'/legend_FPD_map.pdf'), width=5 ,height=5, paper='special')
    grid.draw(legend)
    dev.off()
  }
  
  return(legend)
  
}
