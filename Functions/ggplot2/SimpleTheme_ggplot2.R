#### Blank theme
theme_set(theme_light())

### Change black and white theme
custom_theme <-theme_update(
  
  ### Remove the square line around the plot
  panel.border = element_rect( fill = NA, colour = 'black'),
  ### Grid on the plot
  # #panel.grid.major = element_line( colour = "grey90", size = 0.2),
  # panel.grid.major = element_blank(),
  # panel.grid.minor = element_blank(),
   strip.background = element_rect( fill = NA, colour = "black"),
  
  ### Axis
  axis.line = element_line(),
  axis.line.x = element_line( colour = "black", size = 0.5, linetype = 1),
  axis.line.y = element_line( colour = "black", size = 0.5, linetype = 1),
  axis.text = element_text( size = rel(0.8)),
  axis.ticks = element_line( colour = "black", size = 0.5),
  
  ### Background
  panel.background = element_rect( fill = "transparent", colour = NA), 
  plot.background = element_rect( fill = "transparent", colour = NA),
  
  ### Legend
  legend.background = element_rect( colour = NA, fill = NA),  
  legend.key = element_rect( colour = NA, fill = NA),
  legend.key.size = unit( 1.1, 'line'),
  legend.title = element_text( size = rel(1), face = 'bold'),
  legend.text = element_text(size = rel(1)),
  
  ### Facet
  strip.text.x = element_text(size = 9, color='black'),
  strip.text.y = element_text(size = 9, color='black'),
 # strip.background = element_rect(colour = "black", fill = NA)
)

textwidth = 17
textheight = textwidth
