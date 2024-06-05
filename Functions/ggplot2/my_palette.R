require(RColorBrewer)

# Palette1 <- colorRampPalette(c(colors()[142], colors()[77], colors()[54], 
#                               colors()[505], colors()[554], colors()[153]))

wrb_palette <- colorRampPalette(c("cadetblue", colors()[554], colors()[153]))



cb_palette <- c("#E69F00", "#56B4E9",  "#D55E00", # Gold, Light blue, Red
                "#009E73", "#CC79A7", # Green, Violet
                "#0072B2", # dark blue
                "#F0E442", "#999999") # yellow, grey

cb_palette_black <- c("#E69F00", "#56B4E9",  "#D55E00", # Gold, Light blue, Red
                      "#009E73", "#CC79A7", # Green, Violet
                      "#0072B2", # dark blue
                      "#F0E442", "#999999", # yellow, grey
                      "#000000") # black

cb_map_palette_blue_red <- c('#a50026','#d73027','#f46d43', '#fdae61',
                             '#fee090', '#ffffbf','#e0f3f8', '#abd9e9',
                             '#74add1','#4575b4','#313695')


cb_map_palette_violet_green <- c('#40004b', '#762a83', '#9970ab', '#c2a5cf', 
                                 '#e7d4e8','#f7f7f7','#d9f0d3','#a6dba0',
                                 '#5aae61','#1b7837','#00441b')

