save_plot <- function(plot,
                      file_name = NULL,
                      path,
                      output = '',
                      scale = 1,
                      width = NULL,
                      height = NULL,
                      device = 'pdf',
                      Save = SavePlots,
                      EraseCoda = T){
  
  if(is.null(file_name)){
    file_name <- deparse(substitute(plot))
  }
  
  if(SavePlots){
    
    if(file.exists(paste0(path, file_name)) & EraseCoda == FALSE){
      warning('plot already saved')
    }else{
      
      if(output == 'Beamer'){
        if(is.null(width)){
          width <- 12  
        }
        
        if(is.null(height)){
          height <- 8
        }
        
        units <- 'cm'
        scale <- 1.5
        
        ggsave(plot = plot,
               filename = paste0(file_name, '.' ,device),
               device = device,
               path = path,
               scale = scale,
               dpi = 600,
               width = width,
               height = height,
               units = units,
               bg = "transparent")
      }
      
      if(output == 'Beamer/2'){
        width <- 10.8/2
        height <- 8.38/2
        units <- 'cm'
        scale <- 2
        
        ggsave(plot = plot,
               filename = paste0(file_name, '.' ,device),
               device = device,
               path = path,
               scale = scale,
               dpi = 600,
               width = width,
               height = height,
               units = units,
               bg = "transparent")
      }
      
      
      if(output == ''){
       
        scale <- 1.5
        units <- 'cm'
        if(is.null(width)){
          width <- 12  
        }
        
        if(is.null(height)){
          height <- 8
        }
        
        ggsave(plot = plot,
               filename = paste0(file_name, '.' ,device),
               device = device,
               path = path,
               scale = scale,
               width = width,
               height = height,
               units = units,
               dpi = 200,
               bg = "transparent")
        
        
      }
    }
  }
}
