################################################################################
### Function to cut continous variable by quantilles or manually
################################################################################

cut_continuous_variable <- function(x,
                                    bins = 5,
                                    Type = 'quantiles',
                                    brks = NULL,
                                    zero_category = TRUE,
                                    levels_label = NULL,
                                    round_cut_points = TRUE){
  
  if(!(Type == '10breaks' | Type == 'quantiles' | Type == 'binbreaks')){
    stop('Type should be \'10breaks\', \'quantiles\', or \'binbreaks\' ')
  }
  
  if(Type != '' &is.null(brks)){
    ### Type = quantiles or nothing
    if(Type == '10breaks') {
      cutpoints <- c(seq(from = 1, to = range(x, na.rm = TRUE)[2],
                         length.out = 10), Inf)
    }
    
    ### Cut by quantiles
    if(Type == 'quantiles') {
      cutpoints <- quantile(x, (0:bins)/bins, na.rm = TRUE)
    }
    
    ### Cut by breaks
    if(Type == 'binbreaks' & is.null(brks)) {
      cutpoints <- c(seq(from = range(x, na.rm = TRUE)[1],
                         to = range(x, na.rm = TRUE)[2],
                         length.out = bins), Inf)
    }
  }
  
  if(!is.null(brks)) {
    cutpoints <- c(brks, Inf)
  }
  
  ### Add -1 to be sure to plot the 0 as an entire category
  if(zero_category) {
    cutpoints <- c(-10, cutpoints)
  }
  
  ### Round to have no digits
  if(round_cut_points){
    cutpoints <- round(cutpoints, digits = -1)
  }
  ### Remove duplicated cut-points
  cutpoints <- unique(cutpoints)
  
  ### Cut
  binned <- cut(x, breaks = cutpoints, include.lowest = TRUE, right = TRUE)
  
  ### Add -1 to be sure to plot the 0 as an entire category
  if(zero_category) {
    levels(binned)[1] <- '0'
  }
  
  if(!is.null(levels_label) & zero_category){
    levels(binned)[2:nlevels(binned)] <- as.character(levels_label)
  }
  
  
  if(!is.null(levels_label) & zero_category == FALSE){
    levels(binned) <- as.character(levels_label)
  }
  
  return(binned)
}
