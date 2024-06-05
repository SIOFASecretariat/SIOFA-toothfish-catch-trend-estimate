##### Get contour plot from interpolated bathymetry (fishing operation)

bathy_plotfun <- function(path='Data/Spatial_data_frames/') {
  
  ### load df bathy data
  load(file = paste(path,'df_bathy_SP.RData',sep='')) ## pol_bath_SP_fort_mer : format = dataframe
  load(file = paste(path,'df_bathy_SP_centered.RData',sep='')) ## pol_bath_SP_fort_mer_centered : format = dataframe
  
  load(file = paste(path,'df_bathy_A.RData',sep='')) ## pol_bath_A_fort_mer : format = dataframe
  load(file = paste(path,'df_bathy_A_centered.RData',sep='')) ## pol_bath_A_fort_mer_centered : format = dataframe
  
  ### Format data for countour plot 
  pol_bath_A_fort_mer_contour <- pol_bath_A_fort_mer %>% group_by(id) %>% 
              summarise(long = mean(long),
                        lat = mean(lat),
                        var1.pred = mean(z))
  
  pol_bath_A_fort_mer_contour_centered <- pol_bath_A_fort_mer_centered %>% group_by(id) %>% 
    summarise(long = mean(long),
              lat = mean(lat),
              var1.pred = mean(z))
  
 
  pol_bath_SP_fort_mer_contour <- pol_bath_SP_fort_mer %>% group_by(id) %>%
                       summarise(long = mean(long),
                                 lat = mean(lat),
                                 var1.pred = mean(z))
  
  pol_bath_SP_fort_mer_contour_centered <- pol_bath_SP_fort_mer_centered %>% group_by(id) %>%
    summarise(long = mean(long),
              lat = mean(lat),
              var1.pred = mean(z))
  
  return(list(pol_bath_SP_fort_mer_contour = pol_bath_SP_fort_mer_contour,
              pol_bath_SP_fort_mer_contour_centered = pol_bath_SP_fort_mer_contour_centered,
              pol_bath_A_fort_mer_contour = pol_bath_A_fort_mer_contour,
              pol_bath_A_fort_mer_contour_centered = pol_bath_A_fort_mer_contour_centered,
              pol_bath_A_fort_mer = pol_bath_A_fort_mer, 
              pol_bath_A_fort_mer_centered = pol_bath_A_fort_mer_centered, 
              pol_bath_SP_fort_mer = pol_bath_SP_fort_mer,
              pol_bath_SP_fort_mer_centered = pol_bath_SP_fort_mer_centered))
  }