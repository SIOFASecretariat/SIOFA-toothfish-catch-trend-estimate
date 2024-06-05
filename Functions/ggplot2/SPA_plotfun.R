##### Function to get Amsterdam and St Paul ggplot from google map with graticules
## values : Sp_area & A_area -> ready to plot with ggmap 
#           grat_A_for & grat_SP_for -> ready to add on plot with geom_path

## !!! Obsolète 


SPA_plotfun <- function (A_coord = c(77.559, -37.832),           ## Amsterdam coordinates
                         SP_coord = c(77.518, -38.724),          ## Saint paul coordinates
                         prj = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 "),
                         path_eez ='Data/World_EEZ/', ## projection referentiel
                         path_aera = 'Data/Spatial_data_frames/',
                         color=F) {
  
  ## Laod SPA sf map :standard.spa.map.plot
  
  load(file='Data/Spatial_data_frames/standard.map.plot.RData')
  
  # Key API for ggmap
  key_api <- "AIzaSyDOmSpYSMCW3u2p980Sfp0nr0L3TrYnsjg"
  ggmap::register_google(key = key_api)
  
  ################################
  ### Coord by island
  ## Amsterdam
  
   A_coord_square_large <- c(77,-38.5, 79, -36.5)
   A_coord_square  <- c(77, -38.5, 78, -37.5)
   A_coord_square_centered <- c(77.45,  -37.95, 77.65,  -37.75)
   A_coord = list(standard=A_coord_square, centered=A_coord_square_centered, large=A_coord_square_large)
   
  ## Graticule 
   mer_A = seq(A_coord_square_large[1], A_coord_square_large[3], by=0.1)
   mer_A_centered = seq(A_coord_square_centered[1], A_coord_square_centered[3], by=0.05)## Vector of meridiens position at A
   par_A = seq(A_coord_square_large[2], A_coord_square_large[4],  by=0.1)
   par_A_centered = seq(A_coord_square_centered[2], A_coord_square_centered[4], by=0.05) ## Vector of parrallels position  at A
   
  ## retrieve map from google
  if (color == T){   A_area <- sapply(A_coord,function(x) get_map(location = x,  force=T,
                         source = 'google',maptype="satellite" )) 
  } else { A_area <- sapply(A_coord,function(x) get_map(location = x,  force=T,
                         source = 'google',maptype="satellite" , color = "bw")) 
  }
  
  ##-------------------
  ## StPaul
  SP_coord_square_large <- c(77, -40, 79, -38)
  SP_coord_16milesbank <- c(77.65, -39.02, 77.82, -38.85 )
  SP_coord_square_centered <- c(77.35, -38.9 , 77.7, -38.55 )
  SP_coord_square_centered_c <- c(77.48, -38.76 , 77.57, -38.68 )
  SP_coord_square_centered_cbis <- c(77.38, -38.82 , 77.6, -38.62 )
  
  
  SP_coord_square <- c(77.3, -39, 77.8, -38.5) 
  SP_coord = list(standard=SP_coord_square, centered=SP_coord_square_centered, large=SP_coord_square_large, 
                  bank=SP_coord_16milesbank, centered_c =SP_coord_square_centered_c, 
                  centered_cbis =SP_coord_square_centered_cbis)
  ## Graticule 
  mer_SP = seq(SP_coord_square_large[1], SP_coord_square_large[3], by=0.1)
  mer_SP_centered = seq(SP_coord_square_centered[1], SP_coord_square_centered[3], by=0.1)## Vector of meridiens position at A
  mer_SP_centered_c = seq(SP_coord_square_centered_c[1], SP_coord_square_centered_c[3], by=0.05)## Vector of meridiens position at A
  
  par_SP = seq(SP_coord_square_large[2], SP_coord_square_large[4],  by=0.1)
  par_SP_centered = seq(SP_coord_square_centered[2], SP_coord_square_centered[4], by=0.1) ## Vector of parrallels position  at A
  par_SP_centered_c = seq(SP_coord_square_centered_c[2], SP_coord_square_centered_c[4], by=0.05) ## Vector of parrallels position  at A
  
  ## retrieve map from google
  if (color == T){SP_area <- sapply(SP_coord, function(x) get_map(location = x,force=T,
                       source = 'google', maptype="satellite")) # retrieve from google map  
  } else {   SP_area <- sapply(SP_coord, function(x) get_map(location = x, force=T,
                     source = 'google', maptype="satellite", color = "bw")) # retrieve from google map
  }
  
  ##-------------------
  ### Graticules
  grat_SP <- graticule(mer_SP, par_SP, proj = prj, 
                       xlim = c(mer_SP[1], mer_SP[length(mer_SP)]),
                       ylim = c(par_SP[1],par_SP[length(par_SP)]))
  grat_SP_for <- fortify(grat_SP)
  grat_SP_centered <- graticule(mer_SP_centered, par_SP_centered , proj = prj, 
                               xlim = c(mer_SP_centered[1], mer_SP_centered[length(mer_SP_centered)]),
                               ylim = c(par_SP_centered[1],par_SP_centered[length(par_SP_centered)]))
  grat_SP_for_centered <- fortify(grat_SP_centered)
  
  grat_SP_centered_c <- graticule(mer_SP_centered_c, par_SP_centered_c , proj = prj, 
                                xlim = c(mer_SP_centered_c[1], mer_SP_centered[length(mer_SP_centered_c)]),
                                ylim = c(par_SP_centered_c[1],par_SP_centered[length(par_SP_centered_c)]))
  grat_SP_for_centered_c <- fortify(grat_SP_centered_c)
  
  grat_A <- graticule(mer_A, par_A, proj = prj, 
                      xlim = c(mer_A[1], mer_A[length(mer_A)]),
                      ylim = c(par_A[1],par_A[length(par_A)]))
  grat_A_for <- fortify(grat_A)
  
  grat_A_centered <- graticule(mer_A_centered, par_A_centered , proj = prj, 
                       xlim = c(mer_A_centered[1], mer_A_centered[length(mer_A_centered)]),
                       ylim = c(par_A_centered[1],par_A_centered[length(par_A_centered)]))
  grat_A_for_centered <- fortify(grat_A_centered)
  
  
  ### Management area 
  load(file = paste0(path_aera,'contour_coastal_A_pol.RData',sep=''))
  load(file = paste0(path_aera,'contour_pelagic_A_pol.RData',sep=''))
  load(file = paste0(path_aera,'contour_coastal_SP_pol.RData',sep=''))
  load(file = paste0(path_aera,'contour_pelagic_SP_pol.RData',sep=''))
  load(file = paste0(path_aera,'contour_banc_SP_pol.RData',sep=''))
  
  management_area = list(coastal_A = fortify(contour_coastal_A_pol), offshore_A = fortify(contour_pelagic_A_pol),
                         coastal_SP = fortify(contour_coastal_SP_pol), offshore_SP = fortify(contour_pelagic_SP_pol),
                         banc_SP = fortify(contour_banc_SP_pol))
  
  ### EEZ area 
  
  if(!file.exists(paste0(path_eez,'fr_eez.RData',sep=''))){
    eez <- readOGR(dsn = path_eez, layer = "eez_v10")
    fr_eez <- eez[which(eez$Sovereign1=='France'),]
    save(fr_eez, file=paste(path_eez, 'fr_eez.RData', sep=''))
  } else { load(file=paste(path_eez, 'fr_eez.RData', sep='')) }
  
  SPA_eez <- fr_eez[which(fr_eez$Territory1=='Amsterdam and Saint Paul Islands' ),]
  SPA_eez_pol <- fortify(SPA_eez)
  
  ### Island barycenter 
  
  SP_bar = c(77.518, -38.724)
  A_bar = c(77.559, -37.832)
    
  return(list(SP_bar=SP_bar, SP_area = SP_area$standard, SP_coord_square =SP_coord$standard,
              SP_area_centered = SP_area$centered, SP_coord_square_centered = SP_coord$centered, 
              SP_area_centered_c = SP_area$centered_c, SP_coord_square_centered_c = SP_coord$centered_c,
              SP_area_centered_cbis = SP_area$centered_cbis, SP_coord_square_centered_cbis = SP_coord$centered_cbis,
              SP_area_large = SP_area$large, SP_coord_square_large =SP_coord$large,
              SP_coord_bank = SP_coord$bank, SP_area_bank = SP_area$bank,
              A_bar=A_bar, A_area = A_area$standard, A_coord_square = A_coord$standard,
              A_area_centered = A_area$centered, A_coord_square_centered = A_coord$centered,
              A_area_large = A_area$large, A_coord_square_large = A_coord$large,
              grat_A_for = grat_A_for, grat_A_for_centered=grat_A_for_centered, 
              grat_SP_for = grat_SP_for, grat_SP_for_centered=grat_SP_for_centered,
              grat_SP_for_centered_c=grat_SP_for_centered_c,
              management_area = management_area,
              SPA_eez = SPA_eez_pol))
}
