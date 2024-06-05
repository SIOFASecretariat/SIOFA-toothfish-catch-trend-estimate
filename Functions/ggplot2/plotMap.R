# R script ----------------------------------------------------------------
#
# Author : Félix Massiot-Granier felix.massiot-granier@mnhn.fr , Jules Selles jules.selles@mnhn.fr
# Last update : 20-10-2020
#
# Script - fonction de cartographie pour les TAAF - adaptée pour St Paul et Amsterdam 
#                                          

PlotMap = function( 
  data_source, 
  data_type,  
  fun.user=mean,
  Species = "",
  Gear = "",
  Unit = "",
  Season = "",  
  Zone = "All",
  Res = c(0.025,0.025),
  Log =  FALSE,
  Data = TRUE,
  Map = TRUE,
  Date = TRUE,
  SousSecteur = FALSE,
  UTF = FALSE,
  id,
  pw
                    
  # Info --------------------------------------------------------------------
  #  Compute maps of catch, effort of St Paul and Amsterdam
                  
  # Input -------------------------------------------------------------------
  # data_source: Either a view/ table from Pecheker tibble (x,y,z) containing the maps info
  # Species: Species name : factor level used
  # Season: Season (DPMA, CALENDAIRE, CCAMLR,...) : factor level used 
  # Zone: "Amsterdam" , "Saint Paul" or "All"
  # fun.user: Custom function for aggregating data : mean, sum, sd ...
  # data_type: Variable to map
  # Gear: Gear name : factor level used  
  # Unit: Varibale unit : factor level used
  # Res: Two values vector for resolution c(res.x,res.y)
  # Log: Map in log scale TRUE/FALSE
  # Date: Print the date of plot or not TRUE/FALSE
  # Sous Secteur: Print the sector number TRUE/FALSE
  # UTF : Print Underwater Topographic Features (UTF)
  # Data: Si data doit être mis en forme ou importé: TRUE si déja formaté: FALSE
                    
  # Output ------------------------------------------------------------------
  # Return a map
                    
){
  
   if(Map ==TRUE){
  if(Data==TRUE){
    # Import the data ---------------------------------------------------------
    # (1. create a connexion to base if needed, 2 Import it from base if needed, 3. If it already exists, change its names to data_import)
     if (is.character(data_source)) {
       if (!exists("db_Pecheker")) {
         db_Pecheker <- Connection.Pecheker(id = id , pw = pw )
       }
       dat_import <-
         dbGetQuery(db_Pecheker,
                    paste("SELECT * FROM PECHEKER.", data_source, sep = ""))
     } else {
      dat_import <- data_source
    }
    dat<-dat_import
    
    
    # Create a reference raster -----------------------------------------------
    r <- raster(
      xmn = 75,
      xmx = 80,
      ymn = -40,
      ymx = -35,
      res = Res
    )
    raster::values(r) <- 1:(dim(r)[1] * dim(r)[2])
    r_dt <- as.data.frame(r, xy = T)
    proj4string(r) <-
      CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    
    
    # Selectionne la colonne d'interêt data_type
    dat <- dat %>% dplyr::select(LAT, LON, all_of(data_type)) %>% 
      dplyr::rename(data.type := all_of(data_type)) %>%
      mutate(data.type=as.numeric(as.character(data.type))) %>%
      filter(!is.na(LAT) & !is.na(LON))
    
    
    dat_spatial <- dat
    sp::coordinates(dat_spatial) <- ~ LON + LAT
    proj4string(dat_spatial) <-
      CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    dat$layer <- raster::extract(r, dat_spatial)
    
  
    # Applique la fonction désirée : mean(), sum() ....
    data_plot <- dat %>% group_by(layer) %>% 
        dplyr::summarize(FUN = fun.user(data.type)) %>%
        left_join(r_dt, by = c("layer")) %>%
        mutate(layer = as.factor(layer)) %>%
        filter(FUN> quantile(FUN, probs=0.05)) #%>%

    
  }else{
    data_plot<-data_source
  }
  
  
  # Importation des données de carto ----------------------------------------
  if(!exists("RNN_AMP_sf")) {
    load("Data/Spatial_data_frames/RNN_AMP_sf.rda") # RNN_AMP_sf : RNN polygon sf
  }
   
  if(!exists("FR_island_sf")) {
    load("Data/Spatial_data_frames/FR_island_sf.rda") # FR_island_sf : Fr islands polygons sf
    load("Data/Spatial_data_frames/EEZ1_sf.rda") # EEZ_sf : EEZ polygons sf
  }
   
  if(!exists("Isobath_SPA_large_sf")){
    load("Data/Spatial_data_frames/Isobath_SPA_large_sf.rda") # Isobath_SPA_large_sf : Gebco isobath sf
  }
   
  if(!exists("seamount_base_sf")){
    #load(file='Data/Spatial_data_frames/UTF_SPA_banc.sf.RData') # UTF_SPA_banc.sf : sf Yesson seammount bases
    load(file='Data/Spatial_data_frames/seamount_base_sf_gebco.Rdata') # seamount_base_sf : sf Harris seamount bases computing from gebco data 
    load(file='Data/Spatial_data_frames/seamount_peak_sf_gebco.Rdata') # seamount_peak_sf : sf Harris seamount peaks computing from gebco data 
  }
   
  if(Map==TRUE){  
    
    if (Zone =='Saint Paul'){ 
      lim_x=c(77.2,77.8); lim_y=c(-39, -38.5)
    }else if(Zone=='Amsterdam'){ 
      lim_x=c(77.3,77.8); lim_y=c(-38, -37.7)
    }else{
      lim_x = c(76.5,79); lim_y=c(-39.3,-36.5)
    }
    
      
  # Carte  ---------------------------------------------------------
  map<-ggplot() + xlim(lim_x[1],lim_x[2]) + ylim(lim_y[1],lim_y[2]) + 
                      xlab("Longitude") + ylab("Latitude") 
        
    
  # UTF 
  if (UTF){
    # Add UTF 
    map <- 
      map + 
      geom_sf(data = seamount_base_sf ,
              fill = "transparent",
              inherit.aes = F,
              size=0.3
      ) +
      
      geom_text_repel(
        data=as.data.frame(do.call(rbind, st_geometry(seamount_peak_sf)) %>% 
                             as_tibble() %>% setNames(c("lon","lat"))%>%
                             mutate(local_name=as.character(seamount_peak_sf$seamount_name))),
        box.padding = 2.5, seed=5, force =0.75,
        size=1.5, segment.size = 0.2,
        aes(x=lon,y=lat, label=local_name)
      ) 
    
  }
  
  # Sous Secteur
  if (SousSecteur) {
    map <-
      map + 
      geom_sf(
        data = EEZ_stat_grid_sf,
        fill = "transparent",
        col = "grey60",
        size = 0.5
      ) +
      
      geom_text(
        data = EEZ_stat_grid_sf,
        aes(XMIN+0.1, YMIN+0.05, label = SOUS_SECTE),
        inherit.aes = F,
        size = 1.5,
        col = "grey30"
      )
  }
  
  # Data
  map <- map + 
    geom_tile(data=data_plot, aes(x, y, fill = FUN))
  
  if(Log){
    map<-map+scale_fill_viridis(begin=0,end=1,direction = -1,trans="log")
    #map<-map+scale_fill_distiller(palette="Spectral",trans="log") 
  }else{
    map<-map+scale_fill_viridis(begin=0.3,end=1,direction = -1)
  }  
  
  # Other features
  map <- map +
        
      geom_sf(
        data = RNN_AMP_sf %>% dplyr::filter(district %in% c("SPA")),
        colour = "#b2182b",
        inherit.aes = F,
        size = 0.5,
        fill = "transparent"
        ) +
        
        # Add Gebco bathymetry
        geom_sf(
          data = Isobath_SPA_large_sf  %>% dplyr::filter(level %in% c("-500")),
          colour = "grey80",
          inherit.aes = F,
          size = 0.3
        ) +
        
        geom_sf(
          data = Isobath_SPA_large_sf  %>% dplyr::filter(level %in% c("-1000")),
          colour = "grey60",
          inherit.aes = F,
          size = 0.3
        ) +
        
        geom_sf(
          data = Isobath_SPA_large_sf  %>% dplyr::filter(level %in% c("-1500")),
          colour = "grey30",
          inherit.aes = F,
          size = 0.3
        ) +
        
        geom_sf(
          data = Isobath_SPA_large_sf  %>% dplyr::filter(level %in% c("-2000")),
          colour = "black",
          inherit.aes = F,
          size = 0.3
        ) +
        
        # Add EEZ polygon
        geom_sf(
          data = EEZ_sf %>% dplyr::filter(ID==92),
          fill = "transparent",
          inherit.aes = F,
          size = 0.5
        ) +
        
        # Add islands polygons 
        geom_sf(
          data = FR_island_sf ,
          fill = "darkgrey",
          inherit.aes = F
        ) +
        
       # Orientation
       annotation_north_arrow(
          location = "tl",
          which_north = "true",
          pad_x = unit(0.1, "in"),
          pad_y = unit(0.1, "in"),
          style = north_arrow_fancy_orienteering
        ) +
      
      # Labs
       labs(
          title = paste(data_type, Gear, Species, sep=" "),
          subtitle = Season,
          fill = Unit
        )
      
      
      
      # Source & date
      if (Date) {
        map <-
          map + labs(caption = paste("Source: MNHN", format(Sys.Date(), "%d/%m/%Y")))
       }
      
    }
    

    
    return(map)}
  else{
     return(data_plot)
  }

}



