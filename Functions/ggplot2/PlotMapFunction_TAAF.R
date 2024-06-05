#' Make a map of the SPA area
#'
#' Make a map of the SPA area with associated spatial layers
#' @param data_source Either a view/ table from Pecheker tibble (x,y,z) containing the maps info
#' @param data_type Variable name to map.
#' @param Fun.user Custom function for aggregating data : mean, sum, sd ... Default: mean
#' @param Species  Species name : factor level used. Default: ""
#' @param Gear Gear name : factor level used. Default: ""
#' @param Unit Varibale unit : factor level used. Default: ""
#' @param Season Season (DPMA, CALENDAIRE, CCAMLR,...) : factor level used. Default: ""
#' @param Zone "Amsterdam", "Saint Paul" or "All". Default : "All"
#' @param Res Two values vector for resolution c(res.x,res.y). Defaut : c(0.025,0.025)
#' @param Log Map in log scale. Default: FALSE
#' @param Data the isobath that you want to display on the map. Defaults are '-3000', '-2000', '-1500', '-1000', '-400'
#' @param Map the isobath that you want to display on the map. Defaults are '-3000', '-2000', '-1500', '-1000', '-400'
#' @param Date Print the date of plot or not. Default: TRUE
#' @param SousSecteur  Print the sector number. Default: FALSE
#' @param UTF Print Underwater Topographic Features (UTF). Default: FALSE
#' @param id ID PECHEKER connection. 
#' @param pw PW PECHEKER connection.
#' @return A Map or data formatted x,y,z 
#' 
#' @author Felix Massiot-Granier, Jules Selles
#' @export


PlotMap_TAAF = function(
  data_source=NULL,
  data_type=NULL,
  Fun.user=mean,
  facet=NULL,
  Species = "",
  Gear = "",
  Unit = "",
  Season = "",
  Zone = "DelCano",
  Res = c(0.025,0.025),
  Log =  FALSE,
  Data = FALSE,
  Map = TRUE,
  Date = TRUE,
  SousSecteur = FALSE,
  UTF = FALSE,
  ID = NA,
  PW = NA
  
  
){

  #Load required package, and install it if not
  list.of.packages <- c("ggspatial","tidyverse","lubridate","ggplot2",  "viridis", "rasterVis", "raster",
                        "rJava", "RJDBC",
                        "dplyr","readr", "tidyr", "stringr",  "sf","rgdal", "ggloop",
                        "MAPPOEPA"
                        )
  
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages, type = 'binary')
  lapply(list.of.packages, require, character.only = TRUE)
  
  if (!"MAPPOEPA" %in% (.packages())) {
    install.packages('../Scripts/MAPPOEPA_0.1.0.tar.gz', repos=NULL, type='source') 
    require(MAPPOEPA)
  }
  
  load("Data/Data_frames/sf_spatial_objects_Roberto.Rdata")
  
  
  
  if(Map ==TRUE){
    if(Data==TRUE){
      # Import the data ---------------------------------------------------------
      # (1. create a connexion to base if needed, 2 Import it from base if needed, 3. If it already exists, change its names to data_import)
      if (is.character(data_source)) {
        db_Pecheker <- Connection.Pecheker(id = ID , pw = PW )
       
        dat_import <-
          dbGetQuery(db_Pecheker,
                          paste0("SELECT * FROM PECHEKER.", data_source, sep = ""))
        
        dbDisconnect(db_Pecheker)
        
      } else {
        dat_import <- data_source
      }
      
      dat<-dat_import
      
      
      # Create a reference raster -----------------------------------------------
      r <- raster::raster(
        xmn = 40,
        xmx = 80,
        ymn = -55,
        ymx = -35,
        res = Res
      )
      raster::values(r) <- 1:(dim(r)[1] * dim(r)[2])
      r_dt <- raster::as.data.frame(r, xy = T)
      sp::proj4string(r) <-
        sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      
      
      # Selectionne la colonne d'interêt data_type
      dat <- dat %>% dplyr::select(LAT, LON, all_of(facet), all_of(data_type)) %>%
        dplyr::rename(data.type := all_of(data_type)) %>%
        dplyr::mutate(data.type=as.numeric(as.character(data.type))) %>%
        dplyr::filter(!is.na(LAT) & !is.na(LON))
      
      
      dat_spatial <- dat
      sp::coordinates(dat_spatial) <- ~ LON + LAT
      sp::proj4string(dat_spatial) <-
        sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      dat$layer <- raster::extract(r, dat_spatial)
      
      
      # Applique la fonction désirée : mean(), sum() ....
      if(is.null(facet)){
        data_plot <- dat %>% dplyr::group_by(layer) %>%
          dplyr::summarize(FUN = Fun.user(data.type)) %>%
          dplyr::left_join(r_dt, by = c("layer")) %>%
          dplyr::mutate(layer = as.factor(layer))# %>%
        #dplyr::filter(FUN> stats::quantile(dat$FUN, probs=0, na.rm=T)) #%>%
      }else{
        data_plot <- dat %>% dplyr::group_by(layer, across(all_of(facet)) ) %>%
          dplyr::summarize(FUN = Fun.user(data.type)) %>%
          dplyr::left_join(r_dt, by = c("layer")) %>%
          dplyr::mutate(layer = as.factor(layer))# %>%
          #dplyr::filter(FUN> stats::quantile(dat$FUN, probs=0, na.rm=T)) #%>%
      }
      
    }
    
    if(Map==TRUE){
      
      if (Zone =='Saint Paul'){
        lim_x=c(77.2,77.8); lim_y=c(-39.1, -38.5)
        districtTAAF ='SPA'
        isobath=Isobath_SPA_large_sf
      }else if(Zone=='Amsterdam'){
        lim_x=c(77.3,77.8); lim_y=c(-38, -37.7)
        districtTAAF ='SPA'
        isobath=Isobath_SPA_large_sf
      }else if (Zone =='bancdes16milles'){
          lim_x=c(77.6,77.9); lim_y=c(-39.1, -38.9)
          districtTAAF ='SPA'
          isobath=Isobath_SPA_large_sf
        }else if(Zone=='Amsterdam'){
      }else if(Zone=='Crozet'){
        lim_x=c(45.3, 53.1); lim_y=c(-47.1,-44.4)
        districtTAAF ='CRO'
        isobath=Isobath_CRO_sf
      }else if(Zone=='Kerguelen'){
        lim_x=c(63.6,75.9); lim_y=c(-52.4,-45.3)
        districtTAAF ='KER'
        isobath=Isobath_KER_sf
      }else if(Zone=='DelCano'){
        lim_x=c(42,47); lim_y=c(-45.5,-42.0)
        districtTAAF ='CRO'
        isobath=Isobath_IO_felix_sf
      }else if(Zone=='SPA'){
        lim_x = c(76.5,79); lim_y=c(-39.3,-36.5)
        districtTAAF ='SPA'
        isobath=Isobath_SPA_large_sf
      }else{
      lim_x = c(73,82); lim_y=c(-42,-34.5)
      districtTAAF ='SPA'
      isobath=Isobath_SPA_large_sf
      }
      
      
      # Carte  ---------------------------------------------------------
      map<- ggplot2::ggplot() + ggplot2::xlim(lim_x[1],lim_x[2]) + ggplot2::ylim(lim_y[1],lim_y[2]) +
        ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude")
      
      # UTF
      if (UTF){
        # Add UTF
        seamount_peak_SPA_sf = seamount_peak_SPA_sf[which(seamount_peak_SPA_sf$seamount_name %in% seamount_base_SPA_sf$seamount_name), ]
        seamount_peak_SPA_sf$seamount_name[which(seamount_peak_SPA_sf$seamount_name %in% c('Nordet.Amsterdam.2', 'Tmp4', 'Tmp5') )] = c('Nordet.Amsterdam', '', 'Banc.Sud')
        map <-
          map +
          ggplot2::geom_sf(data = seamount_base_SPA_sf ,
                           inherit.aes = F,
                           size=0.3
          ) +
          
          ggrepel::geom_text_repel(
            data=as.data.frame(do.call(rbind, 
                                       sf::st_geometry(seamount_peak_SPA_sf)) %>%
                                 dplyr::as_tibble() %>% setNames(c("lon","lat"))%>%
                                 mutate(local_name=as.character(seamount_peak_SPA_sf$seamount_name))),
            box.padding = 2.5, seed=5, force =0.75,
            size=1.5, segment.size = 0.2,
            ggplot2::aes(x=lon,y=lat, label=local_name)
          )
        
      }
      
      # Sous Secteur
      if (SousSecteur) {
        map <-
          map +
          ggplot2::geom_sf(
            data = EEZ_stat_grid_sf,
            fill = "transparent",
            col = "grey60",
            size = 0.5
          ) +
          
          ggplot2::geom_text(
            data = EEZ_stat_grid_sf,
            ggplot2::aes(XMIN+0.1, YMIN+0.05, label = SOUS_SECTE),
            inherit.aes = F,
            size = 1.5,
            col = "grey30"
          )
      }
      
      # Data
      if (Data==TRUE){ 
      map <- map +
        ggplot2::geom_tile(data=data_plot, ggplot2::aes(x, y, fill = FUN))
      
        if(Log){
          map<-map + viridis::scale_fill_viridis(begin=0,end=1,direction = -1,trans="log")
          #map<-map+scale_fill_distiller(palette="Spectral",trans="log")
        }else{
          map<-map + viridis::scale_fill_viridis(begin=0.3,end=1,direction = -1)
        }
      
      # Labs 
      # Labs
      map <- map +
        ggplot2::labs(
          title = paste(data_type, Gear, Species, sep=" "),
          subtitle = Season,
          fill = Unit
        ) 
        
      }
      
      # Other features
      map <- map +
        
        # ggplot2::geom_sf(
        #   data = RNN_AMP_sf %>% dplyr::filter(district %in% c(districtTAAF)),
        #   colour = "#b2182b",
        #   inherit.aes = F,
        #   size = 0.5,
        #   fill = "transparent"
        # ) +
        
        # Add Gebco bathymetry
        ggplot2::geom_sf(
          data = isobath  %>% dplyr::filter(level %in% c("-50")),
          colour = "grey80",
          inherit.aes = F,
          size = 0.3
        ) + 
        ggplot2::geom_sf(
          data = isobath  %>% dplyr::filter(level %in% c("-200")),
          colour = "grey70",
          inherit.aes = F,
          size = 0.3
        ) +
        
        ggplot2::geom_sf(
          data = isobath  %>% dplyr::filter(level %in% c("-500")),
          colour = "grey60",
          inherit.aes = F,
          size = 0.3
        ) +
        
        ggplot2::geom_sf(
          data = isobath  %>% dplyr::filter(level %in% c("-1000")),
          colour = "grey50",
          inherit.aes = F,
          size = 0.3
        ) +
        
        ggplot2::geom_sf(
          data = isobath  %>% dplyr::filter(level %in% c("-1500")),
          colour = "grey30",
          inherit.aes = F,
          size = 0.3
        ) +
        
        ggplot2::geom_sf(
          data = isobath  %>% dplyr::filter(level %in% c("-2000")),
          colour = "grey10",
          inherit.aes = F,
          size = 0.3
        ) +
        ggplot2::geom_sf(
          data = isobath  %>% dplyr::filter(level %in% c("-3000")),
          colour = "black",
          inherit.aes = F,
          size = 0.3
        ) +
        
        
        # Add EEZ polygon
        ggplot2::geom_sf(
          data = EEZ_sf,
          fill = "transparent",
          inherit.aes = F,
          size = 0.5
        ) +
        
        # Add islands polygons
        ggplot2::geom_sf(
          data = FR_island_sf ,
          fill = "darkgrey",
          inherit.aes = F
        ) +
        
        # Orientation
        ggspatial::annotation_north_arrow(
          location = "tl",
          which_north = "true",
          pad_x = ggplot2::unit(0.1, "in"),
          pad_y = ggplot2::unit(0.1, "in"),
          style = ggspatial::north_arrow_fancy_orienteering
        ) +
        
        ggplot2::theme_light() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                       plot.margin = ggplot2::margin(0, 0.5, 0, 0, "cm"))
      
      
      
      # Source & date
      if (Date) {
        map <-
          map +
          ggplot2::labs(caption = paste("Source: MNHN", format(Sys.Date(), "%d/%m/%Y")))
      }
      
      
      # Facet
      if (!is.null(facet)) {
        map <-
          map +
         facet_wrap(reformulate(facet), ncol=2)
      }
      
    }
    
    
    
    return(map)
    }else{
    return(data_plot)
  }
  
}



