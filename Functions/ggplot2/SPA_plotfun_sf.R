##### Script to get large scale Amsterdam and St Paul and seamounts standard ggplot map

### Bathymetry and bank area - exploration Croix du Sud Campaign ### 

### Get SHOP bathymetry
#raster('Data/Ifremer_SPA_MNT/SHOM_SPA_180820.grd')

### Get bathymetry GEBCO
Gebco_bat = readGEBCO.bathy(file='Data/Gebco_Bathymetry/gebco_2022_n-36.2219_s-40.5725_w76.0474_e79.3762.nc')
bath.gebco <- as.xyz(Gebco_bat); names(bath.gebco)= c('lon','lat','bathy')
bath.raster = rasterFromXYZ(bath.gebco)
bath.sf = st_as_sf(bath.gebco, coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84",
                   agr = "constant")

iso.bath.sf <- rasterToContour(bath.raster,levels=seq(-5000,0, by=50))
iso.bath.sf <- st_as_sf(iso.bath.sf)

## Identified bank coordinates
Ams <- c(77.4,-37.95, 77.7, -37.75)
Sp <- c(77.1,-38.85, 77.6, -38.55)

NordetStP.bank <- c(77.61,-38.65, 78, -38.4)
NordetAms.bank <- c(77.7,-37.8, 78.1, -37.55)
tmp4.bank <- c(77.70,-38.1, 77.80, -37.9)
tmp5.bank <- c(77.70,-38.35, 77.80, -38.15)
JeanLuc.bank <- c(77.5,-39.4, 77.8, -39.1)
StPierre.bank <-  c(77.6,-39.1, 77.85, -38.85 )

CapHorn.bank <- c(78.7,-36.8, 79, -36.6)
milles45.bank <- c(78.4,-38.7, 78.6, -38.5)

## Surface explored 
linf = -1000; lsup= -10
area.list = list(NordetStP.bank= NordetStP.bank, NordetAms.bank=NordetAms.bank, tmp4.bank=tmp4.bank,
                 tmp5.bank=tmp5.bank, JeanLuc.bank=JeanLuc.bank, StPierre.bank=StPierre.bank,
                 CapHorn.bank=CapHorn.bank, milles45.bank=milles45.bank,Amsterdam=Ams, StPaul=Sp)

area.surf <- lapply(area.list,  FUN= function(area){get.area(Gebco_bat, xlim=c(area[1],area[3]), 
                                                             ylim=c(area[2],area[4]),
                                                             level.inf=linf, level.sup=lsup)$Square.Km} )

#save(area.surf, file='Data/area.surf.RData')

## SPA map 
Ams.map <- maps::map(database='worldHires', xlim=c(73, 82),ylim=c(-38, -36), resolution=0.01, fill=T)
Ams.map <- fortify(Ams.map)
Sp.map <- maps::map(database='worldHires', xlim=c(73, 82),ylim=c(-42, -38), resolution=0.01)
Sp.map <- fortify(Sp.map)
map = as.data.frame(rbind(Ams.map,Sp.map)) 

##----------------------------------
#### Plot study area

### SPA map area
standard.spa.map.plot <- ggplot() +
  geom_polygon(data=map, aes(x=long, y=lat, group=subregion), fill='grey', col='black') +
  # geom_raster(data=bath.gebco,aes(x=lon,y=lat,fill=bathy)) +
  geom_sf(data= iso.bath.sf %>% dplyr::filter(level == c("-2000")), 
          inherit.aes=F,  colour = "grey80", size=0.5) +
  geom_sf(data= iso.bath.sf %>% dplyr::filter(level %in% c("-1500")), 
          inherit.aes=F,  colour = "grey70", size=0.5) +
  # geom_sf(data= iso.bath.sf %>% dplyr::filter(level %in% c("-1000")), 
  #         inherit.aes=F,  colour = "black", size=0.5)+
  geom_sf(data= iso.bath.sf %>% dplyr::filter(level %in% c("-400")), 
          inherit.aes=F,  colour = "grey50", size=0.5)+
  geom_sf(data= iso.bath.sf %>% dplyr::filter(level %in% c("-200")), 
          inherit.aes=F,  colour = "grey30", size=0.5)+
  geom_sf(data= iso.bath.sf %>% dplyr::filter(level %in% c("-50")), 
          inherit.aes=F,  colour = "grey20", size=0.5)+
 # coord_sf(xlim = c(73, 82), ylim = c(-43, -34), expand = FALSE) +
  theme_bw()

  
  # annotate(geom="rect", xmin=Sp[1], xmax=Sp[3], 
  #          ymin=Sp[2], ymax=Sp[4], 
  #          colour = "blue", fill = NA) +
  # annotate(geom="text", x=mean(Sp[1], Sp[3])-0.4, 
  #          y= mean(Sp[2], Sp[4])-0.1, 
  #          label = "St Paul", size=2, color='black') +
  # 
  # annotate(geom="rect", xmin=Ams[1], xmax=Ams[3], 
  #          ymin=Ams[2], ymax=Ams[4], 
  #          colour = "blue", fill = NA) +
  # annotate(geom="text", x=mean(Ams[1], Ams[3])-0.4, 
  #          y= mean(Ams[2], Ams[4])-0.1, 
  #          label = "Amsterdam", size=2, color='black') +
  # 
  # annotate(geom="rect", xmin=NordetStP.bank[1], xmax=NordetStP.bank[3], 
  #          ymin=NordetStP.bank[2], ymax=NordetStP.bank[4], 
  #          colour = "red", fill = NA) +
  # annotate(geom="text", x=mean(NordetStP.bank[1], NordetStP.bank[3])+0.4, 
  #          y= mean(NordetStP.bank[2], NordetStP.bank[4])-0.05, 
  #          label = "NordetStP bank", size=2, color='black') +
  # 
  # annotate(geom="rect", xmin=StPierre.bank[1], xmax=StPierre.bank[3], 
  #          ymin=StPierre.bank[2], ymax=StPierre.bank[4], 
  #          colour = "red", fill = NA) +
  # annotate(geom="text", x=mean(StPierre.bank[1], StPierre.bank[3])+0.8, 
  #          y= mean(StPierre.bank[2], StPierre.bank[4])-0.1, 
  #          label = "StPierre bank", size=2, color='black') +
  # annotate(geom="rect", xmin=CapHorn.bank[1], xmax=CapHorn.bank[3], 
  #          ymin=CapHorn.bank[2], ymax=CapHorn.bank[4], 
  #          colour = "red", fill = NA) +
  # annotate(geom="text", x=mean(CapHorn.bank[1], CapHorn.bank[3])+0.8, 
  #          y= mean(CapHorn.bank[2], CapHorn.bank[4])-0.1, 
  #          label = "CapHorn bank", size=2, color='black') +
  # annotate(geom="rect", xmin=milles45.bank[1], xmax=milles45.bank[3], 
  #          ymin=milles45.bank[2], ymax=milles45.bank[4], 
  #          colour = "red", fill = NA) +
  # annotate(geom="text", x=mean(milles45.bank[1], milles45.bank[3])+0.8, 
  #          y= mean(milles45.bank[2], milles45.bank[4])-0.1, 
  #          label = "milles45 bank", size=2, color='black') +
  # annotate(geom="rect", xmin=JeanLuc.bank[1], xmax=JeanLuc.bank[3], 
  #          ymin=JeanLuc.bank[2], ymax=JeanLuc.bank[4], 
  #          colour = "red", fill = NA) +
  # annotate(geom="text", x=mean(JeanLuc.bank[1], JeanLuc.bank[3])-0.8, 
  #          y= mean(JeanLuc.bank[2], JeanLuc.bank[4])-0.1, 
  #          label = "JeanLuc bank", size=2, color='black') +
  # 
  # annotate(geom="rect", xmin=tmp5.bank[1], xmax=tmp5.bank[3], 
  #          ymin=tmp5.bank[2], ymax=tmp5.bank[4], 
  #          colour = "red", fill = NA) +
  # annotate(geom="text", x=mean(tmp5.bank[1], tmp5.bank[3])-0.6, 
  #          y= mean(tmp5.bank[2], tmp5.bank[4])-0.025, 
  #          label = "tmp5 bank", size=2, color='black') +
  # annotate(geom="rect", xmin=tmp4.bank[1], xmax=tmp4.bank[3], 
  #          ymin=tmp4.bank[2], ymax=tmp4.bank[4], 
  #          colour = "red", fill = NA) +
  # annotate(geom="text", x=mean(tmp4.bank[1], tmp4.bank[3])+0.6, 
  #          y= mean(tmp4.bank[2], tmp4.bank[4])-0.025, 
  #          label = "tmp4 bank", size=2, color='black') +
  # annotate(geom="rect", xmin=NordetAms.bank[1], xmax=NordetAms.bank[3], 
  #          ymin=NordetAms.bank[2], ymax=NordetAms.bank[4], 
  #          colour = "red", fill = NA) +
  # annotate(geom="text", x=mean(NordetAms.bank[1], NordetAms.bank[3])-0.8, 
  #          y= mean(NordetAms.bank[2], NordetAms.bank[4])-0.1, 
  #          label = "NordetAms bank", size=2, color='black') 
  #
#plot(standard.map.plot)

ggsave("Figures/Bathymetry/Carte.SPA.pdf",width = 30, height = 20, units = "cm")
save(standard.spa.map.plot, file='Data/Spatial_data_frames/standard.map.plot.RData')
save(iso.bath.sf, file='Data/Spatial_data_frames/isobath.gebco.spa.RData')
