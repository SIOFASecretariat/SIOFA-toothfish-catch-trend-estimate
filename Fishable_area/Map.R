#Script to generate Management Unit map thumbnails
#Launch RefArea_Shapefiles & FishableArea, 01_LoadData

# load spatial objects 
SIOFAsubarea <- sf::read_sf("Data/siofa_subareas_final.shx") 
st_crs(SIOFAsubarea)<-4326
SIOFAsubarea=st_transform(SIOFAsubarea,crs=4326)
SIOFAsubarea$name=NA

EEZs=load_EEZs()
world <- ne_countries(scale = "medium", returnclass = "sf")

load("Data/sf_spatial_objects_Roberto.Rdata")

DelCano=st_read(dsn=path.expand(paste0(getwd(),'/Data')), layer="DelCano", quiet = TRUE)
SIR=st_read(dsn=path.expand(paste0(getwd(),'/Data')), layer="SIR", quiet = TRUE)
PolysMU= rbind(DelCano,SIR)#rbind(PolysLL,RefAreas)

#Get Bathy (! UP TO DATE)
B=rast("C:/Users/jules/Dropbox/PostDoc_MNHN/RworkingDirectory/Rproject_Lobster/Data/Gebco_Bathymetry/gebco_2022_n-25.5762_s-60.0293_w37.2656_e104.2383.tif")


minDepth = 600 # 500
maxDepth = 2000 # 2000

#Catch data 

Catch= read.csv( "Output/Output_Catch.csv")

Catch$Catch =  rowSums(cbind(Catch$Catch_Kg , Catch$Weigth),na.rm=T) 

Catch = Catch %>% mutate(CPUE= Catch/HooksSet) 

Catch$DistLL = 
  sapply(1:nrow(Catch),function(i)
    distGeo(as.data.frame(Catch)[i,c('fishopSetStartLongitude','fishopSetStartLatitude')],
            as.data.frame(Catch)[i,c('fishopSetEndLongitude','fishopSetEndLatitude')]))

Catch=as.data.frame(Catch)
Catch = Catch %>% 
  mutate(Depth = rowMeans(.[, c("StartDepth","EndDepth")])) %>% 
  mutate(fishopSetEndDate = as.POSIXct(fishopSetEndDate), 
         fishopSetStartDate = as.POSIXct(fishopSetStartDate),
         fishopHaulStartDate = as.POSIXct(fishopHaulStartDate), 
         fishopHaulEndDate = as.POSIXct(fishopHaulEndDate)) %>%
  mutate(Latitude = rowMeans(.[, c("fishopSetStartLatitude","fishopSetEndLatitude")])) %>% 
  mutate(Longitude = rowMeans(.[, c("fishopSetStartLongitude","fishopSetEndLongitude")])) %>% 
  mutate(SizeCatch = Catch_Kg/max(Catch_Kg))


# Map area generic -------------------------------------------------------------
TOP_MU_map = 
  ggplot(data=Catch) + 
  xlim(38,49)  + ylim(-46, -40) +xlab("Longitude") + ylab("Latitude")+
  theme(panel.background = element_rect(fill = "transparent", colour="black")) + 
  
  geom_sf(data= PolysMU,  fill = "lightgrey", inherit.aes=F) +
  
  geom_sf(data=Isobath_IO_felix_sf  %>% dplyr::filter(level %in% c("-500")), colour = "grey80", inherit.aes=F, size=0.5) + 
  geom_sf(data=Isobath_IO_felix_sf  %>% dplyr::filter(level %in% c("-1000")), colour = "grey60" ,fill= "grey60", inherit.aes=F, size=0.5)+
  geom_sf(data=Isobath_IO_felix_sf  %>% dplyr::filter(level %in% c("-1500")), colour = "grey30", inherit.aes=F, size=0.5)+
  geom_sf(data=Isobath_IO_felix_sf  %>% dplyr::filter(level %in% c("-2000")), colour = "black", inherit.aes=F, size=0.5)+
  geom_hline(yintercept= -45, size= 0.5, linetype ="dashed", show.legend = TRUE) +
  annotate("text", x =44.5, y=-45.5, label="CCAMLR",
           color="black", size =3) +
  annotate("text", x =46, y=-43, label="SIOFA",
           color="black", size =3)+
  annotate("text", x =47, y=-44.5, label="Crozet EEZ",
           color="black", size =3)+
  geom_richtext(aes(x =42.5, y=-44.2, label="**Del Cano MU**"), 
                color="black", size =2)+
  geom_richtext(aes(x =39.5, y=-42.5, label="**South Indian Ridge MU**"), 
                color="black", size =2)+
  geom_sf(data=EEZ_sf, inherit.aes=F, fill="transparent",size=0.6) +
  geom_sf(data = FR_island_sf, fill = "darkgrey", inherit.aes=F) 

save(TOP_MU_map, file="Data/TOP_MU_map.RData")


# Map area bathy -------------------------------------------------------------- 
#Bind
MUs=PolysMU #rbind(PolysMU)

#Get Ref Areas
RA=st_read(dsn=path.expand(paste0(getwd(),'/Data')), layer="RefAreas", quiet = TRUE)
RA=st_transform(RA,crs=4326)
RA=RA%>% filter(name%in%c('HIMI'))

#Get contour of ASD to mask bathy outside
Cont=st_union(SIOFAsubarea)
#Cont=st_buffer(st_union(SIOFAsubarea),dist=0.5)

#Extract outer boundary
pol=Cont[[1]][[1]]
pol=st_polygon(list(pol))

#Mask bathy
Bm=suppressWarnings(terra::mask(B, vect(pol)))
st_bbox(pol)

#Get labels
#Labs=read.csv("Data/LabelsRBs.csv")

#Plot

Dcuts=c(-15000,-1800, -1500,-1000,-600,0,10000)
Dcols=c(scales::alpha("grey90", 0),'aquamarine','aquamarine4','aquamarine3','grey90','white')
 
png(filename = 'Data/Map_area.png', width = 2100, height = 1900, units = "px", pointsize = 12,
     bg = "white", res = 200)

#par(mai=c(0.5,1,0.5,0.5),xaxs='i',yaxs='i')
plot(NULL, xlim=c(35,90),ylim=c(-55,-15), axes=F)

plotmap( lon=c(32,93) , lat=c(-56,-13), add=T)

#plot(NULL, xlim=c(35,90),ylim=c(-55,-25), axes=T)

#plot(Bm,legend=FALSE,axes=F, add = T) 

plot(Bm,breaks=Dcuts,col=Dcols,legend=FALSE,axes=FALSE,  add = T, asp=1.5) #xpd=T, 

#add_RefGrid(bb=st_bbox(Bm),
#            ResLat=5,ResLon=10,LabLon=0, LatR = c(-55, -25), offset = 0.2,lwd=1,fontsize = 0.9)

#plot(st_geometry(world), col='grey20',border='black',lwd=1, add=T)
plot(st_geometry(SIOFAsubarea),xlim=c(35,90), ylim=c(-55,-25),add=T,lwd=2,border="black", col = scales::alpha("grey90", 0.2))
plot(st_geometry(MUs),add=T,lwd=2,border="blue",xpd=T)
plot(st_geometry(RA),add=T,lwd=2,border=rgb(1,0.5,0,0.5),col=rgb(1,0.5,0,0.4),xpd=T)

text(77,-52,"HIMI",adj=c(0.5,0.5),cex=1.1,col='black',font=2)
text(51,-44,"CI",adj=c(0.5,0.5),cex=1.1,col='black',font=2)

text(37,-37,"SubArea 3.b",adj=c(0.5,0.5),cex=1.4,col='black',font=2)

text(42,-39,"SIR",adj=c(0.5,0.5),cex=1.1,col='blue',font=2)
text(46,-43,"DC",adj=c(0.5,0.5),cex=1.1,col='blue',font=2)

dev.off()



#Management area thumbnails ----------------------------------------------------
#Create Area Polygons
dc=MUs[grep("DC",MUs$name),]
dc=st_union(dc)
sir=MUs[grep("SIR",MUs$name),]
sir=st_union(sir)


#Temporary conversion to sp while xpd is fixed in sf
MUs=as_Spatial(MUs)
dc=as_Spatial(dc)
sir=as_Spatial(sir)


#plot
png(filename = 'Data/3b.png', width = 1000, height = 1000, units = "px", pointsize = 12,
    bg = "transparent", res = 200)

par(mai=c(0.11,0.15,0.05,0.12),xaxs='i',yaxs='i')
plot(st_geometry(SIOFAsubarea),col='grey70',border='black',lwd=1, xlim=c(50,60), ylim=c(-55,-20))
plot(st_geometry(world), col='grey20',border='black',lwd=1, add=T)

plot(MUs,col='white',lwd=0.75,xpd=T,add=T)
plot(dc,col=rgb(0.1,0.1,0.1,0.0),lwd=2,add=T,xpd=T)
plot(sir,col=rgb(0.1,0.1,0.1,0.0),lwd=2,add=T,xpd=T)
text(40,-37,"SubArea 3.b",adj=c(0.5,0.5),cex=1.4,col='black',font=2)

text(38.5,-40,"SIR",adj=c(0.5,0.5),cex=1.1,col='blue',font=2)
text(46,-42.5,"DC",adj=c(0.5,0.5),cex=1.1,col='blue',font=2)

dev.off()


