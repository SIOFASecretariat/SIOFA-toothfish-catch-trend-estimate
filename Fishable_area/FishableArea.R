#Script to compute fishable areas - only APSOI Del Cano at the moment


#Load CCAMLR Areas
ASDs=load_ASDs()
EEZs=load_EEZs()
MAs=load_MAs()
MPAs=load_MPAs()

# Fishable depth range 

minDepth = 600 # 500
maxDepth = 2000 # 1800
# 
# 
# quantiles_depth_dc= quantile(Catch %>% filter(Depth<2500, MU=='DC') %>% pull(Depth), probs=c(0.025, 0.975))
# quantiles_depth_sir= quantile(Catch %>% filter(Depth<2500, MU=='SIR') %>% pull(Depth), probs=c(0.025, 0.975))
# 
# minDepth = round(min(c(quantiles_depth_dc[1], quantiles_depth_sir[1])),0)
# maxDepth = round(max(c(quantiles_depth_dc[2], quantiles_depth_sir[2])),0)


#Load reference areas (created in RefArea_Shp_Maker.R)
RefAreas=st_read(dsn=path.expand(paste0(getwd(),'/Data')), layer="RefAreas", quiet = TRUE)

#Get the unprojected GEBCO data
B=rast("C:/Users/jules/Dropbox/PostDoc_MNHN/RworkingDirectory/Rproject_Lobster/Data/Gebco_Bathymetry/gebco_2022_n-25.5762_s-60.0293_w37.2656_e104.2383.tif")

#Load SIOFA TOT MU - not up to date 
#totMU <- sf::read_sf("Data/TOT_MU.shp") 

#Create Del Cano MU polygon 
lon = c(41, 41, 44.15, 44.15, 48, 48 )
lat = c(-45, -44, -44, -43.5, -43.5, -45)
Poly_Coord_df = data.frame(lon, lat)

DC = Poly_Coord_df %>% 
  mutate(ID='DC') %>% 
  st_as_sf(coords = c("lon", "lat"), 
           crs = 4326) %>% 
  group_by(ID) %>%
  dplyr::summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") %>% 
   st_as_sfc() %>% 
  st_as_sf(crs = 4326) 
 
DC= st_transform(DC, crs(RefAreas[RefAreas$name=="CI",]))

#ggplot(DC)+ geom_sf()


#Create South Indian Ridge MU polygon 
lon = c(40.3,40.3,43.3,43.3)
lat = c(-44,-40,-44,-40)
Poly_Coord_df = data.frame(lon, lat)

SIR = Poly_Coord_df %>% 
  mutate(ID='SIR') %>% 
  st_as_sf(coords = c("lon", "lat"), 
           crs = 4326) %>% 
  st_bbox() %>% 
  st_as_sfc() %>% 
  st_as_sf(crs = 4326) %>%
  mutate(ID='SIR') 


SIR= st_transform(SIR, crs(RefAreas[RefAreas$name=="CI",]))

#ggplot(SIR)+ geom_sf()


#Take CCAMLR area
CCAMLRarea=ASDs[ASDs$GAR_Short_Label%in%c("587","586"),]
CCAMLReez=EEZs[EEZs$GAR_Short_Label%in% c("CI","PEI"),]

CCAMLRarea=st_transform(CCAMLRarea,crs=crs(RefAreas[RefAreas$name=="CI",])) %>% mutate(name=GAR_Name)
CCAMLRarea=CCAMLRarea%>%select(name) 

CCAMLReez=st_transform(CCAMLReez,crs=crs(RefAreas[RefAreas$name=="CI",]))%>% mutate(name=GAR_Name)
CCAMLReez=CCAMLReez%>%select(name)

CCAMLR_areas = rbind(CCAMLRarea,CCAMLReez)
CCAMLR_areas = st_union(st_combine(CCAMLRarea),st_combine(CCAMLReez), by_feature = T)

#Remove CCAMLR area
DelCano=suppressWarnings( st_difference(DC, CCAMLR_areas ) )
SIR=suppressWarnings( st_difference(SIR, CCAMLR_areas ) )

#Plot 
#ggplot(CCAMLR_areas)+ geom_sf() +   geom_sf(data=DelCano )

#Add name to polygon
DelCano$name="DC"
DelCano=DelCano%>% dplyr::rename(geometry=x) %>% select(name, geometry) 

SIR$name="SIR"
SIR=SIR%>% dplyr::rename(geometry=x) %>% select(name, geometry) 

#back-Project Polys to Latitudes/Longitudes
DelCano=st_transform(DelCano,crs=4326)
SIR=st_transform(SIR,crs=4326)
RefAreas=st_transform(RefAreas,crs=4326)

# Export
st_write(DelCano, "Data/DelCano.shp",append = F,quiet = T)
st_write(SIR, "Data/SIR.shp",append = F,quiet = T)

#Add RefAreas to PolysLL
PolysLL= rbind(DelCano,SIR, RefAreas)#rbind(PolysLL,RefAreas)

PolysMU= rbind(DelCano,SIR)#rbind(PolysLL,RefAreas)

# Export
st_write(PolysMU, "Data/PolysMU.shp",append = F,quiet = T)
st_write(PolysLL, "Data/PolysLL.shp",append = F,quiet = T)

#At this point, 'PolysLL' contains all areas for which fishable areas need to be computed

#Convert Polys to Spatvector for the terra package
PolysLLsv=vect(PolysLL)

#Loop over polygons that are inside PolysLLsv
RawAr=data.frame(Poly=character(),Area=numeric()) #Prepare empty output

for (i in seq(1,length(PolysLLsv))){
  #Take one polygon
  pol=PolysLLsv[i,]
  #Get its name
  pname=PolysLLsv$name[i]
  #Take bathymetry data that matches the extent of the polygon
  Btmp=crop(B,ext(pol))
  #Turn GEBCO cells that are not inside the polygon into NAs
  Btmp=terra::mask(Btmp,pol)
  #Turn cells outside the fishable depth into NAs
  Btmp = classify(Btmp, cbind(-100000, -maxDepth, NA), right=TRUE)
  Btmp = classify(Btmp, cbind(-minDepth, 100000, NA), right=FALSE)
  #Compute the area covered by cells that are not NA
  Ar=round(expanse(Btmp, unit="km"),2)
  #Store result
  RawAr=rbind(RawAr,data.frame(Poly=pname,Area=Ar))
}

RawAr=data.frame(Poly=RawAr[,1],Area=RawAr[,3])

#Export
write.csv(RawAr,'Data/FishableArea2023.csv',row.names = F)

