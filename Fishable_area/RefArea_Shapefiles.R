#Script to build shapefiles for Reference areas (both projected and back-projected)

#Load CCAMLR Areas
ASDs=load_ASDs()
EEZs=load_EEZs()
MAs=load_MAs()
MPAs=load_MPAs()

#1. RefArea HIMI (HIMI EEZ within 58.5.2)
#Take HIMI EEZ
HIMI=EEZs[EEZs$GAR_Short_Label=="HIMI",]
#Take Division 58.5.2
D5852=ASDs[ASDs$GAR_Short_Label=="5852",]
#Remove area outside 58.5.2 from HIMI EEZ
HIMI=suppressWarnings( st_intersection(HIMI,D5852) )
#Add name to polygon
HIMI$name="HIMI"
HIMI=HIMI%>%select(name)

#2. RefArea CROZET (HIMI CROZET within 58.5.2)
#Take CI EEZ
CI=EEZs[EEZs$GAR_Short_Label=="CI",]

#Add name to polygon
CI$name="CI"
CI=CI%>%select(name)

#3. Merge polygons
RefAreas=rbind(CI,HIMI)

#4. Export
st_write(RefAreas, "Data/RefAreas.shp",append = F,quiet = T)

#5. Plot 
#plot(st_geometry(RefAreas),col=rainbow(3))



