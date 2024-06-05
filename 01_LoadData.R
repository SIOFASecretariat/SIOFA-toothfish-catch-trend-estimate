#-------------------------------------------------------------------------------
## Load Data from 
#-------------------------------------------------------------------------------

#to do/check :
#1. Script/function to ensure that the catch, tagging and biological have the same format 
#2. Add species information in tagging  
#3. fishes not linked in tag link data transferred by the secretariat => do the linking 
#4. Check links period 

# Get fishable area ------------------------------------------------------------

# RefAreas 
Ref_area_seabed = fishable_area[fishable_area$Poly%in%c(RefArea),]

# MU
RB_area_seabed = fishable_area[fishable_area$Poly%in%Mu,]

#Get spatial objects ------------------------------------------------------------

# RefAreas 
RefAreas=st_read(dsn=path.expand(paste0(getwd(),'/Data')), layer="RefAreas", quiet = TRUE)

# MU
DelCano=st_read(dsn=path.expand(paste0(getwd(),'/Data')), layer="DelCano", quiet = TRUE)
SIR=st_read(dsn=path.expand(paste0(getwd(),'/Data')), layer="SIR", quiet = TRUE)
MU=st_read(dsn=path.expand(paste0(getwd(),'/Data')), layer="PolysMU", quiet = TRUE)
 
#Biomass and CV for Reference Areas history ------------------------------------

# 2021-2023:
# HIMI_biomass_est=31111 
# HIMI_CV_biomass_est=0.0281

# Set parameters ---------------------------------------------------------------
#Min and max seasons for data queries
maxSeason=Est_Season
minSeason=Min_Season

#For CPUE-based estimation: get hauls in last 3 seasons in the Reference Areas to get median CPUE
#HIMI/Crozet assessment is done before the end of the fishing season so last season should be assessment season minus one
cpue_seasons=seq(maxSeason-2,maxSeason) 
cpue_seasons=cpue_seasons-1

HIMI_Ass_seasons = CI_Ass_seasons = last(cpue_seasons)

# expanded tag_parameters
tag_pars=list("mean_wt"=0,
                  "method"="Chapman",
                  "unit"="kg",
                  "type"=1,
                  "tag_mort"=0.1,
                  "reporting"=1,
                  "nat_mort"=0.155,
                  "chronic_shed"=0.0084,
                  "chronic_mort"=0)

#Source disambiguator (Used to merge ambiguous links that share season and RB)
source("Functions/Disambiguator.R")

#################################################################################################################

#Initialize Report card####
#Open Report Card
Rcard = file(paste0('Output/Biomass_Estimation_ReportCard_',as.character(Time),'.txt'),"w")
#Write Header 
cat("Biomass Estimation Report Card", file = Rcard, sep = "\n")
cat(paste0('Date: ',Time), file = Rcard, sep = "\n")
cat("--------------------------------------------", file = Rcard, sep = "\n")
cat("", file = Rcard, sep = "\n")
#####


#Load Data ---------------------------------------------------------------------

# catch and effort 
LL= read_excel( "Data/SIOFA_3b_Toothfish_catch_effort.xlsx")

# tagging 
Rel= read_excel( "Data/SIOFA_3b_tagging.xlsx",sheet ='tag_release_3b')
Rec= read_excel( "Data/SIOFA_3b_tagging.xlsx",sheet ='tag_recapture_3b')
Rlink= read_excel( "Data/SIOFA_3b_tagging.xlsx",sheet ='tag_linking')

# link tag fished and set
LinkOP_release= read_excel( "Data/SIOFA_3b_tag_operation_link.xlsx",sheet ='tag_release')
LinkOP_recapture= read_excel( "Data/SIOFA_3b_tag_operation_link.xlsx",sheet ='tag_recapture')

# biological sampling
Bio= read_excel( "Data/SIOFA_3b_fish_sampling.xlsx")

# Catch ------------------------------------------------------------------------
# filter species & area 
Catch = LL %>% mutate(Season = SIOFA_Season(fishopSetStartDate)) %>% 
  filter(Season>=minSeason & Season<=maxSeason & speciesFAOCode %in%c('TOP')) %>% 
  filter(Subarea == '3b')

#Assign MUs 
Catch=assign_areas_rev(Input=Catch,
                   NamesIn = c("fishopSetStartLatitude","fishopSetStartLongitude"),
                   Polys = c('MU'),
                   AreaNameFormat=c('name'),
                   Buffer=0, 
                   NamesOut=c('MU'))

Catch = Catch %>% filter(MU %in% Mu) 


# Release ----------------------------------------------------------------------
#Warnings multiple relationship 
Release = left_join(LinkOP_release, Rel, by="obs_foID", multiple="first")

Release = Release %>%  mutate(Season = SIOFA_Season(Dat_release)) %>% 
  filter(Season>=minSeason & Season<=maxSeason ) %>%  #speciesFAOCode %in%c('TOP')
  filter(Subarea == '3b')

#Assign MUs 
Release=assign_areas_rev(Input=Release,
                       NamesIn = c("Lat_release","Lon_release"),
                       Polys = c('MU'),
                       AreaNameFormat=c('name'),
                       Buffer=0, 
                       NamesOut=c('MU'))

Release = Release %>% filter(MU %in% Mu) 

#tag code
Release$tag_code_1 = paste(Release$tagrel1Type,
                           Release$tagrel1Colour,
                           Release$tagrel1Number,
                           Release$tagrel1Wording, sep="+")

Release$tag_code_2 = paste(Release$tagrel2Type,
                             Release$tagrel2Colour,
                             Release$tagrel2Number,
                             Release$tagrel2Wording, sep="+")
# Length weigth data -----------------------------------------------------------
Biology = Bio %>%  mutate(Season = SIOFA_Season(SettingStartDatetime)) %>% 
  filter(Season>=minSeason & Season<=maxSeason ) %>%  #speciesFAOCode %in%c('TOP')
  filter(SubArea == '3b')

#Assign MUs 
Biology=assign_areas_rev(Input=Biology,
                         NamesIn = c("SettingStartLatitude","SettingStartLongitude"),
                         Polys = c('MU'),
                         AreaNameFormat=c('name'),
                         Buffer=0, 
                         NamesOut=c('MU'))

Biology = Biology %>% filter(MU %in% Mu) 


#Estimate fish weigth ----------------------------------------------------------
Release$Est_weigth_release = est_fish_weight_rev(length_weight_data=Biology, length_data= Release)

#Correct catch data ------------------------------------------------------------
ReleaseCatch_cor = Release %>% group_by(opeID) %>% dplyr::summarise(Weigth=sum(Est_weigth_release))

Catch <- left_join(Catch, ReleaseCatch_cor, by="opeID")

#Recapture ---------------------------------------------------------------------
Recapture = left_join(LinkOP_recapture, Rec, by="obs_foID", multiple="first")

Recapture = Recapture %>%  mutate(Season = SIOFA_Season(Dat_recapture)) %>% 
  filter(Season>=minSeason & Season<=maxSeason ) %>%  #speciesFAOCode %in%c('TOP')
  filter(Subarea == '3b')

#Assign MUs 
Recapture=assign_areas_rev(Input=Recapture,
                         NamesIn = c("Lat_recapture","Lon_recapture"),
                         Polys = c('MU'),
                         AreaNameFormat=c('name'),
                         Buffer=0, 
                         NamesOut=c('MU'))

#Recapture = Recapture %>% filter(MU %in% Mu) 

#tag code
Recapture$tag_code_1 = paste(Recapture$tagrec1Type,
                             Recapture$tagrec1Colour,
                             Recapture$tagrec1Number,
                             Recapture$tagrec1Wording, sep="+")

Recapture$tag_code_2 = paste(Recapture$tagrec2Type,
                             Recapture$tagrec2Colour,
                             Recapture$tagrec2Number,
                             Recapture$tagrec2Wording, sep="+")

#Linked ---------------------------------------------------------------------
Rlinked = left_join(Rlink, Recapture %>% dplyr::select(opeID,tagrecID),
                    by=c("Recapture_tagID" = "tagrecID"))


Rlinked = Rlinked %>%  mutate(Release_Season = SIOFA_Season(Release_Date),
                            Recapture_Season = SIOFA_Season(Recapture_Date)) %>% 
  filter(Recapture_Season>=minSeason & Recapture_Season<=maxSeason ) %>% 
  filter( species3ACode %in%c('TOP')) #%>%
  #filter(Release_Area == "SIOFA")


#Assign MUs 
Rlinked=assign_areas_rev(Input=Rlinked,
                           NamesIn = c("Release_Latitude","Release_Longitude"),
                           Polys = c('MU'),
                           AreaNameFormat=c('name'),
                           Buffer=0, 
                           NamesOut=c('Release_MU'))

Rlinked=assign_areas_rev(Input=Rlinked,
                         NamesIn = c("Recapture_Latitude","Recapture_Longitude"),
                         Polys = c('MU'),
                         AreaNameFormat=c('name'),
                         Buffer=0, 
                         NamesOut=c('Recapture_MU'))

#Rlinked = Rlinked %>% filter(Recapture_MU %in% Mu, Release_MU %in% Mu) 

#Merge ambiguous links
# Links=Disambiguator(Links=Links,
#                     RecF=c("season_ccamlr_recapture","RESEARCH_BLOCK_CODE_RECAPTURE"),
#                     RelF=c('season_ccamlr_release','RESEARCH_BLOCK_CODE_RELEASE'),
#                     append='Y')


#Prep to export for post-processing
T_Rels=Release
T_Recs=Recapture[Recapture$MU%in%Mu,] 
T_Links=Rlinked#[Rlinked$Release_MU%in%Mu,] 
T_Catch=Catch
T_Biol =Biology

#Write
write.csv(T_Links,paste0("Output/Output_Recaptures_Linked_",Time,".csv"),row.names = FALSE)
write.csv(T_Rels,paste0("Output/Output_All_Releases_",Time,".csv"),row.names = FALSE)
write.csv(T_Recs,paste0("Output/Output_All_Recaptures_",Time,".csv"),row.names = FALSE)
write.csv(T_Catch,paste0("Output/Output_All_Catch_",Time,".csv"),row.names = FALSE)
write.csv(T_Biol,paste0("Output/Output_All_Biology_",Time,".csv"),row.names = FALSE)

rm(T_Rels,T_Recs,T_Links,T_Catch, T_Biol)

#Find releases that have been recaptured elsewhere and are therefore not available for recapture
Migrants=Rlinked%>%filter(Recapture_MU!=Release_MU)
RelEm=Release[Release$tagrelID%in%Migrants$Release_TagID, ]
RelEm=dplyr::summarise(group_by(RelEm,MU),n=n())
#Report on releases that are removed for Chapman estimates (after CPUE estimates, in EstimateBiomass.R)
message("Releases that emigrated and will be excluded from Chapman estimates:")
for(i in seq(1,nrow(RelEm))){message(paste(RelEm[i,],collapse = ": "))}


#Filter Links
# remove tagged outside MU
Rlinked=Rlinked%>%filter(Release_MU %in% Mu)

# remove recapture outside MU
Rlinked=Rlinked%>%filter(Recapture_MU == Release_MU)

# remove within season recaptures 
Rlinked=Rlinked%>%filter(Recapture_Season!=Release_Season)

#year-at-liberty filter of Links
YatL3=Rlinked%>% #1-3 years at liberty
  filter(Recapture_Season-Release_Season<=3)
Flinks=rbind(YatL3)
rm(YatL3)
Flinks=Flinks[Flinks$Recapture_Season>=(Est_Season-4),] #Last 5 seasons
L_TOP=Flinks[Flinks$Recapture_MU%in%Mu & Flinks$species3ACode=='TOP',] #TOP

Flinks=rbind(L_TOP)
rm(L_TOP)

Lcounts=dplyr::summarise(group_by(Flinks,Recapture_MU,Recapture_Season),n=n(),.groups = "drop")

#Export filtered Links
write.csv(Flinks,paste0("Output/Output_Recaptures_Linked_Filtered_",Time,".csv"),row.names = FALSE)
rm(Flinks)


#Data processing ####
cat("###Data Processing start###########################", file = Rcard, sep = "\n")
cat("", file = Rcard, sep = "\n")


#1. Process catch data####
cat("#Catch data:", file = Rcard, sep = "\n")

#Missing datetime_set_start
indx=which(is.na(Catch$fishopSetStartDate)==T)
#if(length(indx)>0){Catch=Catch[-indx,]}
cat(paste0("Missing datetime_set_start: ",length(indx),' records without datetime_set_start'), file = Rcard, sep = "\n")
rm(indx)

#Missing datetime_set_end
indx=which(is.na(Catch$fishopSetEndDate)==T)
#if(length(indx)>0){Catch=Catch[-indx,]}
cat(paste0("Missing datetime_set_end: ",length(indx),' records without datetime_set_end'), file = Rcard, sep = "\n")
rm(indx)

#datetime_set_end<datetime_set_start
indx=which(Catch$fishopSetEndDate<Catch$fishopSetStartDate)
#if(length(indx)>0){Catch=Catch[-indx,]}
cat(paste0("datetime_set_end<datetime_set_start: ",length(indx),' records with datetime_set_end<datetime_set_start'), file = Rcard, sep = "\n")
rm(indx)

#Missing longitude_set_start
indx=which(is.na(Catch$fishopSetStartLongitude)==T)
if(length(indx)>0){Catch=Catch[-indx,]}
cat(paste0("Missing longitude_set_start: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

#Missing latitude_set_start
indx=which(is.na(Catch$fishopSetStartLatitude)==T)
if(length(indx)>0){Catch=Catch[-indx,]}
cat(paste0("Missing latitude_set_start: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

#Missing longitude_set_end
indx=which(is.na(Catch$fishopSetEndLongitude)==T)
if(length(indx)>0){Catch=Catch[-indx,]}
cat(paste0("Missing longitude_set_end: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

#Missing latitude_set_end
indx=which(is.na(Catch$fishopSetEndLatitude)==T)
if(length(indx)>0){Catch=Catch[-indx,]}
cat(paste0("Missing latitude_set_end: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

#Missing line_length_m
# 
# Catch = Catch %>% mutate(line_length= vecdistGeo(p1=c(fishopSetStartLongitude, fishopSetStartLatitude), 
#                                               p2=c(fishopSetEndLongitude, fishopSetEndLatitude)))
#  indx=which(is.na(Catch$line_length_m)==T)
#  if(length(indx)>0){Catch=Catch[-indx,]}
#  cat(paste0("Missing line_length_m: ",length(indx),' records removed'), file = Rcard, sep = "\n")
#  rm(indx)

#Missing greenweight_caught_kg
indx=which(is.na(Catch$Catch_Kg)==T)
if(length(indx)>0){Catch$Catch_Kg[indx]=0}
cat(paste0("Missing greenweight_caught_kg: ",length(indx),' records replaced by zero'), file = Rcard, sep = "\n")
rm(indx)

cat("", file = Rcard, sep = "\n")
#####

#2. Process Length-Weight data####
cat("#Length-Weight data:", file = Rcard, sep = "\n")

#Missing datetime_set_start
indx=which(is.na(Biology$SettingStartDatetime)==T)
if(length(indx)>0){Biology=Biology[-indx,]}
cat(paste0("Missing datetime_set_start: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

#Missing datetime_set_end
# indx=which(is.na(Biology$datetime_set_end)==T)
# if(length(indx)>0){Biology=Biology[-indx,]}
# cat(paste0("Missing datetime_set_end: ",length(indx),' records removed'), file = Rcard, sep = "\n")
# rm(indx)

#datetime_set_end<datetime_set_start
# indx=which(Biology$datetime_set_end<LW$datetime_set_start)
# if(length(indx)>0){Biology=Biology[-indx,]}
# cat(paste0("datetime_set_end<datetime_set_start: ",length(indx),' records removed'), file = Rcard, sep = "\n")
# rm(indx)

#Missing asd_code
# indx=which(is.na(Biology$asd_code)==T)
# if(length(indx)>0){Biology=Biology[-indx,]}
# cat(paste0("Missing asd_code: ",length(indx),' records removed'), file = Rcard, sep = "\n")
# rm(indx)

#Missing length_total_cm
indx=which(is.na(Biology$bsLength_cm)==T)
if(length(indx)>0){Biology=Biology[-indx,]}
cat(paste0("Missing length_total_cm: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

#Missing greenweight_kg
indx=which(is.na(Biology$bsWeight_Kg)==T)
if(length(indx)>0){Biology=Biology[-indx,]}
cat(paste0("Missing greenweight_kg: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)
cat("", file = Rcard, sep = "\n")
#####

cat("", file = Rcard, sep = "\n")



#3. Process Releases data####
cat("#Releases data:", file = Rcard, sep = "\n")

#Missing date_release
indx=which(is.na(Release$Dat_release)==T)
if(length(indx)>0){Release=Release[-indx,]}
cat(paste0("Missing date_release: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

#Missing longitude_release
indx=which(is.na(Release$longitude_release)==T)
if(length(indx)>0){Release=Release[-indx,]}
cat(paste0("Missing longitude_release: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

#Missing latitude_release
indx=which(is.na(Release$latitude_release)==T)
if(length(indx)>0){Release=Release[-indx,]}
cat(paste0("Missing latitude_release: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

#Replace missing length_total_cm by CRUISE-SPECIES average
MeanLengths=dplyr::summarise(group_by(Release,datasetID),L=mean(Length_release,na.rm=T),.groups = "drop") #taxon_code

indx=which(is.na(Release$Length_release)==T)
if(length(indx)>0){
  tmpdat=Release[indx,c("datasetID","Length_release")] #"taxon_code"
  tmpdat=left_join(tmpdat,MeanLengths,by = c("datasetID")) # "taxon_code"
  Release$Length_release[indx]=tmpdat$L
  rm(tmpdat)
}
cat(paste0("Missing LENGTH_CM replaced by CRUISE-SPECIES average for: ",length(indx),' records'), file = Rcard, sep = "\n")
rm(indx,MeanLengths)

#Estimate weight of released fish using LW data#### 
cat(paste0('Add weight estimate for ind. with missing length'), file = Rcard, sep = "\n")

# Release$EST_WEIGHT_KG=est_fish_weight_rev(length_weight_data=Biology[,c("MU", "Season", "species3ACode", "bsLength_cm", "bsWeight_Kg", "bsSex")],
#                                    length_data=Release[,c("MU", "Season", "datasetID", "opeID", "Length_release")])

#####

cat("", file = Rcard, sep = "\n")
#####

#4. Process Recaptures data####
cat("#Recaptures data:", file = Rcard, sep = "\n")

#Missing date_release
indx=which(is.na(Rlinked$Release_Date)==T)
if(length(indx)>0){Rlinked=Rlinked[-indx,]}
cat(paste0("Missing date_release: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

#Missing longitude_release
indx=which(is.na(Rlinked$Release_Longitude)==T)
if(length(indx)>0){Rlinked=Rlinked[-indx,]}
cat(paste0("Missing longitude_release: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

#Missing latitude_release
indx=which(is.na(Rlinked$Release_Latitude)==T)
if(length(indx)>0){Rlinked=Links[-indx,]}
cat(paste0("Missing latitude_release: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

#Missing research_block_release
indx=which(is.na(Rlinked$Release_MU)==T)
if(length(indx)>0){Rlinked=Links[-indx,]}
cat(paste0("Missing research_block_release: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

#Missing date_recapture
indx=which(is.na(Rlinked$Recapture_Date)==T)
if(length(indx)>0){Rlinked=Rlinked[-indx,]}
cat(paste0("Missing date_recapture: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

#Missing longitude_recapture
indx=which(is.na(Rlinked$Recapture_Longitude)==T)
if(length(indx)>0){Rlinked=Rlinked[-indx,]}
cat(paste0("Missing longitude_recapture: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

#Missing latitude_recapture
indx=which(is.na(Rlinked$Recapture_Latitude)==T)
if(length(indx)>0){Rlinked=Rlinked[-indx,]}
cat(paste0("Missing latitude_recapture: ",length(indx),' records removed'), file = Rcard, sep = "\n")
rm(indx)

#taxon_code_release!=taxon_code_recapture
# indx=which(Rlinked$species3ACode!=Rlinked$species3ACode)
# if(length(indx)>0){Links=Links[-indx,]}
# cat(paste0("taxon_code_release!=taxon_code_recapture: ",length(indx),' records removed'), file = Rcard, sep = "\n")
# rm(indx)
# cat("", file = Rcard, sep = "\n")
#####


cat("###Data Processing end###########################", file = Rcard, sep = "\n")
cat("", file = Rcard, sep = "\n")


#5. Management Unit with data####
# find Research blocks in which TOP were caught
Management_Units=dplyr::summarise(group_by(Catch,MU,speciesFAOCode),n=n(),.groups = "drop")
Management_Units=Management_Units%>%filter(speciesFAOCode%in%c("TOP") &
                                           (is.na(MU)==F))
Management_Units=sort(unique(c(Management_Units$MU)))

#Report if MUs don't have data or fishable area
AllRBs=RB_area_seabed$Poly
AllRBs=AllRBs[-which(AllRBs%in%c("HIMI","CI"))]
RBnodat=sort(AllRBs[AllRBs%in%Mu==F])
RBnoFA=sort(RB_area_seabed$Poly[RB_area_seabed$Area==0])

if(length(RBnodat)>0){
  cat(paste0(length(RBnodat)," RBs do not have catch data: ",paste0(RBnodat,collapse = ", "),"."), file = Rcard, sep = "\n")
  cat(paste0(length(RBnodat)," RBs do not have catch data: ",paste0(RBnodat,collapse = ", "),"."), sep = "\n")
}

if(length(RBnoFA)>0){
  cat(paste0(length(RBnoFA)," RBs do not have fishable area: ",paste0(RBnoFA,collapse = ", "),"."), file = Rcard, sep = "\n")
  cat(paste0(length(RBnoFA)," RBs do not have fishable area: ",paste0(RBnoFA,collapse = ", "),"."), sep = "\n")
}


 if(Output=="Y"){
    write.csv(Catch,"Output/Output_Catch.csv")
    write.csv(Biology,"Output/Output_LW.csv")
    write.csv(Release, "Output/Output_Rels.csv")
    write.csv(Rlinked,"Output/Output_Links.csv")
 }

rm(T_Rels,T_Recs,T_Links,T_Catch, T_Biol)

cat("Data loaded", sep = "\n")

close(Rcard)
 rm(Rcard)
#rm(Biology,Release, Recapture)
 a=gc()
 rm(a)
