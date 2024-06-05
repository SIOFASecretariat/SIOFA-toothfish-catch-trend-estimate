#Script to run CPUE-by-seabed area analogy sensitivity analysis 
#Launch 01_LoadData

# 1. Fishable area and ref area sensibility 
# 2. 

#Load Data###################
Catch= read.csv( "Output/Output_Catch.csv")

Catch$Catch =  rowSums(cbind(Catch$Catch_Kg , Catch$Weigth),na.rm=T) 

Catch = Catch %>% mutate(CPUE= Catch/HooksSet) 

# Run sensitivity analysis -------------------------------------------------------

#Set biomass and CV for Reference Areas
HIMI_biomass_est=31111 
HIMI_CV_biomass_est=0.0281

CI_biomass_est=11000 #to update ! 
CI_CV_biomass_est=0.0581 # to update!

#set cpue calculation - if 'dist' kg/km line either kg/ham
CPUE_mod = 'hook' #'dist' 
if (CPUE_mod=='dist'){
  HIMI_cpue_est=0.294
  HIMI_CV_cpue_est=0.1
  CI_cpue_est=0.245
  CI_CV_cpue_est=0 #0.1
} else{
  HIMI_cpue_est=0.210
  HIMI_CV_cpue_est=0.1
  CI_cpue_est=0.175
  CI_CV_cpue_est=0#0.1
}


#List MU in the proper order
Mu=c("DC", "SIR")

#Ref area
Ref_area= c("HIMI","CI") # ifelse(RB%in%TOP_target_RBs,"HIMI","RSR_open")

#Set number of bootstrap iterations
n_boot=10000
Est_Season = 2023-1
Min_Season=2019

#Get the unprojected GEBCO data
B=rast("C:/Users/jules/Dropbox/PostDoc_MNHN/RworkingDirectory/Rproject_Lobster/Data/Gebco_Bathymetry/gebco_2022_n-25.5762_s-60.0293_w37.2656_e104.2383.tif")

#Fishable area 
PolysLL=st_read(dsn=path.expand(paste0(getwd(),'/Data')), layer="PolysLL", quiet = TRUE)
minDepth = c(600, 600)
maxDepth = c(1800, 2000)
#Loop over polygons 
RawAr=data.frame(Poly=character(), Area=numeric(), min_depth=numeric(), max_depth=numeric()) #Prepare empty output

for (i in 1:length(PolysLL$name)){
  for (j in 1:length(minDepth)){
    #Take one polygon
    pol=PolysLL[i,]
    #Get its name
    pname=PolysLL$name[i]
    #Take bathymetry data that matches the extent of the polygon
    Btmp=crop(B,ext(pol))
    #Turn GEBCO cells that are not inside the polygon into NAs
    Btmp=terra::mask(Btmp,pol)
    #Turn cells outside the fishable depth into NAs
    Btmp.area = classify(Btmp, cbind(-100000, -maxDepth[j], NA), right=TRUE)
    Btmp.area = classify(Btmp.area, cbind(-minDepth[j], 100000, NA), right=FALSE)
    #Compute the area covered by cells that are not NA
    Ar=round(expanse(Btmp.area, unit="km"),2)
    #Store result
    RawAr=rbind(RawAr,data.frame(Poly=pname,Layer = j, Area=Ar$area, min_depth= minDepth[j], max_depth=maxDepth[j]))
    
  }
}

##
df_res = data.frame(MU=character(), AreaMU=numeric(),REF=character(), AreaREF=numeric(), min_depth=numeric(), 
                    max_depth=numeric(), B=numeric())#Prepare empty output

for (RB in Mu){
  for (RF in Ref_area){
    for (d in 1:length(minDepth)){
      dmin =minDepth[d]
      dmax= maxDepth[d]
      Seasons=  Est_Season
      # Get RB Seabed Area
      Seabed_RB=RawAr$Area[RawAr$Poly==RB & RawAr$max_depth==dmax]
      # Get RefArea Seabed Area
      Seabed_RA=RawAr$Area[RawAr$Poly==RF & RawAr$max_depth==dmax]
    
      # Restrict Catch data to the relevant Management Unit (on either end of the line)   
      Catch_RB_CPUE=Catch[which(Catch$MU==RB),]

      # Format catch data for input into biomass estimate function
      Catch_RB_CPUE=Catch_RB_CPUE[,c("Season","opeID","speciesFAOCode",
                               "Catch_Kg", "HooksSet")] # "line_length"

      Haul_RB_CPUE =Catch_RB_CPUE %>% filter(Season%in%Seasons) %>% mutate(CPUE=Catch_Kg/HooksSet) %>% 
            pull(CPUE)

      # Set biomass and CV for Ref_area
      Ref_area_biomass=ifelse(RF=="HIMI",HIMI_biomass_est,CI_biomass_est)
      Ref_area_CVs=ifelse(RF=="HIMI",HIMI_CV_biomass_est,CI_CV_biomass_est)

      # CPUE - Set median for Ref_area
      Ref_area_cpue=ifelse(RF=="HIMI",HIMI_cpue_est,CI_cpue_est)
      Ref_area_cpue_CVs=ifelse(RF=="HIMI",HIMI_CV_cpue_est,CI_CV_cpue_est)

      #Run CPUE Seabed area analogy
      CPUE_Seabed=CPUE_seabed_rev_cvCPUE(
                            fish_CPUE_data=Haul_RB_CPUE,
                            fish_area=Seabed_RB,     
                            ref_area=Seabed_RA,
                            ref_bio=Ref_area_biomass,
                            ref_bio_cv=Ref_area_CVs,
                            ref_cpue=Ref_area_cpue,
                            ref_cpue_cv=Ref_area_cpue_CVs
                            
                            )
      Boot_CPUE=cpue_bootstrap.cpue_area_rev(CPUE_Seabed,nboot=10000)
      
      
      df_res=rbind(df_res, data.frame(MU=RB, AreaMU=Seabed_RB, REF=RF, AreaREF=Seabed_RA, 
                                      min_depth=dmin,max_depth=dmax, B=summary(Boot_CPUE)["Est"]))
}
}
}

df_res
