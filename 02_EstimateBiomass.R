#-------------------------------------------------------------------------------
## Estimate biomass
#-------------------------------------------------------------------------------
cat("CPUE start", sep = "\n")

#to do/check :
#1. rev extract_catch_data_cpue_est function

# Estimate Biomass using CPUE####
store_biomass_estimate=NULL
for(RA in RefArea){
  cat(" || ");cat(RA);cat(" : ")
  
for (RB in Mu){
  cat(RB);cat(" | ")
  #1. RB data
  
  # Restrict Release data to relevant management unit 
  Rel_RB_CPUE=Release[which(Release$MU==RB),]
  
  # Format Release data for input into biomass estimate function
  Rel_RB_CPUE=Rel_RB_CPUE[,c("Season","obs_foID","opeID","Weight_release")]
  
  # Restrict Catch data to the relevant Management Unit (on either end of the line)   
  Catch_RB_CPUE=Catch[which(Catch$MU==RB),]
  
  # Format catch data for input into biomass estimate function
  Catch_RB_CPUE=Catch_RB_CPUE[,c("Season","opeID","speciesFAOCode",
                                 "Catch_Kg", "Weigth","HooksSet",
                                 "fishopSetStartLongitude","fishopSetStartLatitude",
                                 'fishopSetEndLongitude','fishopSetEndLatitude')] # "line_length"
  
  ############ Set Ref_area 
  #Ref_area= RA # ifelse(RB%in%TOP_target_RBs,"HIMI","RSR_open")
  
  # Set biomass and CV for Ref_area
  Ref_area_biomass=ifelse(RA=="HIMI",HIMI_biomass_est,CI_biomass_est)
  Ref_area_CVs=ifelse(RA=="HIMI",HIMI_CV_biomass_est,CI_CV_biomass_est)
  
  # CPUE - Set median for Ref_area
  Ref_area_cpue=ifelse(RA=="HIMI",HIMI_cpue_est,CI_cpue_est)
  Ref_area_cpue_CVs=ifelse(RA=="HIMI",HIMI_CV_cpue_est,CI_CV_cpue_est)
  
  #2. RefArea data
  #Restrict Release data to relevant Area
  #Rel_RA_CPUE=Release[which(Release==Ref_area),]
  
  # Format Release data for input into biomass estimate function
  #Rel_RA_CPUE=Rel_RA_CPUE[,c("Season","obs_foID","opeID","Weight_release")]
  
  # Restrict Catch data to relevant management unit 
  # Catch_RA_CPUE=Catch[which(Catch$MU==Ref_area
  #                           |Catch$MU==Ref_area),]
  # 
  # # Format Catch data for input into biomass estimate function
  # Catch_RA_CPUE=Catch_RA_CPUE[,c("c2_id","season_ccamlr","obs_logbook_id","haul_number","taxon_code",
  #                                "greenweight_caught_kg","line_length_m")]
  
  # Initialize dataframe to store estimates
  Survey_est=sort(unique(Catch_RB_CPUE$Season))
  
  #Initialize dataframe to store estimates
  store_annual_estimates=data.frame(matrix(0,nrow=length(Survey_est),ncol=11))
  names(store_annual_estimates)=c('RB','Species','Season','Ref_area','RefArea_seabed_area','RB_seabedarea','RB_N_Hauls',
                                  'RB_N_Hooks','Est','CI_lower','CI_upper')
  store_annual_estimates$Season=Survey_est
  
  # Specifiy target species
  target_species<-"TOP"#ifelse(RB%in%TOP_target_RBs,"TOP","TOA")
  
  #Loop to get annual estimates
  for (y in Survey_est){
    
    # select the last three seasons, prior to and including y, in which fishing occurred 
    Seasons=tail(Survey_est[Survey_est<=y],3)
    
    #run extract_catch_data_cpue_est on RB data
    Catch_RB_CPUE$Catch =  rowSums(cbind(Catch_RB_CPUE$Catch_Kg , Catch_RB_CPUE$Weigth),na.rm=T)
    
    Catch_RB_CPUE$DistLL = 
      sapply(1:nrow(Catch_RB_CPUE),function(i)
        distGeo(as.data.frame(Catch_RB_CPUE)[i,c('fishopSetStartLongitude','fishopSetStartLatitude')],
                as.data.frame(Catch_RB_CPUE)[i,c('fishopSetEndLongitude','fishopSetEndLatitude')]))
    
    if(CPUE_mod == 'dist'){
      Haul_RB_CPUE = Catch_RB_CPUE %>% filter(Season%in%Seasons) %>% 
        mutate(CPUE= Catch / (DistLL*10^-3)) %>% 
        pull(CPUE)
    }else{
      Haul_RB_CPUE = Catch_RB_CPUE %>% filter(Season%in%Seasons) %>% 
        mutate(CPUE= Catch /HooksSet) %>% 
        pull(CPUE)
    }
    
    Nbhook_RB_CPUE =Catch_RB_CPUE %>% filter(Season%in%y) %>% dplyr::summarise(HooksSet=sum(HooksSet)) %>% 
      pull(HooksSet)
    
    #replace by predict from std model ## to check 
    
    #Package BERT
    # Haul_RB_CPUE=extract_catch_data_cpue_est(catch_data=Catch_RB_CPUE,
    #                                          release_data=Rel_RB_CPUE,
    #                                          measure ="weights",
    #                                          target_species=target_species,
    #                                          catch_season=Seasons)
    
    
    #get hauls in last 3 seasons in the Reference Areas that match the latest assessment period
    if(RA=="HIMI"){Ref_area_seasons=HIMI_Ass_seasons}
    if(RA=="CI"){Ref_area_seasons=CI_Ass_seasons}else{Ref_area_seasons=HIMI_Ass_seasons}
    
    # Haul_RA_CPUE= HIMI_cpue_est
    #run extract_catch_data_cpue_est on RefArea data
    # Haul_RA_CPUE=extract_catch_data_cpue_est(catch_data=Catch_RA_CPUE,
    #                                          release_data=Rel_RA_CPUE,
    #                                          measure="weights",
    #                                          target_species=target_species,
    #                                          catch_season=Ref_area_seasons)
    
    # Get RB Seabed Area
    Seabed_RB=RB_area_seabed$Area[RB_area_seabed$Poly==RB]
    # Get RefArea Seabed Area
    if(RA=="HIMI"){
      Seabed_RA=Ref_area_seabed$Area[Ref_area_seabed$Poly=="HIMI"]
    }else if(RA=='CI') {
      Seabed_RA=Ref_area_seabed$Area[Ref_area_seabed$Poly=="CI"]
    } else {
      Seabed_RA=Ref_area_seabed$Area[Ref_area_seabed$Poly=="HIMI"]
    }
    
    #Run CPUE Seabed area analogy
    CPUE_Seabed=
      CPUE_seabed_rev_cvCPUE(
        fish_CPUE_data=Haul_RB_CPUE,
        fish_area=Seabed_RB,     
        ref_area=Seabed_RA,
        ref_bio=Ref_area_biomass,
        ref_bio_cv=Ref_area_CVs,
        ref_cpue=Ref_area_cpue,
        ref_cpue_cv=Ref_area_cpue_CVs
      )
    
    #Bootstrap CPUE
    Boot_CPUE=cpue_bootstrap.cpue_area_rev(CPUE_Seabed,n_boot)
    
    store_annual_estimates$RB[store_annual_estimates$Season==y] = RB
    store_annual_estimates$Species[store_annual_estimates$Season==y] = as.character(target_species)
    store_annual_estimates$Ref_area[store_annual_estimates$Season==y] = RA
    store_annual_estimates$RefArea_seabed_area[store_annual_estimates$Season==y] = Seabed_RA
    store_annual_estimates$RefArea_biomass[store_annual_estimates$Season==y] = Ref_area_biomass
    #store_annual_estimates$RefArea_N_Hauls[store_annual_estimates$Season==y] =length(Haul_RA_CPUE)
    store_annual_estimates$RefArea_CPUE[store_annual_estimates$Season==y] = as.numeric(CPUE_Seabed$data["ref_cpue"])
    store_annual_estimates$RB_seabedarea[store_annual_estimates$Season==y] = Seabed_RB
    store_annual_estimates$RB_N_Hauls[store_annual_estimates$Season==y] = length(Haul_RB_CPUE)
    store_annual_estimates$RB_N_Hooks[store_annual_estimates$Season==y] = Nbhook_RB_CPUE
    store_annual_estimates$RB_CPUE[store_annual_estimates$Season==y] = as.numeric(CPUE_Seabed$data["fish_CPUE_est"])
    store_annual_estimates$Est[store_annual_estimates$Season==y] = summary(Boot_CPUE)["Est"]
    store_annual_estimates$CI_lower[store_annual_estimates$Season==y] = summary(Boot_CPUE)["2.5%"]
    store_annual_estimates$CI_upper[store_annual_estimates$Season==y] = summary(Boot_CPUE)["97.5%"]
    store_annual_estimates$sd[store_annual_estimates$Season==y] = sd(Boot_CPUE$Boot_estimates)
  }
  store_biomass_estimate=rbind(store_biomass_estimate,store_annual_estimates)
  
}
}


write.csv(store_biomass_estimate,paste0("Output/Output_CPUE_3yMedian_",Time,".csv"),row.names = FALSE)

store_biomass_estimates_CPUE=store_biomass_estimate[,c('RB','Species','Ref_area','Season','Est','CI_lower','CI_upper','RB_N_Hauls','RB_N_Hooks')]
store_biomass_estimates_CPUE$Method="CPUE-by-seabed area"

rm(RB); rm(RA)
rm(store_biomass_estimate,Ref_area_biomass,Ref_area_CVs,
   Survey_est,y,Seasons,Ref_area_seasons,Boot_CPUE,store_annual_estimates)
rm(Rel_RB_CPUE,Catch_RB_CPUE,Haul_RB_CPUE,Seabed_RB,Seabed_RA,CPUE_Seabed)

cat("", sep = "\n")
cat("CPUE end", sep = "\n")



#####
# Estimate local biomass using the Chapman mark-recapture method####

cat("Chapman start", sep = "\n")

#Remove emigrants
Release=Release%>%filter(!tagrelID%in%Migrants$Release_TagID)
rm(Migrants,RelEm)

cat("\n")

# Management_Unitss with at least one recapture in any survey year
Management_Units=unique(Rlinked$Recapture_MU)

store_biomass_estimate_chapman=NULL

#for(RA in RefArea){
#  cat(" || ");cat(RA);cat(" : ")
  
for (RB in Management_Units){
  cat(RB);cat(" | ")
  # Set target species
  target_species<-"TOP"#ifelse(RB%in%TOP_target_RBs,"TOP","TOA")
  
  # Restrict Recapture data to relevant management unit 
  Rec_RB_CHAP=Rlinked[which(Rlinked$Release_MU==RB
                          & Rlinked$Recapture_MU==RB),]
  
  # all other RBs recaptures are limited within 3 years of release
  Rec_RB_CHAP=Rec_RB_CHAP[which(Rec_RB_CHAP$Recapture_Season-Rec_RB_CHAP$Release_Season<=3),]
  
  # Restrict Release data to relevant management unit 
  Rel_RB_CHAP=Release[which(Release$MU==RB),]
  
  # Restrict Catch data to the relevant management unit  (on either end of the line)   
  Catch_RB_CHAP=Catch[which(Catch$MU==RB),]
  
  #Release catch correction 
  Catch_RB_CHAP$Catch =  rowSums(cbind(Catch_RB_CHAP$Catch_Kg , Catch_RB_CHAP$Weigth),na.rm=T)
  
  # Get Survey Years by releases
  Survey_years=seq(min(Rel_RB_CHAP$Season,na.rm=T),max(Rel_RB_CHAP$Season,na.rm=T))
  
  #Subset Catch data
  Catch_Effort_RB_CHAP=Catch_RB_CHAP[,c("Season","opeID","speciesFAOCode","Catch","HooksSet")]
  Catch_RB_CHAP=Catch_RB_CHAP[,c("Season","opeID","speciesFAOCode","Catch")]
  
  # Catch_RB_CHAP=Catch_RB_CHAP[which(is.na(Catch_RB_CHAP$obs_logbook_id)==F),] #Remove records that have not been linked yet due to data loading issues
  #Subset Release data
  Rel_RB_CHAP=Rel_RB_CHAP[,c("Season", "opeID","Est_weigth_release")]
  # colnames(Rel_RB_CHAP)[colnames(Rel_RB_CHAP)=='SEASON_New']='SEASON'
  
  #Subset Recapture data
  Rec_RB_CHAP=Rec_RB_CHAP[,c("Release_Season","Recapture_Season",
                             "opeID")] # "haul_number_recapture", "taxon_code_recapture"
  
  
  # for data that is input into tag release and recap matrix remove non-target species
   Tag_RB_CHAP=extract_recaptures_season(
     release_data=Rel_RB_CHAP$Season[],
     recapture_data=cbind(Rec_RB_CHAP$Release_Season[],
                          Rec_RB_CHAP$Recapture_Season[]),
     release_seasons=Survey_years,
     within_season_recaps = T)
  
  # No estimates in years where there are zero recaptures 
  Survey_est=sort(unique(Rec_RB_CHAP$Recapture_Season,na.rm=T))
  
  #Initialize dataframe to store estimates
  store_annual_estimates<-data.frame(matrix(0,nrow=length(Survey_est),ncol=10))
  names(store_annual_estimates)=c("Species","RB","Season","N_recaptures","Total_Catch","RB_N_Hauls","Avail_tags", "Est","CI_lower","CI_upper") 
  store_annual_estimates$Species=rep(target_species,length(Survey_est))
  store_annual_estimates$RB=rep(RB,length(Survey_est))
  #store_annual_estimates$RA=rep(RA,length(Survey_est))
  store_annual_estimates$Season=Survey_est
  
  #Loop over Survey est
  for (y in Survey_est){
    
    Season_releases=seq(min(Survey_years),y,1)
    
    # Only 1 year for RBs_1YearAtLiberty
    #if(RB%in%RBs_1YearAtLiberty & length(Season_releases)>1){
      # 1 year of releases are included as the 1 year prior to the current season (y) are used in calculating the
      #tagged fish available for recapture
     # Season_releases=seq(y-1,y,1)}else{
    #    if(length(Season_releases)>3){
          # 4 years of releases are included as the 3 years prior to the current season (y) are used in calculating the
          # tagged fish available for recapture
         # Season_releases=seq(y-3,y,1)
     #   }
    #  }
    
    if(min(Tag_RB_CHAP$Year)>=y-3){
      Season_releases=seq(y-1,y,1)
    }else{
      Season_releases=seq(y-3,y,1)
    }
          
    # Nbhooks 
    Nbhook_RB_CHAP =Catch_Effort_RB_CHAP %>% filter(Season%in%y) %>% dplyr::summarise(HooksSet=sum(HooksSet)) %>% 
      pull(HooksSet)
    
    ## expanded tag_parameters
    tag_pars=tag_pars
   
    tag_pars[["tag_mort"]]=rep(tag_pars[["tag_mort"]],length(Season_releases))
    tag_pars[["reporting"]]=rep(tag_pars[["reporting"]],length(Season_releases))
    tag_pars[["nat_mort"]]=rep(tag_pars[["nat_mort"]],length(Season_releases))
    tag_pars[["chronic_shed"]]=rep(tag_pars[["chronic_shed"]],length(Season_releases))
    tag_pars[["chronic_mort"]]=rep(tag_pars[["chronic_mort"]],length(Season_releases))
    
    Haul_RB_CHAP=extract_catch_data_tag_est_rev(catch_data=Catch_RB_CHAP,
                                            release_data=Rel_RB_CHAP,
                                            recapture_data=Rec_RB_CHAP,
                                            measure="weights",
                                            target_species=target_species,
                                            release_seasons=Season_releases,
                                            catch_season=y)
  
    tags=Tag_RB_CHAP[match(min(Season_releases),Tag_RB_CHAP$Year):(match(max(Season_releases),Tag_RB_CHAP$Year)-1),]
    tags=tags[,names(tags)%in%c("Releases",Season_releases)]
    # remove survey year col from the matrix for input into tagr multi release function
    pars=tag_pars
    
    if(length(Season_releases)>2){
      
      ## now run the model
      PopSize=multi_release(tags,hauls=Haul_RB_CHAP,pars=tag_pars)
      Boot_TAG <- tag_bootstrap(PopSize,n_boot,boot_zeroes=TRUE)
      # Boot_TAG <- tag_bootstrap_mrelease(PopSize,n_boot,boot_zeroes=TRUE)
      
      store_annual_estimates$N_recaptures[store_annual_estimates$Season==y]=sum(tags[,ncol(tags)])
      store_annual_estimates$Avail_tags[store_annual_estimates$Season==y]=sum(PopSize$Avail_tags[,ncol(PopSize$Avail_tags)])
      store_annual_estimates$Total_Catch[store_annual_estimates$Season==y]=sum(Haul_RB_CHAP[,1])/1e3
      store_annual_estimates$RB_N_Hauls[store_annual_estimates$Season==y]=nrow(Haul_RB_CHAP)   
      store_annual_estimates$Est[store_annual_estimates$Season==y]=summary(Boot_TAG)[["Combined"]]/1e3
      store_annual_estimates$CI_lower[store_annual_estimates$Season==y]=summary(Boot_TAG)[["2.5%"]]/1e3  
      store_annual_estimates$CI_upper[store_annual_estimates$Season==y]=summary(Boot_TAG)[["97.5%"]]/1e3 
      store_annual_estimates$sd[store_annual_estimates$Season==y]=sd(Boot_TAG$Boot_estimates$boot_est,na.rm=T)/1e3
    }else{
      PopSize=single_release(tags=as.numeric(tags[1]),
                             catch=Haul_RB_CHAP[,1],
                             recaps=Haul_RB_CHAP[,ncol(Haul_RB_CHAP)-1],
                             method=tag_pars$method,
                             unit=tag_pars$unit,
                             type=tag_pars$type,
                             tag_mort=tag_pars$tag_mort[1],
                             reporting=tag_pars$reporting[1],
                             nat_mort=tag_pars$nat_mort[1],
                             chronic_shed=tag_pars$chronic_shed[1],
                             chronic_mort=tag_pars$chronic_mort[1])
      
      Boot_TAG=tag_bootstrap(PopSize,n_boot,boot_zeroes=TRUE)
      # Boot_TAG=tag_bootstrap_srelease(PopSize,n_boot,boot_zeroes=TRUE)
      
      store_annual_estimates$N_recaptures[store_annual_estimates$Season==y]=sum(Haul_RB_CHAP[,ncol(Haul_RB_CHAP)-1])
      store_annual_estimates$Avail_tags[store_annual_estimates$Season==y]=PopSize$TagsAvailable
      store_annual_estimates$Total_Catch[store_annual_estimates$Season==y]=sum(Haul_RB_CHAP[,1])/1e3
      store_annual_estimates$RB_N_Hauls[store_annual_estimates$Season==y]=nrow(Haul_RB_CHAP)
      store_annual_estimates$RB_N_Hooks[store_annual_estimates$Season==y]=Nbhook_RB_CHAP
      store_annual_estimates$Est[store_annual_estimates$Season==y]=summary(Boot_TAG)[["N_hat"]]/1e3
      store_annual_estimates$CI_lower[store_annual_estimates$Season==y]=summary(Boot_TAG)[["2.5%"]]/1e3
      store_annual_estimates$CI_upper[store_annual_estimates$Season==y]=summary(Boot_TAG)[["97.5%"]]/1e3
      store_annual_estimates$sd[store_annual_estimates$Season==y]=sd(Boot_TAG$Boot_estimates$boot_est,na.rm=T)/1e3
    }
    
  }

  
  store_biomass_estimate_chapman=rbind(store_biomass_estimate_chapman,store_annual_estimates)
  
}


  rm(RB,target_species,Rec_RB_CHAP,Rel_RB_CHAP,Catch_RB_CHAP,Survey_years)
  rm(Tag_RB_CHAP,Survey_est,y,Season_releases,tag_pars,Haul_RB_CHAP,tags,pars,PopSize,Boot_TAG)
  
  # store Chapman estimates with inputs
  write.csv(store_biomass_estimate_chapman,paste0("Output/Output_Chapman_",Time,".csv"),row.names = FALSE)
  
  
  store_biomass_estimates_chapman=subset(store_biomass_estimate_chapman,
                                         select=c(RB,Species,Season,Est,CI_lower,CI_upper,N_recaptures,RB_N_Hauls))
  store_biomass_estimates_chapman$Method="Chapman"
  
  store_B_est_all=rbind(data.frame(store_biomass_estimates_chapman,RB_N_Hooks=NA, Ref_area=NA),
                        data.frame(store_biomass_estimates_CPUE,N_recaptures=NA))
  
  write.csv(store_B_est_all,paste0("Output/Output_Bestimates_",Time,".csv"),row.names = FALSE)
  
  
  store_B_est_most_recent <- ddply(store_B_est_all%>% filter(Ref_area==RefArea.selected | is.na(Ref_area)) ,
                                   .(Species,Method,RB),function(x){x[which.max(x$Season),]},.drop=FALSE)
  # remove hauls and catch limit 
  store_B_est_most_recent=subset(store_B_est_most_recent, select=-RB_N_Hauls)
  store_B_est_most_recent=store_B_est_most_recent[order(store_B_est_most_recent$RB),]
  store_B_est_most_recent$Catch_lim=store_B_est_most_recent$Est*0.04
  
  row.names(store_B_est_most_recent)=NULL
  write.csv(store_B_est_most_recent,paste0("Output/Output_Recent_Bestimates_",Time,".csv"),row.names = FALSE)
  
  cat("", sep = "\n")
  cat("Chapman end", sep = "\n")
  
  
  #Check Flinks
  LcountsChap=store_biomass_estimate_chapman[store_biomass_estimate_chapman$Season>=(maxSeason-4),c("RB","Season","N_recaptures")]
  LcountsChap=arrange(LcountsChap,RB,Season)
  
  chk=sum(Lcounts$n - LcountsChap$N_recaptures)
  
  if(chk!=0){warning(
    "###########################################
###########################################
Check Links filtering when producing Flinks
###########################################
###########################################"
  )}
  
  rm(store_annual_estimates)
  rm(store_biomass_estimate_chapman,store_biomass_estimates_chapman,store_biomass_estimates_CPUE)
  rm(n_boot)
  #####
  