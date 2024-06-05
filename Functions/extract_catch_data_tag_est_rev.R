extract_catch_data_tag_est_rev = function (catch_data, release_data, recapture_data, release_seasons = NULL, 
          catch_season = NULL, measure, mean_fish_weight = NULL, target_species) 
{
  
  names(catch_data) <- c("Season",  "opeID", 
                         "speciesFAOCode", "Catch")
  if (measure == "counts") {
    release_data$MEASURE <- rep(1, nrow(release_data))
  }
  names(release_data) <- c("Season", "opeID", "Measure")
  names(recapture_data) <- c("Release_Season", "Recapture_Season", 
                             "opeID")
  
  if (ncol(catch_data) != 4) 
    stop("catch_data table should have 4 columns check that all necessary information has been provided")
  if ((ncol(release_data) != 3)) 
    stop("release_data table should have 3 columns, check all necessary information has been provided")
  if ((ncol(recapture_data) != 3)) 
    stop("recapture_data table should have 3 columns, check all necessary information has been provided")
  if (measure == "counts" & is.null(mean_fish_weight)) 
    stop("a mean fish weight has not been entered, but is required for fish count measurements")
  if (!is.null(release_seasons)) {
    recapture_data <- recapture_data[recapture_data$Release_Season %in% 
                                       release_seasons, ]
  }
  else {
    release_seasons <- unique(release_data$Season)
  }
  if (!is.null(catch_season)) {
    recapture_data <- recapture_data[recapture_data$Recapture_Season %in% 
                                       catch_season, ]
    catch_data <- catch_data[catch_data$Season %in% catch_season,]
    release_data <- release_data[release_data$Season %in% 
                                   catch_season, ]
  }
  
  recapture_data <- recapture_data[recapture_data$Recapture_Season != 
                                     recapture_data$Release_Season , ]
  
  N_recaps_haul_by_haul <- stats::aggregate( Recapture_Season ~ 
                                               opeID + Release_Season, data = recapture_data, 
                                            length)
  
  names(N_recaps_haul_by_haul) <- c("opeID", "Release_Season", "N_recap")
  catch_recap_data <- left_join(catch_data, N_recaps_haul_by_haul, 
                                 by = c("opeID"))
  
  if (nrow(release_data) > 0) {
    
    Weight_releases_haul_by_haul <- stats::aggregate(Measure ~ 
                                                       opeID + Season, data = release_data, 
                                                     sum)
    names(Weight_releases_haul_by_haul) <- c("opeID",  "Season", "Release")
    
    catch_recap_release_data <- plyr::join(catch_recap_data, 
                                           Weight_releases_haul_by_haul, by = c("opeID"))
    
    catch_recap_release_data$Release[is.na(catch_recap_release_data$Release)] <- 0
    catch_recap_release_data$Catch[is.na(catch_recap_release_data$Catch)] <- 0
    
    if (measure == "counts") {
      catch_recap_release_data$Catch <- catch_recap_release_data$Catch + 
        (catch_recap_release_data$Release * mean_fish_weight)
    }
    else {
      catch_recap_release_data$Catch <- catch_recap_release_data$Catch # +  catch_recap_release_data$Release  // already corrected
    }
    catch_recap_release_data$N_recap[is.na(catch_recap_release_data$N_recap)] <- 0
    catch_recap_release_data$Catch[is.na(catch_recap_release_data$Catch)] <- 0
  }
  else {
    catch_recap_release_data <- catch_recap_data
  }
  
  #catch_recap_release_data$Catch[] <- 0
  #catch_recap_release_data$N_recap[] <- 0
  duplicate_hauls <- catch_recap_release_data[duplicated(catch_recap_release_data$opeID) | 
                                                 duplicated(catch_recap_release_data$opeID, fromLast = TRUE), 
  ]
   
  catch_recap_release_data <- catch_recap_release_data[!(duplicated(catch_recap_release_data$opeID) | 
                                                            duplicated(catch_recap_release_data$opeID, fromLast = TRUE)), 
  ]
  

  catch_recap_data <- catch_recap_release_data[, names(catch_recap_release_data) %in% 
                                                 c("Catch","Season" ,"Release_Season", "N_recap", 
                                                    "opeID")]
  
  catch_recap_data <- reshape2::dcast(catch_recap_data, opeID  + Catch ~ Release_Season, value.var = "N_recap", 
                            drop = TRUE, fun.aggregate = sum)
  
  catch_recap_data <- catch_recap_data[, !names(catch_recap_data) %in% 
                                         c("NA", "opeID")]
  
  catch_recap_data[is.na(catch_recap_data)] <- 0
  catch_recap_output <- data.frame(matrix(0, nrow = nrow(catch_recap_data), 
                                          ncol = length(release_seasons) + 1))
  names(catch_recap_output) <- c(names(catch_recap_data)[1], 
                                 release_seasons)
  catch_recap_output[, names(catch_recap_output) %in% names(catch_recap_data)] <- catch_recap_data
  catch_recap_output
}