est_fish_weight_rev <- function (length_weight_data, length_data) 
{
  colnames_lw= c("MU", "Season", "species3ACode", "bsLength_cm", "bsWeight_Kg", "bsSex")
  if ( !all(colnames_lw%in% names(length_weight_data))){ 
    stop(paste0("Dataframe needs to contain : "), paste0(colnames_lw))}
  
  colnames_l= c("MU", "Season", "datasetID", "opeID", "Length_release")
  if ( !all(colnames_l%in% names(length_data))){ 
    stop(paste0("Dataframe needs to contain : "),paste(colnames_l, sep=";"))}
  
  length_weight_data <- length_weight_data %>% dplyr::select(MU,Season, species3ACode, 
                                      bsLength_cm, bsWeight_Kg, bsSex) 

  length_weight_data <-  sf::st_drop_geometry(length_weight_data)
  
  names(length_weight_data) <- c("MU", "Season", "species3ACode", 
                                 "bsLength_cm", "bsWeight_Kg", "bsSex")
  
  length_data <- length_data %>% dplyr::select(MU, Season, datasetID, 
                          opeID, Length_release)
  length_data <-  sf::st_drop_geometry(length_data)
  
  names(length_data) <- c("MU", "Season", "datasetID", 
                          "opeID", "Length_release")
  
  MUs <- unique(length_weight_data$MU)
  Species <- unique(length_weight_data$species3ACode)
  length_weight_parameters <- NULL
  for (Sp in Species) {
    for (MU in MUs) {
      data_temp <- length_weight_data[length_weight_data$MU == 
                                        MU & length_weight_data$species3ACode == Sp, 
      ]
      
      if (nrow(data_temp) > 0) {
        thisLWT <- stats::lm(log(bsWeight_Kg) ~ log(bsLength_cm), 
                             data = data_temp)
        syx <- summary(thisLWT)$sigma
        cf <- exp((syx^2)/2)
        lwtIntercept_1 <- as.numeric(exp(thisLWT$coefficients["(Intercept)"]))
        lwtIntercept_2 <- as.numeric(thisLWT$coefficients["(Intercept)"])
        lwtSlope <- as.numeric(thisLWT$coefficients["log(bsLength_cm)"])
        lwtMSE <- mean((thisLWT$residuals)^2)
        length_weight_parameters <- rbind(length_weight_parameters, 
                                          cbind(Sp, MU, lwtIntercept_1, lwtIntercept_2, 
                                                lwtSlope, lwtMSE, cf))
      }
    }
  }
  length_weight_parameters <- data.frame(length_weight_parameters)
  names(length_weight_parameters) <- c("SPECIES_CODE", "MU", 
                                       "INTERCEPT1", "INTERCEPT2", "SLOPE", "MSE", "CF")
  for (Sp in Species) {
    for (MU in MUs) {
      index_releases <- length_data$MU == MU  # length_data$SPECIES_CODE == Sp #no species info in tagging export 
      index_param <- length_weight_parameters$SPECIES_CODE == 
        Sp & length_weight_parameters$MU == MU
      if (nrow(length_weight_parameters[index_param, ]) > 
          0 & nrow(length_data[index_releases, ]) > 0) {
        a <- as.numeric(as.character(length_weight_parameters$INTERCEPT2[index_param]))
        b <- as.numeric(as.character(length_weight_parameters$SLOPE[index_param]))
        cf <- as.numeric(as.character(length_weight_parameters$CF[index_param]))
        length_data$EST_WEIGHT_KG[index_releases] <- exp(a + 
                                                           b * log(length_data$Length_release[index_releases])) * 
          cf
      }
    }
  }
  length_data$EST_WEIGHT_KG
}
