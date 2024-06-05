#Standardize the continuous covariates
Cov_Std <- function(x) { (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)}

#Standardize the continuous covariates of the grid based on the observation
Cov_Std_grid <- function(x, mean_x_obs, sd_x_obs) { (x - mean_x_obs) / sd_x_obs}
