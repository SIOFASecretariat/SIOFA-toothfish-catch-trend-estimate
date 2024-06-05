quantile25 <- function (Y_Obs) {
  q25 <- quantile(Y_Obs, probs = 0.25)
  return(q25)
}

#quantile25(df_R$median_R)
