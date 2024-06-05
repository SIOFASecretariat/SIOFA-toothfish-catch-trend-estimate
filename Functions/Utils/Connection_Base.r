###########################################
# Fonction de connection PECHEKER
# Jules Selles
###########################################

Connection.Pecheker<-function(Pecheker_last_version=F,id='jselles', pw='jules'){
  
  # Prerecquis
  # Connection au reseau ou par VPN
  
  # Input -------------------------------------------------------------------
  # @Pecheker_last_version: module pour migration de PECHEKER sur Lbspred, pour le moment utiliser, =FALSE
  # @id: Identifiant de connection PECHEKER
  # @pw: Mot de passe de connection PECHEKER
  
  # Output ------------------------------------------------------------------
  # <JDBCConnection> Object
  
  # library
  lapply(c("RJDBC","rJava","askpass"), require, character.only = TRUE)
  
  if(is.na(id)){
    id <- askpass("Please enter your ID")
  }

  if(is.na(pw)){
    pw <- askpass("Please enter your password")
  }

if(Pecheker_last_version == T){
  driver <- JDBC("oracle.jdbc.OracleDriver",
                 classPath= paste0(getwd(),"/ojdbc6.jar"))
  db_Pecheker <- dbConnect(driver,
                           "jdbc:oracle:thin:@//ora-labosprd.drzp.mnhn.fr:1251/labosprd",
                           id,pw)
} else {
  driver <- JDBC("oracle.jdbc.OracleDriver",
                 classPath= paste0(getwd(),"/ojdbc6.jar"))
  db_Pecheker <- dbConnect(driver,
                           "jdbc:oracle:thin:@//dsiracscan.mnhn.fr:1521/S_EGB",
                           id,pw)
}

return(db_Pecheker)  
}


