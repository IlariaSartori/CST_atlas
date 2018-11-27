library(tidyverse)
library(devtools)
# MD
get_patient_MD = function(tract) {
  patient_MD_left = sapply(tract$lhs$Streamlines, get_streamline_MD)  # Restituisce una matrice (cbind delle MD)
  patient_MD_right = sapply(tract$rhs$Streamlines, get_streamline_MD) 
  return(cbind(patient_MD_left, patient_MD_right))
}

get_streamline_MD = function(streamline) {
  return(streamline$md)
}

# AD
get_patient_AD = function(tract) {
  patient_AD_left = sapply(tract$lhs$Streamlines, get_streamline_AD)  # Restituisce una matrice (cbind delle MD)
  patient_AD_right = sapply(tract$rhs$Streamlines, get_streamline_AD) 
  return(cbind(patient_AD_left, patient_AD_right))
}

get_streamline_AD = function(streamline) {
  return(streamline$ad)
}

# RD
get_patient_RD = function(tract) {
  patient_RD_left = sapply(tract$lhs$Streamlines, get_streamline_RD)  # Restituisce una matrice (cbind delle MD)
  patient_RD_right = sapply(tract$rhs$Streamlines, get_streamline_RD) 
  return(cbind(patient_RD_left, patient_RD_right))
}

get_streamline_RD = function(streamline) {
  return(streamline$rd)
}

# FA
get_patient_FA = function(tract) {
  patient_FA_left = sapply(tract$lhs$Streamlines, get_streamline_FA)  # Restituisce una matrice (cbind delle MD)
  patient_FA_right = sapply(tract$rhs$Streamlines, get_streamline_FA) 
  return(cbind(patient_FA_left, patient_FA_right))
}

get_streamline_FA = function(streamline) {
  return(streamline$fa)
}


setwd("/Users/ILARIASARTORI/Politecnico di Milano/Luca Torriani - Project StatApp/RData")
load("cst_list.RData")

#### MD
tract_tot_MD_list = map(cst_list, get_patient_MD)
tract_tot_MD = do.call(cbind.data.frame, tract_tot_MD_list)

tract_tot_MD=t(tract_tot_MD)
save(tract_tot_MD, file="tract_tot_MD.RData")

#### AD
tract_tot_AD_list = map(cst_list, get_patient_AD)
tract_tot_AD = do.call(cbind.data.frame, tract_tot_AD_list)

tract_tot_AD=t(tract_tot_AD)
save(tract_tot_AD, file="tract_tot_AD.RData")


#### RD
tract_tot_RD_list = map(cst_list, get_patient_RD)
tract_tot_RD = do.call(cbind.data.frame, tract_tot_RD_list)

tract_tot_RD=t(tract_tot_RD)
save(tract_tot_RD, file="tract_tot_RD.RData")
