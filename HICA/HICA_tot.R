get_HICA_loadings <- function (dataset, K, title, coda = FALSE)
{
  
  if (coda)
  {
    dataset = dataset[,26:50]
    lev = 24
  }
  
  else 
  {
    lev = 49
  }
  
  
  library(fastHICA)
  
  basis <- basis_hica(as.matrix(dataset))
  energy <- energy_hica(basis, maxcomp = K)
  hica <- extract_hica(energy, comp = K, level=lev)
  
  
  hica.load <- hica$C
  rownames(hica.load) = as.character(c(1:50))
  
  # x11()
  quartz()
  par(mar = c(1,4,0,2), oma=c(0,0,3,0), mfrow = c(K,1))
  for(i in 1:K)
  {
    barplot(hica.load[,i], ylim = c(-1, 1), col=rainbow(lev+1))
    abline(h=0)
  }
  title(paste("HICA for ", title), outer = T)  
  
}

library(tidyverse)
library(devtools)
# MD
get_patient_MD = function(tract) {
  patient_MD = sapply(tract$data, get_streamline_MD)  # Restituisce una matrice (cbind delle MD)
  return(patient_MD)
}

get_streamline_MD = function(streamline) {
  return(streamline$MD)
}

# AD
get_patient_AD = function(tract) {
  patient_AD = sapply(tract$data, get_streamline_AD)  # Restituisce una matrice (cbind delle AD)
  return(patient_AD)
}

get_streamline_AD = function(streamline) {
  return(streamline$AD)
}

# RD
get_patient_RD = function(tract) {
  patient_RD = sapply(tract$data, get_streamline_RD)  # Restituisce una matrice (cbind delle RD)
  return(patient_RD)
}

get_streamline_RD = function(streamline) {
  return(streamline$RD)
}

# FA
get_patient_FA = function(tract) {
  patient_FA = sapply(tract$data, get_streamline_FA)  # Restituisce una matrice (cbind delle FA)
  return(patient_FA)
}

get_streamline_FA = function(streamline) {
  return(streamline$FA)
}

#____________________________________________________________________________________________________
# setwd("C:/Users/Vale/Google Drive/Progetto_StatApp/File per paper/RData streamline riparametrizzate")
setwd("/Users/ILARIASARTORI/Google drive/Progetto_StatApp/File per paper/RData_tratti_riparametrizzati")

# load("lists_patients.RData")

setwd("/Users/ILARIASARTORI/Google drive/Progetto_StatApp/File per paper/HICA")

# ###_______________________________________________________________________________________________________________________________________________
# ###########
# ##   MD  ##
# ###########
 
# tract_tot_MD_VECCHIO = NULL
# for (i in 1:length(patients_left))
# {
#   for (j in 1:length(patients_left[[i]]$data))
#     tract_tot_MD_VECCHIO = rbind(tract_tot_MD_VECCHIO, patients_left[[i]]$data[[j]]$MD)
# }
# for (i in 1:length(patients_right))
# {
#   for (j in 1:length(patients_right[[i]]$data))
#     tract_tot_MD_VECCHIO = rbind(tract_tot_MD_VECCHIO, patients_right[[i]]$data[[j]]$MD)
# }

## Con le mappe
# tract_tot_MD_left_list = map(patients_left, get_patient_MD)
# tract_tot_MD_left=NULL
# for (i in 1: length(patients_left)) {
#   tract_tot_MD_left = cbind(tract_tot_MD_left, tract_tot_MD_left_list[[i]])
# }
# 
# tract_tot_MD_right_list = map(patients_right, get_patient_MD)
# tract_tot_MD_right=NULL
# for (i in 1: length(patients_right)) {
#   tract_tot_MD_right = cbind(tract_tot_MD_right, tract_tot_MD_right_list[[i]])
# }
# 
# tract_tot_MD=cbind(tract_tot_MD_left, tract_tot_MD_right)
# tract_tot_MD=t(tract_tot_MD)
# 
# save(tract_tot_MD, file="tract_tot_MD.RData")

load("tract_tot_MD.RData")

# Plot loadings
get_HICA_loadings(tract_tot_MD, 4, "Mean Diffusivity tracts")




# ###_______________________________________________________________________________________________________________________________________________
# ###########
# ##   AD  ##
# ###########

# tract_tot_AD = NULL
# for (i in 1:length(patients_left))
# {
#   for (j in 1:length(patients_left[[i]]$data))
#     tract_tot_AD = rbind(tract_tot_AD, patients_left[[i]]$data[[j]]$AD)
# }
# for (i in 1:length(patients_right))
# {
#   for (j in 1:length(patients_right[[i]]$data))
#     tract_tot_AD = rbind(tract_tot_AD, patients_right[[i]]$data[[j]]$AD)
# }

# tract_tot_AD_left_list = map(patients_left, get_patient_AD)
# tract_tot_AD_left=NULL
# for (i in 1: length(patients_left)) {
#   tract_tot_AD_left = cbind(tract_tot_AD_left, tract_tot_AD_left_list[[i]])
# }
# 
# tract_tot_AD_right_list = map(patients_right, get_patient_AD)
# tract_tot_AD_right=NULL
# for (i in 1: length(patients_right)) {
#   tract_tot_AD_right = cbind(tract_tot_AD_right, tract_tot_AD_right_list[[i]])
# }
# 
# tract_tot_AD=cbind(tract_tot_AD_left, tract_tot_AD_right)
# tract_tot_AD=t(tract_tot_AD)
# 
# save(tract_tot_AD, file="tract_tot_AD.RData")

load("tract_tot_AD.RData")
# Plot loadings
get_HICA_loadings(tract_tot_AD, 4, "Axial Diffusivity tracts")




# ###_______________________________________________________________________________________________________________________________________________
# ###########
# ##   RD  ##
# ###########

# tract_tot_RD = NULL
# for (i in 1:length(patients_left))
# {
#   for (j in 1:length(patients_left[[i]]$data))
#     tract_tot_RD = rbind(tract_tot_RD, patients_left[[i]]$data[[j]]$RD)
# }
# for (i in 1:length(patients_right))
# {
#   for (j in 1:length(patients_right[[i]]$data))
#     tract_tot_RD = rbind(tract_tot_RD, patients_right[[i]]$data[[j]]$RD)
# }

# tract_tot_RD_left_list = map(patients_left, get_patient_RD)
# tract_tot_RD_left=NULL
# for (i in 1: length(patients_left)) {
#   tract_tot_RD_left = cbind(tract_tot_RD_left, tract_tot_RD_left_list[[i]])
# }
# 
# tract_tot_RD_right_list = map(patients_right, get_patient_RD)
# tract_tot_RD_right=NULL
# for (i in 1: length(patients_right)) {
#   tract_tot_RD_right = cbind(tract_tot_RD_right, tract_tot_RD_right_list[[i]])
# }
# 
# tract_tot_RD=cbind(tract_tot_RD_left, tract_tot_RD_right)
# tract_tot_RD=t(tract_tot_RD)
# 
# save(tract_tot_RD, file="tract_tot_RD.RData")

load("tract_tot_RD.RData")
# Plot loadings
get_HICA_loadings(tract_tot_RD, 4, "Radial Diffusivity tracts")




# ###_______________________________________________________________________________________________________________________________________________
# ###########
# ##   FA  ##
# ###########

# tract_tot_FA = NULL
# for (i in 1:length(patients_left))
# {
#   for (j in 1:length(patients_left[[i]]$data))
#     tract_tot_FA = rbind(tract_tot_FA, patients_left[[i]]$data[[j]]$FA)
# }
# for (i in 1:length(patients_right))
# {
#   for (j in 1:length(patients_right[[i]]$data))
#     tract_tot_FA = rbind(tract_tot_FA, patients_right[[i]]$data[[j]]$FA)
# }
# tract_tot_FA_left_list = map(patients_left, get_patient_FA)
# tract_tot_FA_left=NULL
# for (i in 1: length(patients_left)) {
#   tract_tot_FA_left = cbind(tract_tot_FA_left, tract_tot_FA_left_list[[i]])
# }
# 
# tract_tot_FA_right_list = map(patients_right, get_patient_FA)
# tract_tot_FA_right=NULL
# for (i in 1: length(patients_right)) {
#   tract_tot_FA_right = cbind(tract_tot_FA_right, tract_tot_FA_right_list[[i]])
# }
# 
# tract_tot_FA=cbind(tract_tot_FA_left, tract_tot_FA_right)
# tract_tot_FA=t(tract_tot_FA)
# 
# save(tract_tot_FA, file="tract_tot_FA.RData")

load("tract_tot_FA.RData")
# Plot loadings
get_HICA_loadings(tract_tot_FA, 4, "Fractional Anisotropy tracts")
