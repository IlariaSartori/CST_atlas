library(tidyverse)
library(devtools)
library(readr)
library(boot)
library(gtools)
library(Gmedian)
library(sfsmisc)
library(cluster)
library(fiber)
library(fda)

source("utility_functions.R")
source("helpers_create_features_dataset.R")
# source("Curvatura_torsione3D.RData")

# source("tensor_variables.R")
# setwd("C:/Users/Vale/Google Drive/Progetto_StatApp/File per paper/Rdata_tratti_riparametrizzati_no_outliers")
# setwd("/Users/ILARIASARTORI/Google drive/Progetto_StatApp/File per paper/RData_tratti_riparametrizzati_no_outliers")
# setwd("C:/Users/User/Google Drive/Progetto_StatApp/File per paper/RData_tratti_riparametrizzati_no_outliers")

setwd("/Users/ILARIASARTORI/Politecnico di Milano/Luca Torriani - Project StatApp/RData")
setwd("C:/Users/User/OneDrive - Politecnico di Milano/Project StatApp/RData")

load("cst_list.RData")
# load("cst00_rep_no_outliers.RData")


#########################################################################################
################################ CURVATURA TORSIONE SUE #################################
#########################################################################################
cst08 = cst_list[[8]]
# lambda_opt = 3.04284112980894

cst08__left_features = create_dataset_new (cst08$lhs, "left", standardized = F) 


cst08__right_features = create_dataset_new (cst08$rhs, "right", standardized = F)

setwd("C:/Users/User/OneDrive - Politecnico di Milano/Project StatApp/RData/features_rep_no_std")
save(cst08__left_features, cst08__right_features, file = "cst_08_features.RData")


# quartz()
# par(mfrow=c(2,3))
# plot(cst01__left_features$curv_maxDIFF)
# abline(h=0, col="red")
# plot(cst01__left_features$curv_meanDIFF)
# abline(h=0, col="red")
# plot(cst01__left_features$curv_sdDIFF)
# abline(h=0, col="red")
# plot(cst01__left_features$tors_maxDIFF)
# abline(h=0, col="red")
# plot(cst01__left_features$tors_meanDIFF)
# abline(h=0, col="red")
# plot(cst01__left_features$tors_sdDIFF)
# abline(h=0, col="red")

#########################################################################################
################################ CURVATURA TORSIONE NOSTRE ##############################
#########################################################################################
# source("create_dataset.R")
# # Evaluate lambda_opt
# # lambda_opt_vec = map_dbl(cst01_left$data, fda3D_evaluate_lambda)  
# # lambda_opt=median(lambda_opt_vec)   
# # 3.187206 (con optim)
# # 3.04284112980894 (con optimize)
# lambda_opt = 3.04284112980894
# 
# # cst00__left_features = create_dataset (cst00_left, "left") 
# # cst00__right_features = create_dataset (cst00_right, "right")
# 
# # Prova quello left! (se hai voglia prova a dargli un'occhiata)
# # setwd("C:/Users/User/Google Drive/Progetto_StatApp/File per paper/RData_tratti_riparametrizzati_no_outliers")
# setwd("/Users/ILARIASARTORI/Google drive/Progetto_StatApp/File per paper/RData_tratti_riparametrizzati_no_outliers")
# load("cst16_rep_no_outliers.RData")
# cst16__left_features = create_dataset (cst16_left, "left", lambda_opt=lambda_opt, standardized = F) 
# cst16__right_features = create_dataset (cst16_right, "right", lambda_opt=lambda_opt, standardized = F)
# # setwd("C:/Users/User/Google Drive/Progetto_StatApp/File per paper/RData_dataset_no_std")
# setwd("/Users/ILARIASARTORI/Google drive/Progetto_StatApp/File per paper/RData_dataset_no_std")
# save(cst16__left_features, cst16__right_features, file = "cst_16_features.RData")
# 
# cst02__left_features = create_dataset (cst02_left, "left") 
# cst02__right_features = create_dataset (cst02_right, "right")
# 
# cst03__left_features = create_dataset (cst03_left, "left") 
# cst03__right_features = create_dataset (cst03_right, "right")
# 
# cst04__left_features = create_dataset (cst04_left, "left") 
# cst04__right_features = create_dataset (cst04_right, "right")
# 
# cst05__left_features = create_dataset (cst05_left, "left") 
# cst05__right_features = create_dataset (cst05_right, "right")
# 
# cst06__left_features = create_dataset (cst06_left, "left") 
# cst06__right_features = create_dataset (cst06_right, "right")
# 
# cst07__left_features = create_dataset (cst07_left, "left") 
# cst07__right_features = create_dataset (cst07_right, "right")
# 
# cst08__left_features = create_dataset (cst08_left, "left") 
# cst08__right_features = create_dataset (cst08_right, "right")
# 
# cst09__left_features = create_dataset (cst09_left, "left") 
# cst09__right_features = create_dataset (cst09_right, "right")
# 
# cst10__left_features = create_dataset (cst10_left, "left") 
# cst10__right_features = create_dataset (cst10_right, "right")
# 
# cst11__left_features = create_dataset (cst11_left, "left") 
# cst11__right_features = create_dataset (cst11_right, "right")
# 
# cst12__left_features = create_dataset (cst12_left, "left") 
# cst12__right_features = create_dataset (cst12_right, "right")
# 
# cst13__left_features = create_dataset (cst13_left, "left") 
# cst13__right_features = create_dataset (cst13_right, "right")
# 
# cst14__left_features = create_dataset (cst14_left, "left") 
# cst14__right_features = create_dataset (cst14_right, "right")
# 
# cst15__left_features = create_dataset (cst15_left, "left") 
# cst15__right_features = create_dataset (cst15_right, "right")
# 
# cst16__left_features = create_dataset (cst16_left, "left") 
# cst16__right_features = create_dataset (cst16_right, "right")
# 
# cst17__left_features = create_dataset (cst17_left, "left") 
# cst17__right_features = create_dataset (cst17_right, "right")
# 
# cst18__left_features = create_dataset (cst18_left, "left") 
# cst18__right_features = create_dataset (cst18_right, "right")
# 
# cst19__left_features = create_dataset (cst19_left, "left") 
# cst19__right_features = create_dataset (cst19_right, "right")
# 
# cst20__left_features = create_dataset (cst20_left, "left") 
# cst20__right_features = create_dataset (cst20_right, "right")
