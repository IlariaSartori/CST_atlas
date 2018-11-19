library(tidyverse)
library(devtools)
library(readr)
library(fdatractography)
library(boot)
library(gtools)
library(Gmedian)
library(sfsmisc)
library(cluster)
library(fda)

source("utility_functions.R")
source("create_dataset.R")
# source("tensor_variables.R")
setwd("C:/Users/Vale/Google Drive/Progetto_StatApp/File per paper/Rdata_tratti_riparametrizzati_no_outliers")
setwd("/Users/ILARIASARTORI/Google drive/Progetto_StatApp/File per paper/RData_tratti_riparametrizzati_no_outliers")
setwd("C:/Users/User/Google Drive/Progetto_StatApp/File per paper/RData_tratti_riparametrizzati_no_outliers")


load("cst00_rep_no_outliers.RData")
load("cst01_rep_no_outliers.RData")
load("cst02_rep_no_outliers.RData")
load("cst03_rep_no_outliers.RData")
load("cst04_rep_no_outliers.RData")
load("cst05_rep_no_outliers.RData")
load("cst06_rep_no_outliers.RData")
load("cst07_rep_no_outliers.RData")
load("cst08_rep_no_outliers.RData")
load("cst09_rep_no_outliers.RData")
load("cst10_rep_no_outliers.RData")
load("cst11_rep_no_outliers.RData")
load("cst12_rep_no_outliers.RData")
load("cst13_rep_no_outliers.RData")
load("cst14_rep_no_outliers.RData")
load("cst15_rep_no_outliers.RData")
load("cst16_rep_no_outliers.RData")
load("cst17_rep_no_outliers.RData")
load("cst18_rep_no_outliers.RData")
load("cst19_rep_no_outliers.RData")
load("cst20_rep_no_outliers.RData")

# Evaluate lambda_opt
# lambda_opt_vec = map_dbl(cst01_left$data, fda3D_evaluate_lambda)  
# lambda_opt=median(lambda_opt_vec)   
# 3.187206 (con optim)
# 3.04284112980894 (con optimize)
lambda_opt = 3.04284112980894

# cst00__left_features = create_dataset (cst00_left, "left") 
# cst00__right_features = create_dataset (cst00_right, "right")

# Prova quello left! (se hai voglia prova a dargli un'occhiata)
# setwd("C:/Users/User/Google Drive/Progetto_StatApp/File per paper/RData_tratti_riparametrizzati_no_outliers")
setwd("/Users/ILARIASARTORI/Google drive/Progetto_StatApp/File per paper/RData_tratti_riparametrizzati_no_outliers")
load("cst16_rep_no_outliers.RData")
cst16__left_features = create_dataset (cst16_left, "left", lambda_opt=lambda_opt, standardized = F) 
cst16__right_features = create_dataset (cst16_right, "right", lambda_opt=lambda_opt, standardized = F)
# setwd("C:/Users/User/Google Drive/Progetto_StatApp/File per paper/RData_dataset_no_std")
setwd("/Users/ILARIASARTORI/Google drive/Progetto_StatApp/File per paper/RData_dataset_no_std")
save(cst16__left_features, cst16__right_features, file = "cst_16_features.RData")

cst02__left_features = create_dataset (cst02_left, "left") 
cst02__right_features = create_dataset (cst02_right, "right")

cst03__left_features = create_dataset (cst03_left, "left") 
cst03__right_features = create_dataset (cst03_right, "right")

cst04__left_features = create_dataset (cst04_left, "left") 
cst04__right_features = create_dataset (cst04_right, "right")

cst05__left_features = create_dataset (cst05_left, "left") 
cst05__right_features = create_dataset (cst05_right, "right")

cst06__left_features = create_dataset (cst06_left, "left") 
cst06__right_features = create_dataset (cst06_right, "right")

cst07__left_features = create_dataset (cst07_left, "left") 
cst07__right_features = create_dataset (cst07_right, "right")

cst08__left_features = create_dataset (cst08_left, "left") 
cst08__right_features = create_dataset (cst08_right, "right")

cst09__left_features = create_dataset (cst09_left, "left") 
cst09__right_features = create_dataset (cst09_right, "right")

cst10__left_features = create_dataset (cst10_left, "left") 
cst10__right_features = create_dataset (cst10_right, "right")

cst11__left_features = create_dataset (cst11_left, "left") 
cst11__right_features = create_dataset (cst11_right, "right")

cst12__left_features = create_dataset (cst12_left, "left") 
cst12__right_features = create_dataset (cst12_right, "right")

cst13__left_features = create_dataset (cst13_left, "left") 
cst13__right_features = create_dataset (cst13_right, "right")

cst14__left_features = create_dataset (cst14_left, "left") 
cst14__right_features = create_dataset (cst14_right, "right")

cst15__left_features = create_dataset (cst15_left, "left") 
cst15__right_features = create_dataset (cst15_right, "right")

cst16__left_features = create_dataset (cst16_left, "left") 
cst16__right_features = create_dataset (cst16_right, "right")

cst17__left_features = create_dataset (cst17_left, "left") 
cst17__right_features = create_dataset (cst17_right, "right")

cst18__left_features = create_dataset (cst18_left, "left") 
cst18__right_features = create_dataset (cst18_right, "right")

cst19__left_features = create_dataset (cst19_left, "left") 
cst19__right_features = create_dataset (cst19_right, "right")

cst20__left_features = create_dataset (cst20_left, "left") 
cst20__right_features = create_dataset (cst20_right, "right")
