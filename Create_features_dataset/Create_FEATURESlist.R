
setwd("/Users/ILARIASARTORI/Politecnico di Milano/Luca Torriani - Project StatApp/RData")

load("cst01_features.RData")
load("cst02_features.RData")
load("cst03_features.RData")
load("cst04_features.RData")
load("cst05_features.RData")
load("cst06_features.RData")
load("cst07_features.RData")
load("cst08_features.RData")
load("cst09_features.RData")
load("cst10_features.RData")
load("cst11_features.RData")
load("cst12_features.RData")
load("cst13_features.RData")
load("cst14_features.RData")
load("cst15_features.RData")
load("cst16_features.RData")
load("cst17_features.RData")
load("cst18_features.RData")
load("cst19_features.RData")
load("cst20_features.RData")


features_list = list( cst01_features, 
                      cst02_features, 
                      cst03_features, 
                      cst04_features, 
                      cst05_features, 
                      cst06_features,
                      cst07_features, 
                      cst08_features, 
                      cst09_features,
                      cst10_features,
                      cst11_features,
                      cst12_features,
                      cst13_features,
                      cst14_features,
                      cst15_features,
                      cst16_features,
                      cst17_features,
                      cst18_features,
                      cst19_features,
                      cst20_features)

select_left = function(features_pat) {
  return(features_pat$lhs)
}
select_right = function(features_pat) {
  return(features_pat$rhs)
}

library(purrr)
features_left = map_dfr(features_list, select_left)
mean_left = colMeans(features_left[,1:33])
sd_left = apply(features_left[,1:33], 2, sd)
features_right = map_df(features_list, select_right)
mean_right = colMeans(features_right[,1:33])
sd_right = apply(features_right[,1:33], 2, sd)
setwd("/Users/ILARIASARTORI/Politecnico di Milano/Luca Torriani - Project StatApp/RData")

save(features_list, mean_left, sd_left, mean_right, sd_right, file = "features_list.RData")
