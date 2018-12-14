library(tidyverse)
# library(devtools)
# library(readr)
# library(fdatractography)
# library(boot)
# library(gtools)
# library(Gmedian)
# library(sfsmisc)
library(cluster)
# library(rgl)


######################################################################################
############################### Caricare tratti ######################################
######################################################################################
# Loading tracts
setwd("/Users/ILARIASARTORI/Politecnico di Milano/Luca Torriani - Project StatApp/RData")
load("cst_list.RData")


################################### MANCA IL MALATO ###################################
### Aggiungo a mano il cst del malato (poi quello definitivo va inserito nella cst_list)
# load("../RData/cst00_rep_no_outliers.RData")
# cst00 = list(cst00_left, cst00_right)
# cst_list = c(list(cst00), cst_list) 
# Lista di 21 elementi. 
# Ogni elemento rappresenta un paziente (il primo è il paziente malato e poi in ordine da 1 a 20)
# Ogni elemento (i.e paziente) è rappresentato da una lista di 2 elementi: il cst sinistro e il destro
#######################################################################################

source("~/Desktop/OneDrive - Politecnico di Milano/CST_atlas/Clustering/helper_cluster.R")
source("/Users/ILARIASARTORI/Desktop/Poli/IVanno/CST_atlas/Clustering/helper_cluster.R")

num_streamline_patients = map_dbl (cst_list, num_of_streamline_patient)   # !! Senza outliers!!!!! 
# Per ogni paziente somma numero streamline destro+sinistro

################################################################################################
############################ Creare dataset primi punti ########################################
################################################################################################
# names_col = names(cst_list[[1]]$lhs$Streamlines[[1]])
# ind_x = which(names_col=="x")
# ind_y = which(names_col=="y")
# ind_z = which(names_col=="z")
# data_first_points = map(cst_list, dataset_first_points, ind_x, ind_y, ind_z)
# # Mappa di 21 elementi. Ogni elemento è il dataset dei punti iniziali delle streamline di quel paziente
# 
# save(data_first_points, file = "data_first_points.RData")

#### Caricare dataset primi punti (data_first_points)
load("data_first_points.RData")    # Lista dataset primi punti

################################################################################################
######################################## Clustering ############################################
################################################################################################

################################### MANCA IL MALATO ###################################
# data_first_points_sick_matrix = data_first_points[[1]]
# 
# data_first_points_healty_matrix = NULL  # Metto i punti iniziali dei sani insieme in una matrice
# for (i in 2:length(data_first_points)) {
#   data_first_points_healty_matrix = rbind (data_first_points_healty_matrix, data_first_points[[i]])
# }

data_first_points_healty_matrix = map_df (data_first_points, rbind)
#######################################################################################


#### Cluster sani
n=50
treshold = 20
num_healty_patients = 20
k_opt = get_k_opt(data_first_points_healty_matrix, n, treshold, num_healty_patients) ## 9
clara = get_cluster_clara(data_first_points_healty_matrix, k_opt)  # prima di clara c'è un set.seed(1994)
cluster_healty = clara$clustering

# n=50
# num_healty_patients = 20
# k_opt_vec=NULL
# treshold_vec = c(5,10,15,20,25,30,35,40,45,50,55,60,65,70)
# for (treshold in treshold_vec) {
#   k_opt_vec = c(k_opt_vec, get_k_opt(data_first_points_healty_matrix, n, treshold, num_healty_patients)) 
# }
# 
# quartz()
# plot(treshold_vec, k_opt_vec, pch = 19, xlab = "Threshold", ylab = "k_opt", main = "k_opt - threshold")
# lines(treshold_vec, k_opt_vec)


################################### MANCA IL MALATO ###################################
# #### Cluster malato
# choose_k_sick(data_first_points_sick_matrix[,1:3], k_opt)  # Mi da errore non converge?
# k_sick = 6 #???? DA SISTEMARE, è per fare una prova
# k_means_sick = kmeans(data_first_points_sick_matrix[,1:3], k_sick)
# cluster_sick = k_means_sick$cluster
# 
# ####  In data_first_points aggiungo la colonna con i cluster di appartenenza
# data_first_points = map(data_first_points, add_cluster_column, clusters = c(cluster_sick, cluster_healty), num_streamline_patients=num_streamline_patients)

# add_cluster_column CAMBIATA per gestire il fatto che non c'è il malato
####  In data_first_points aggiungo la colonna con i cluster di appartenenza
data_first_points = map(data_first_points, add_cluster_column, clusters = cluster_healty, num_streamline_patients=num_streamline_patients)
#######################################################################################

#### Riproietto il tratto sinistro nel piano delle x negative
data_first_points = map(data_first_points, reproject_x)

################################################################################################
############################### Creo features_patients_9_6.RData ###############################
################################################################################################
### Loading features
load("features_list.RData")

################################### MANCA IL MALATO ###################################
# ### Aggiungo a mano le features del malato (poi quello definitivo va inserito nella features_list)
# load("cst0_features.RData")
# features00 = list(cst0_left_features, cst0_right_features)
# features_list = c(list(features00), features_list)
# # Lista di 21 elementi.
# # Ogni elemento rappresenta un paziente (il primo è il paziente malato e poi in ordine da 1 a 20)
# # Ogni elemento (i.e paziente) è rappresentato da una lista di 2 elementi: le features sinistre e le destre
#######################################################################################

# Lista di 21 elementi. Ogni elemento è un dataframe con le features delle varie streamline di quel paziente,
# prima quelle sinistre e poi le destre
features_list_sxdx = map (features_list, merge_left_right_features)

################################### MANCA IL MALATO ###################################
# # Aggiungo gli indici del cluster
# features_list_sxdx = map (features_list_sxdx, add_cluster_column, clusters = c(cluster_sick, cluster_healty), num_streamline_patients=num_streamline_patients)  # features_final
# 
# setwd("/Users/ILARIASARTORI/Google drive/Progetto_StatApp/File per paper/Rdata_clustering")
# save (features_list_sxdx, file="features_patients_9_6.RData")

# Aggiungo gli indici del cluster
features_list_sxdx = map (features_list_sxdx, add_cluster_column, clusters = cluster_healty, num_streamline_patients=num_streamline_patients)  # features_final

save (features_list_sxdx, mean_left, sd_left, mean_right, sd_right, file="features_patients_9.RData")
#######################################################################################


################################################################################################
########################## Creo features_patients_reduced_9_6.RData ############################
################################################################################################
setwd("~/Desktop/Politecnico di Milano/Luca Torriani - Project StatApp/RData")
load("features_patients_9.RData")
# Calcolo i centroidi
features_reduced_tmp = purrr::map (features_list_sxdx, get_reduced_tot, mean_left=mean_left, sd_left=sd_left, mean_right=mean_right, sd_right=sd_right)
extract_centroid = function (data) {
  return(data$centroid)
}
extract_var_sx = function (data) {
  return(data$var_sx)
}
extract_var_dx = function (data) {
  return(data$var_dx)
}
features_reduced = map(features_reduced_tmp, extract_centroid) # Centroids
var_sx_features_reduced = map(features_reduced_tmp, extract_var_sx)
var_dx_features_reduced = map(features_reduced_tmp, extract_var_dx)

save (features_reduced, var_sx_features_reduced, var_dx_features_reduced, file="features_patients_reduced_9.RData")

# Plot variance
find_max = function(data) {
  return (map_dbl(data, max))
}
find_min = function(data) {
  return (map_dbl(data, min))
}

# Create plots variance intracluster patient per patient
library(fields)

setwd("/Users/ILARIASARTORI/Desktop/Poli/IVanno/CST_atlas/Clustering/Plot_variance_intracluster")
# id_pat = 1
# side = "left"

for (side in c("left", "right")) {
  for (id_pat in 1:20) {
    if(side=="left") {
      z_max = max(map_dbl(var_sx_features_reduced[[id_pat]],max))
      z_min = min(map_dbl(var_sx_features_reduced[[id_pat]],min))
      # z_max = max(map_dbl(map(var_sx_features_reduced, find_max), max))
      # z_min = min(map_dbl(map(var_sx_features_reduced, find_min), min))
      
    } else {
      z_max = max(map_dbl(var_dx_features_reduced[[id_pat]],max))
      z_min = min(map_dbl(var_dx_features_reduced[[id_pat]],min))
      # z_max = max(map_dbl(map(var_dx_features_reduced, find_max), max))
      # z_min = min(map_dbl(map(var_dx_features_reduced, find_min), min))
    }
    z_lim=c(z_min,z_max)
    pdf(paste0('Variance_intracluster-Pat_',id_pat,'-Side_',side,'.pdf'))
    par(mfrow=c(3,3), mai = c(0.1,0.1,0.3,0.1), oma=c(1,1,4,1))
    for (i in 1:9){
      if(side=="left") image.plot(var_sx_features_reduced[[id_pat]][[i]], zlim=z_lim, axes=F, main = paste("Cluster",i))
      else image.plot(var_dx_features_reduced[[id_pat]][[i]], zlim=z_lim, axes=F, main = paste("Cluster",i))
    }
    mtext(paste("Patient",id_pat, ", ", "Side ", side), side = 3, line = 1, outer = TRUE, font=2)
    dev.off()
  }
}


# image.plot(t(var_sx_features_reduced[[1]]), axes = F, xlim = c(0,1), ylim=c(0,1) )
# mtext(text=c(paste("Cluster",1:9)), side=2, line=0.3, at=seq(0,1, length.out = 9), las=1, cex=0.8)
# mtext(text=c(paste("Feature",1:33)), side=1, line=0.3, at=seq(0,1, length.out = 33), las=2, cex=0.8)
# image.plot(t(var_sx_features_reduced[[1]]), legend.only=T)


######################################################################################
########################## Primo grafico #############################################
######################################################################################
library(rgl)
setwd("/Users/ILARIASARTORI/Politecnico di Milano/Luca Torriani - Project StatApp/RData")
load("features_patients_9.RData")
load("features_patients_reduced_9.RData")

source("/Users/ILARIASARTORI/Desktop/Poli/IVanno/CST_atlas/Clustering/helper_cluster.R")
source("/Users/ILARIASARTORI/Desktop/Poli/IVanno/CST_atlas/Clustering/helper_cluster_plot.R")

# Loading tracts
load("cst_list.RData")

################################### MANCA IL MALATO ###################################
# ### Aggiungo a mano il cst del malato (poi quello definitivo va inserito nella cst_list)
# load("../RData/cst00_rep_no_outliers.RData")
# cst00 = list(cst00_left, cst00_right)
# cst_list = c(list(cst00), cst_list)
# # tolgo il malato
# features_reduced_healty = features_reduced[-1]
# features_list_sxdx_healty = features_list_sxdx[-1]
# cst_list_healty = cst_list[-1]

features_reduced_healty = features_reduced
features_list_sxdx_healty = features_list_sxdx
cst_list_healty = cst_list
#######################################################################################

indexes_plot1 = get_cluster_patient_true_mean_indexes_healty (features_reduced_healty, features_list_sxdx_healty)

plot_from_indexes (cst_list_healty, indexes_plot1)

### Verifiche sugli indici
# healty_tract_lengths = map(cst_list_healty, num_of_streamline_tract_sx_dx)
# 
# healty_tract_lengths_matrix= NULL
# for (i in 1:length(healty_tract_lengths))  {
#   healty_tract_lengths_matrix = rbind (healty_tract_lengths_matrix, healty_tract_lengths[[i]])
# }
# # 20 righe (pazienti) con due colonne (sx e dx)
# 
# index_left = NULL
# index_right = NULL
# for (j in 1:length(indexes_plot1)) {
#   index_left = cbind(index_left,indexes_plot1[[j]]$lhs$idx)
#   index_right = cbind(index_right,indexes_plot1[[j]]$rhs$idx)
# }
# max_index_left = t(apply(index_left, 1, max))
# max_index_right = t(apply(index_right, 1, max))
# 
# left = (max_index_left < healty_tract_lengths_matrix[,1])
# right = (max_index_right < healty_tract_lengths_matrix[,2])


######################################################################################
################################# Secondo grafico ####################################
######################################################################################
library(rgl)
setwd("/Users/ILARIASARTORI/Politecnico di Milano/Luca Torriani - Project StatApp/RData")
load("features_patients_9.RData")
load("features_patients_reduced_9.RData")

source("/Users/ILARIASARTORI/Desktop/Poli/IVanno/CST_atlas/Clustering/helper_cluster.R")
source("/Users/ILARIASARTORI/Desktop/Poli/IVanno/CST_atlas/Clustering/helper_cluster_plot.R")

# Loading tracts
load("cst_list.RData")

################################### MANCA IL MALATO ###################################
# ### Aggiungo a mano il cst del malato (poi quello definitivo va inserito nella cst_list)
# load("../RData/cst00_rep_no_outliers.RData")
# cst00 = list(cst00_left, cst00_right)
# cst_list = c(list(cst00), cst_list)
# # tolgo il malato
# features_reduced_healty = features_reduced[-1]
# features_list_sxdx_healty = features_list_sxdx[-1]
# cst_list_healty = cst_list[-1]

features_reduced_healty = features_reduced
features_list_sxdx_healty = features_list_sxdx
cst_list_healty = cst_list
#######################################################################################

indexes_plot2 = get_cluster_true_mean_indexes_healty (features_reduced_healty, features_list_sxdx_healty, mean_left=mean_left, sd_left=sd_left, mean_right=mean_right, sd_right=sd_right)

plot_from_indexes (cst_list_healty, indexes_plot2$final_indexes)

# Create plots variance intracluster 
setwd("~/Desktop/OneDrive - Politecnico di Milano/CST_atlas/Clustering/Plot_variance_intracluster")
setwd("/Users/ILARIASARTORI/Desktop/Poli/IVanno/CST_atlas/Clustering/Plot_variance_intracluster")
# id_pat = 1
# side = "left"
var_sx = indexes_plot2$var_sx
var_dx = indexes_plot2$var_dx

z_max_left = max(map_dbl(var_sx,max))
z_min_left = min(map_dbl(var_sx,min))
z_max_right = max(map_dbl(var_dx,max))
z_min_right = min(map_dbl(var_dx,min))
z_max = max(z_max_left, z_max_right)
z_min = min(z_min_left, z_min_right)
z_lim=c(z_min,z_max)

for (side in c("left", "right")) {
  pdf(paste0('Variance_intracluster','-Side_',side,'.pdf'))
  par(mfrow=c(3,3), mai = c(0.1,0.1,0.3,0.1), oma=c(1,1,4,1))
  for (i in 1:9){
    if(side=="left") image.plot(var_sx[[i]], zlim=z_lim, axes=F, main = paste("Cluster",i))
    else image.plot(var_dx[[i]], zlim=z_lim, axes=F, main = paste("Cluster",i))
  }
  mtext(paste("Side ", side), side = 3, line = 1, outer = TRUE, font=2)
  dev.off()
}


### Verifiche sugli indici
# healty_tract_lengths = map(cst_list_healty, num_of_streamline_tract_sx_dx)
# 
# healty_tract_lengths_matrix= NULL
# for (i in 1:length(healty_tract_lengths))  {
#   healty_tract_lengths_matrix = rbind (healty_tract_lengths_matrix, healty_tract_lengths[[i]])
# }
# # 20 righe (pazienti) con due colonne (sx e dx)
# 
# patients_left = NULL
# patients_right = NULL
# index_left = NULL
# index_right = NULL
# for (j in 1:length(indexes_plot2)) {
#   patients_left = c(patients_left,indexes_plot2[[j]]$lhs$patient)
#   patients_right = c(patients_right,indexes_plot2[[j]]$rhs$patient)
#   
#   index_left = c(index_left,indexes_plot2[[j]]$lhs$idx)
#   index_right = c(index_right,indexes_plot2[[j]]$rhs$idx)
# }
# pat_index_left = cbind(patients_left, index_left)
# pat_index_right = cbind(patients_right, index_right)
# 
# left = NULL
# for (i in 1: dim (pat_index_left)[1]) {
#   left = c(left, pat_index_left[i,2] <= healty_tract_lengths_matrix[pat_index_left[i,1], 1])  # Devono essere tutti true
# }
# 
# right = NULL
# for (i in 1: dim (pat_index_right)[1]) {
#   right = c(right, pat_index_right[i,2] <= healty_tract_lengths_matrix[pat_index_right[i,1], 2])  # Devono essere tutti true
# }

######################################################################################
################################# Terzo grafico ##########################################
######################################################################################
load("../RData/features_patients_reduced_9_6.RData")
load("../RData/features_patients_9_6.RData")
load("../RData/tensor_list.RData")

source("helper_cluster.R")
source("helper_grafico3.R")

load ("../RData/outliers.RData")    # Indici outliers
# outliers_sx lista di 20 vettori contenenti gli indici delle streamline individuate come outliers 
# nel tratto sx dei 20 pazienti
# outliers_dx lista di 20 vettori contenenti gli indici delle streamline individuate come outliers 
# nel tratto dx dei 20 pazienti

# NEW: 21/11/18 (per ora non esiste) 
load ("../RData/list_tensors.RData") # pensavo di usarla per recuperare il numero originale di streamlines prima di
                                    # rimuovere gli outliers
# HP: lista di 20/21 elementi (dipende da malato): 
# - ciascun paziente ha due campi (lhs, rhs), ciascuno dei quali e' a sua volta una lista di lunghezza (n_stream_left, n_stream_right)
#   che a sua volta e' una lista di 50 tensori (uno per punto)

#NEW: 20/11/18
# Da testare
num_stream_pat_side_df =  do.call(rbind.data.frame, map(list_tensors, num_of_streamline_from_tensor_list_cst))
num_stream_pat_side = list( lhs = as.list(num_stream_pat_side_df[,1]),  rhs = as.list(num_stream_pat_side_df[,2]))
####

old_indexes = find_original_index_representative(indexes_plot1, outliers_sx, outliers_dx, num_stream_pat_side)
list_mean_tensors = mean_tensors(old_indexes, list_tensors)


# PROVA

# indexes_plot1_tmp = indexes_plot1
# for(i in 1:9){
#   for(j in 1:2){
#     indexes_plot1_tmp[[i]][[j]]$idx = indexes_plot1_tmp[[i]][[j]]$idx[c(1,2)]
#     indexes_plot1_tmp[[i]][[j]]$patient = indexes_plot1_tmp[[i]][[j]]$patient[c(1,2)]
#     
#   }
# }
# 
# 
# outliers_sx_tmp = outliers_sx[c(1,2)]
# outliers_dx_tmp = outliers_dx[c(1,2)]
# 
# 
# old_indexes = find_original_index_representative(indexes_plot1_tmp, outliers_sx_tmp, outliers_dx_tmp, num_stream_pat_side)

