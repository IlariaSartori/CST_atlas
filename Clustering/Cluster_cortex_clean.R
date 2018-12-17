library(tidyverse)
library(cluster)
library(rgl)
library (fiber)
library(dplyr)



######################################################################################
################################### Load tracts ######################################
######################################################################################

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

# For each patient, get the number of streamlines left+right sides
num_streamline_patients = map_dbl (cst_list, num_of_streamline_patient)


################################################################################################
######################### Create dataset points on the cortex punti ############################
################################################################################################
# names_col = names(cst_list[[1]]$lhs$Streamlines[[1]])
# ind_x = which(names_col=="x")
# ind_y = which(names_col=="y")
# ind_z = which(names_col=="z")
# data_first_points = map(cst_list, dataset_first_points, ind_x, ind_y, ind_z)
# # Mappa di 21 elementi. Ogni elemento è il dataset dei punti iniziali delle streamline di quel paziente
# 
# save(data_first_points, file = "data_first_points.RData")

#### Load dataset points on cortex 
load("data_first_points.RData")



################################################################################################
######################################## Clustering ############################################
################################################################################################

################################### MANCA IL MALATO ###################################
# data_first_points_sick_matrix = data_first_points[[1]]
# 
# data_first_points_healthy_matrix = NULL  # Metto i punti iniziali dei sani insieme in una matrice
# for (i in 2:length(data_first_points)) {
#   data_first_points_healthy_matrix = rbind (data_first_points_healthy_matrix, data_first_points[[i]])
# }

data_first_points_healthy_matrix = map_df (data_first_points, rbind)
#######################################################################################


#### Cluster sani
n=50
threshold = 20
num_healthy_patients = 20
k_opt = get_k_opt(data_first_points_healthy_matrix, n, threshold, num_healthy_patients) ## 9
clara = get_cluster_clara(data_first_points_healthy_matrix, k_opt)  # prima di clara c'è un set.seed(1994)
cluster_healthy = clara$clustering

# n=50
# num_healthy_patients = 20
# k_opt_vec=NULL
# threshold_vec = c(5,10,15,20,25,30,35,40,45,50,55,60,65,70)
# for (threshold in threshold_vec) {
#   k_opt_vec = c(k_opt_vec, get_k_opt(data_first_points_healthy_matrix, n, threshold, num_healthy_patients)) 
# }
# 
# quartz()
# plot(threshold_vec, k_opt_vec, pch = 19, xlab = "Threshold", ylab = "k_opt", main = "k_opt - threshold")
# lines(threshold_vec, k_opt_vec)


################################### MANCA IL MALATO ###################################
# #### Cluster malato
# choose_k_sick(data_first_points_sick_matrix[,1:3], k_opt)  # Mi da errore non converge?
# k_sick = 6 #???? DA SISTEMARE, è per fare una prova
# k_means_sick = kmeans(data_first_points_sick_matrix[,1:3], k_sick)
# cluster_sick = k_means_sick$cluster
# 
# ####  In data_first_points aggiungo la colonna con i cluster di appartenenza
# data_first_points = map(data_first_points, add_cluster_column, clusters = c(cluster_sick, cluster_healthy), num_streamline_patients=num_streamline_patients)

# add_cluster_column CAMBIATA per gestire il fatto che non c'è il malato
####  In data_first_points aggiungo la colonna con i cluster di appartenenza
data_first_points = map(data_first_points, add_cluster_column, clusters = cluster_healthy, num_streamline_patients=num_streamline_patients)
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
# features_list_sxdx = map (features_list_sxdx, add_cluster_column, clusters = c(cluster_sick, cluster_healthy), num_streamline_patients=num_streamline_patients)  # features_final
# 
# setwd("/Users/ILARIASARTORI/Google drive/Progetto_StatApp/File per paper/Rdata_clustering")
# save (features_list_sxdx, file="features_patients_9_6.RData")

# Aggiungo gli indici del cluster
features_list_sxdx = map (features_list_sxdx, add_cluster_column, clusters = cluster_healthy, num_streamline_patients=num_streamline_patients)  # features_final

save (features_list_sxdx, mean_left, sd_left, mean_right, sd_right, file="features_patients_9.RData")
#######################################################################################


################################################################################################
########################## Creo features_patients_reduced_9_6.RData ############################
################################################################################################
setwd("~/Desktop/Politecnico di Milano/Luca Torriani - Project StatApp/RData")
load("features_patients_9.RData")

# Calcolo i centroidi
features_reduced_tmp = purrr::map (features_list_sxdx, get_reduced_tot, mean_left=mean_left, sd_left=sd_left, mean_right=mean_right, sd_right=sd_right)

features_reduced = map(features_reduced_tmp, extract_centroid) # Centroids
var_sx_features_reduced = map(features_reduced_tmp, extract_var_sx)
var_dx_features_reduced = map(features_reduced_tmp, extract_var_dx)

save (features_reduced, var_sx_features_reduced, var_dx_features_reduced, file="features_patients_reduced_9.RData")



################################################################################################
######################################### Plot variance ########################################
################################################################################################

# Create plots variance intracluster patient per patient
library(fields)

setwd("/Users/ILARIASARTORI/Desktop/Poli/IVanno/CST_atlas/Clustering/Plot_variance_intracluster")
setwd("C:/Users/User/Programming/CST_atlas/Clustering/Plot_variance_intracluster")

for (side in c("left", "right")) {
  for (id_pat in 1:20) {
    z_max_left = max(map_dbl(var_sx_features_reduced[[id_pat]],max))
    z_min_left =  min(map_dbl(var_sx_features_reduced[[id_pat]],min))
    z_max_right = max(map_dbl(var_dx_features_reduced[[id_pat]],max))
    z_min_right = min(map_dbl(var_dx_features_reduced[[id_pat]],min))
    z_max = max(z_max_left, z_max_right)
    z_min = min(z_min_left, z_min_right)
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



######################################################################################
################################### First plot #######################################
######################################################################################
setwd("~/Desktop/Politecnico di Milano/Luca Torriani - Project StatApp/RData")
setwd("/Users/ILARIASARTORI/Politecnico di Milano/Luca Torriani - Project StatApp/RData")
setwd("C:/Users/User/OneDrive - Politecnico di Milano/Project StatApp/RData")

load("features_patients_9.RData")
load("features_patients_reduced_9.RData")

source("~/Desktop/OneDrive - Politecnico di Milano/CST_atlas/Clustering/helper_cluster.R")
source("~/Desktop/OneDrive - Politecnico di Milano/CST_atlas/Clustering/helper_cluster_plot.R")
source("/Users/ILARIASARTORI/Desktop/Poli/IVanno/CST_atlas/Clustering/helper_cluster.R")
source("/Users/ILARIASARTORI/Desktop/Poli/IVanno/CST_atlas/Clustering/helper_cluster_plot.R")
source("C:/Users/User/Programming/CST_atlas/Clustering/helper_cluster.R")
source("C:/Users/User/Programming/CST_atlas/Clustering/helper_cluster_plot.R")

# Loading tracts
load("cst_list.RData")
# Remove the sick patient if present in cst_list


indexes_plot1 = get_cluster_patient_true_mean_indexes_healthy (features_reduced, features_list_sxdx)
plot_from_indexes (cst_list, indexes_plot1)



######################################################################################
################################## Second plot #######################################
######################################################################################
library(rgl)
setwd("/Users/ILARIASARTORI/Politecnico di Milano/Luca Torriani - Project StatApp/RData")
setwd("C:/Users/User/OneDrive - Politecnico di Milano/Project StatApp/RData")

load("features_patients_9.RData")
load("features_patients_reduced_9.RData")

source("/Users/ILARIASARTORI/Desktop/Poli/IVanno/CST_atlas/Clustering/helper_cluster.R")
source("/Users/ILARIASARTORI/Desktop/Poli/IVanno/CST_atlas/Clustering/helper_cluster_plot.R")
source("C:/Users/User/Programming/CST_atlas/Clustering/helper_cluster.R")
source("C:/Users/User/Programming/CST_atlas/Clustering/helper_cluster_plot.R")


# Loading tracts
load("cst_list.RData")
# Remove the sick patient if present in cst_list


indexes_plot2 = get_cluster_true_mean_indexes_healthy (features_reduced, features_list_sxdx, mean_left=mean_left, sd_left=sd_left, mean_right=mean_right, sd_right=sd_right)
plot_from_indexes (cst_list, indexes_plot2$final_indexes)


# Create plots variance intracluster 
setwd("~/Desktop/OneDrive - Politecnico di Milano/CST_atlas/Clustering/Plot_variance_intracluster")
setwd("/Users/ILARIASARTORI/Desktop/Poli/IVanno/CST_atlas/Clustering/Plot_variance_intracluster")

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



######################################################################################
################################### Third plot #######################################
######################################################################################
setwd("~/Desktop/Politecnico di Milano/Luca Torriani - Project StatApp/RData")
setwd("C:/Users/User/OneDrive - Politecnico di Milano/Project StatApp/RData")

load("features_patients_reduced_9.RData")
load("features_patients_9.RData")
load("cst_list.RData")

source("/Users/ILARIASARTORI/Desktop/Poli/IVanno/CST_atlas/Clustering/helper_cluster.R")
source("/Users/ILARIASARTORI/Desktop/Poli/IVanno/CST_atlas/Clustering/helper_grafico3.R")
source("C:/Users/User/Programming/CST_atlas/Clustering/helper_grafico3.R")
source("C:/Users/User/Programming/CST_atlas/Clustering/helper_cluster.R")
source("C:/Users/User/Programming/CST_atlas/Clustering/helper_cluster_plot.R")


library(fiber)

indexes_plot3 = get_cluster_true_mean_indexes_healthy (features_reduced, features_list_sxdx, mean_left=mean_left, sd_left=sd_left, mean_right=mean_right, sd_right=sd_right)

indexes = indexes_plot3$final_indexes
labels_df = do.call(rbind, map(indexes,unlist))
df_left = labels_df[,c(1,2)]
df_right = labels_df[,c(3,4)]
stream_left = apply(df_left, 1, get_streamline, side = "left", cst_list)
stream_right = apply(df_right, 1, get_streamline, side = "right", cst_list)
streams = do.call(c,list(stream_left,stream_right))

colours =rainbow(9)
plot(streams[[1]], plot_microstructure = TRUE, col = colours[1], scale = 6) 
j= 2
for (i in 2:length(streams)){
  if(j==(length(streams)/2+1)) j = 1
  plot(streams[[i]], plot_microstructure = TRUE, new_window = F, col=colours[j], scale = 6)
  j =j+1
} 



