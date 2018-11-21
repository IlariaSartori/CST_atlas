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
load("../RData/cst_list.RData")
### Aggiungo a mano il cst del malato (poi quello definitivo va inserito nella cst_list)
load("../RData/cst00_rep_no_outliers.RData")
cst00 = list(cst00_left, cst00_right)
cst_list = c(list(cst00), cst_list) 
# Lista di 21 elementi. 
# Ogni elemento rappresenta un paziente (il primo è il paziente malato e poi in ordine da 1 a 20)
# Ogni elemento (i.e paziente) è rappresentato da una lista di 2 elementi: il cst sinistro e il destro

source("helper_cluster.R")

num_streamline_patients = map_dbl (cst_list, num_of_streamline_patient)   # !! Senza outliers!!!!! 
# Per ogni paziente somma numero streamline destro+sinistro

################################################################################################
############################ Creare dataset primi punti ########################################
################################################################################################
# names_col = names(cst_list[[1]][[1]]$data[[1]])
# ind_x = which(names_col=="x")
# ind_y = which(names_col=="y")
# ind_z = which(names_col=="z")
# data_first_points = map(cst_list, dataset_first_points, ind_x, ind_y, ind_z) 
# # Mappa di 21 elementi. Ogni elemento è il dataset dei punti iniziali delle streamline di quel paziente
# 
# setwd("C:/Users/User/Google Drive/Progetto_StatApp/File per paper")
# save(data_first_points, file = "data_first_points.RData")

#### Caricare dataset primi punti (data_first_points)
load("../RData/data_first_points.RData")    # Lista dataset primi punti

################################################################################################
######################################## Clustering ############################################
################################################################################################
data_first_points_sick_matrix = data_first_points[[1]]

data_first_points_healty_matrix = NULL  # Metto i punti iniziali dei sani insieme in una matrice
for (i in 2:length(data_first_points)) {
  data_first_points_healty_matrix = rbind (data_first_points_healty_matrix, data_first_points[[i]])
}


#### Cluster sani
n=50
treshold = 20  
num_healty_patients = 20
k_opt = get_k_opt(data_first_points_healty_matrix, n, treshold, num_healty_patients) ##
clara = get_cluster_clara(data_first_points_healty_matrix, k_opt)  # prima di clara c'è un set.seed(1994)
cluster_healty = clara$clustering

#### Cluster malato
choose_k_sick(data_first_points_sick_matrix[,1:3], k_opt)  # Mi da errore non converge?
k_sick = 6 #???? DA SISTEMARE, è per fare una prova
k_means_sick = kmeans(data_first_points_sick_matrix[,1:3], k_sick)
cluster_sick = k_means_sick$cluster

####  In data_first_points aggiungo la colonna con i cluster di appartenenza
data_first_points = map(data_first_points, add_cluster_column, clusters = c(cluster_sick, cluster_healty), num_streamline_patients=num_streamline_patients)

#### Riproietto il tratto sinistro nel piano delle x negative
data_first_points = map(data_first_points, reproject_x)

################################################################################################
############################### Creo features_patients_9_6.RData ###############################
################################################################################################
# ### Loading features
# load("../RData/features_list.RData")
# ### Aggiungo a mano le features del malato (poi quello definitivo va inserito nella features_list)
# load("cst0_features.RData")
# features00 = list(cst0_left_features, cst0_right_features)
# features_list = c(list(features00), features_list) 
# # Lista di 21 elementi. 
# # Ogni elemento rappresenta un paziente (il primo è il paziente malato e poi in ordine da 1 a 20)
# # Ogni elemento (i.e paziente) è rappresentato da una lista di 2 elementi: le features sinistre e le destre
# 
# # Lista di 21 elementi. Ogni elemento è un dataframe con le features delle varie streamline di quel paziente,
# # prima quelle sinistre e poi le destre
# features_list_sxdx = map (features_list, merge_left_right_features)
# 
# # Aggiungo gli indici del cluster
# features_list_sxdx = map (features_list_sxdx, add_cluster_column, clusters = c(cluster_sick, cluster_healty), num_streamline_patients=num_streamline_patients)  # features_final
# 
# setwd("/Users/ILARIASARTORI/Google drive/Progetto_StatApp/File per paper/Rdata_clustering")
# save (features_list_sxdx, file="features_patients_9_6.RData")


################################################################################################
########################## Creo features_patients_reduced_9_6.RData ############################
################################################################################################
# # Calcolo i centroidi
# features_reduced = map (features_list_sxdx, get_reduced_tot)
# 
# setwd("/Users/ILARIASARTORI/Google drive/Progetto_StatApp/File per paper/Rdata_centroidi")
# save (features_reduced, file="features_patients_reduced_9_6.RData")




######################################################################################
########################## Primo grafico #############################################
######################################################################################

load("../RData/features_patients_9_6.RData")
load("../RData/features_patients_reduced_9_6.RData")

source("helper_cluster.R")
source("helper_cluster_plot.R")

# Loading tracts
load("../RData/cst_list.RData")
### Aggiungo a mano il cst del malato (poi quello definitivo va inserito nella cst_list)
load("../RData/cst00_rep_no_outliers.RData")
cst00 = list(cst00_left, cst00_right)
cst_list = c(list(cst00), cst_list)

# tolgo il malato
features_reduced_healty = features_reduced[-1]
features_list_sxdx_healty = features_list_sxdx[-1]
cst_list_healty = cst_list[-1]

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
load("../RData/features_patients_reduced_9_6.RData")
load("../RData/features_patients_9_6.RData")

source("helper_cluster.R")
source("helper_cluster_plot.R")

# Loading tracts
load("cst_list.RData")
### Aggiungo a mano il cst del malato (poi quello definitivo va inserito nella cst_list)
load("cst00_rep_no_outliers.RData")
cst00 = list(cst00_left, cst00_right)
cst_list = c(list(cst00), cst_list)

# tolgo il malato
features_reduced_healty = features_reduced[-1]
features_list_sxdx_healty = features_list_sxdx[-1]
cst_list_healty = cst_list[-1]

indexes_plot2 = get_cluster_true_mean_indexes_healty (features_reduced_healty, features_list_sxdx_healty)

plot_from_indexes (cst_list_healty, indexes_plot2)


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

