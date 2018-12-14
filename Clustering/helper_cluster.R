
dataset_first_points = function(cst, ind_x, ind_y, ind_z){   # cst[[1]]= cst_left   cst[[2]]=cst_right
  dataset = NULL
  nl = length(cst$lhs$Streamlines)
  nr = length(cst$rhs$Streamlines)

  first_left = map_df(cst$lhs$Streamlines, slice,1) [c(ind_x, ind_y, ind_z)]   # Prendo le colonne x,y,z della prima riga
  first_right = map_df(cst$rhs$Streamlines, slice,1) [c(ind_x, ind_y, ind_z)]

  first_left[,1] = -first_left[,1]  # Proietto il tratto sinistro nel piano delle x positive
  tmp = rbind(first_left, first_right)
  
  side = rep(c("left","right"),c(nl,nr))
  patient = rep (as.numeric(cst$lhs$PatientId), nl+nr)
  dataset = cbind(tmp,side,patient)
  
  return(dataset)
}

add_cluster_column = function (data_first_pat_j, clusters, num_streamline_patients) {
  j = data_first_pat_j$patient[1]  # Indice del paziente: 0 per il malato e gli altri a seguire
  ################################### MANCA IL MALATO ###################################
  # if (j == 0 ) {
  #   num_previous_streamline = 0
  # }
  # else {
  #   num_previous_streamline = sum(num_streamline_patients[1:j])
  # }
  # clust = clusters [(num_previous_streamline+1) : (num_previous_streamline + num_streamline_patients[(j+1)])]
  # return (cbind(data_first_pat_j, clust))
  if (j == 1 ) {
    num_previous_streamline = 0
  }
  else {
    num_previous_streamline = sum(num_streamline_patients[1:(j-1)])
  }
  clust = clusters [(num_previous_streamline+1) : (num_previous_streamline + num_streamline_patients[(j)])]
  return (cbind(data_first_pat_j, clust))
  #######################################################################################
}


# dataset_first_points_old = function(cst_left, cst_right){
#   dataset = NULL
#   obs = paste("patient", cst_left$case)
#   nl = length(cst_left$data)
#   nr = length(cst_right$data)
#   scan = rep (obs, nl+nr)
#   first_right = map_df(cst_right$data, slice,1) [2:4]
#   first_left = map_df(cst_left$data, slice,1) [2:4]
#   first_left[,1] = -first_left[,1]  # Proietto il tratto sinistro nel piano delle x positive
#   side = rep(c("left","right"),c(nl,nr))
#   tmp = rbind(first_left, first_right)
#   dataset = cbind(tmp,side,scan)
# }


get_cluster_clara = function(X,n, PLOT = F){
  
  M <- colMeans(X[,1:3])
  S = cov(X[,1:3])
  
  PC <- princomp(X[,1:3])
  set.seed(1994)
  clara.obj = clara(PC$scores[,1],n)
  
  if(PLOT == T){
    open3d()
    plot3d(X[which(clara$clustering==1),], size=3, col=rainbow(n)[1], aspect = F)
    for (i in 2:n)
    {
      points3d(X[which(clara$clustering==i),], size = 3, col = rainbow(n)[i])
    }
    
  }
  return(clara.obj)
}

get_k_opt = function(data_big, n=22, treshold = 20, num_healty_patients){
  for(i in 1:n){
    clara = get_cluster_clara(data_big,i)
    cluster = clara$clustering
    new_data = cbind(data_big, cluster)
    # attach(new_data)
    # We stop when we find a healty patient for whom a cluster has a number of elements smaller than the threshold
    for(j in 1:num_healty_patients){
      check_sx = filter(new_data, side == "left", patient == j)$cluster
      # check_sx = new_data[which(side == "left"  &&  patient == j) ,6]
      num_sx = table(check_sx)
      check_sx = (unique(check_sx))
      # check_dx = new_data[which(side == "right" &&  patient == j) ,6]
      check_dx = filter(new_data, side == "left", patient == j)$cluster
      num_dx = table(check_dx)
      check_dx = unique(check_dx)
      if( sum(is.element(1:i,check_sx))<i ||    # Check if there is an empty cluster
          sum(is.element(1:i,check_dx))<i || 
          min(num_sx)< treshold || 
          min(num_dx)< treshold ) 
        {
          return(i-1)
        }
    }
  }
  return (n)
}

# get_k_opt = function(data_big, n=22, treshold = 20){
#   
#   for(i in 1:n){
#     clara = get_cluster_clara(data_big,i)
#     cluster = clara$clustering
#     new_data = cbind(data_big, cluster)
#     attach(new_data)
#     # We stop when we find a healty patient for whom a cluster is not present
#     for(j in 1:9){
#       check_sx = new_data[which(side == "left"  &  scan == paste("patient 0",j,sep="")) ,6]
#       num_sx = table(check_sx)
#       check_sx = (unique(check_sx))
#       check_dx = new_data[which(side == "right" &  scan == paste("patient 0",j,sep="")) ,6]
#       num_dx = table(check_dx)
#       check_dx = unique(check_dx)
#       if( sum(is.element(1:i,check_sx))<i || sum(is.element(1:i,check_dx))<i || min(num_sx)< treshold || min(num_dx)< treshold){
#         return(i-1) 
#       }
#     }
#     for(j in 10:20){
#       check_sx = new_data[which(side == "left" &  scan == paste("patient",j,sep=" ")) ,6]
#       num_sx = table(check_sx)
#       check_sx = (unique(check_sx))
#       check_dx =  new_data[which(side == "right" &  scan == paste("patient",j,sep=" ")) ,6]
#       num_dx = table(check_dx)
#       check_dx = (unique(check_dx))
#       if( sum(is.element(1:i,check_sx))<i || sum(is.element(1:i,check_dx))<i || min(num_sx)<treshold || min(num_dx)< treshold){
#         return(i-1) 
#       }
#     }
#   }
#   return (n)
# }

choose_k_sick = function (data, k_opt){
  b <- w <- NULL
  for(k in (k_opt-3):(k_opt+3)){
    result.k <- kmeans(data, k)
    w <- c(w, sum(result.k$wit)) # Equivalent to c(w,result.k$tot.wit)
    b <- c(b, result.k$bet)
  }
  quartz()
  # x11()
  matplot((k_opt-3):(k_opt+3), w/(w+b), pch='', xlab='clusters', ylab='within/tot', main='Choice of k', ylim=c(0,1))
  lines((k_opt-3):(k_opt+3), w/(w+b), type='b', lwd=2)
  
  quartz()
  # x11()
  matplot((k_opt-3):(k_opt+3), b/(w+b), pch='', xlab='clusters', ylab='between/tot', main='Choice of k', ylim=c(0,1))
  lines((k_opt-3):(k_opt+3), b/(w+b), type='b', lwd=2)
}

reproject_x = function (data_patient_j) {
  names_col = names (data_patient_j)
  ind_x = which(names_col=="x")
  ind_side = which(names_col=="side")
  data_patient_j = apply (data_patient_j, 1, change_sign_x_if_left, ind_x=ind_x, ind_side=ind_side)
  return (as.data.frame(t(data_patient_j)))
}

change_sign_x_if_left = function (row_data_patient_j, ind_x, ind_side) {
  if (row_data_patient_j[ind_side] == "left") 
    row_data_patient_j[ind_x] = - as.numeric(row_data_patient_j[ind_x])
  return(row_data_patient_j)
}

merge_left_right_features = function (features_patient_j) {
  return (rbind(features_patient_j[[1]], features_patient_j[[2]]))
}

get_reduced_tot = function(features, mean_left, sd_left, mean_right, sd_right){
  # features deve avere la colonna anche patient e cluster
  
  # left
  features_left = features[features$side == "left",]
  fac_left = factor(features_left$clust)
  n_left = length(levels(fac_left))
  tmp_sx = apply(features_left[,1:33], 2, tapply, fac_left, mean)
  
  # std_data_left = sweep(features_left[,1:33], 2, mean_left)  # Subtract the mean
  # std_data_left = sweep(std_data_left, 2, sd_left, FUN = "/")   # Divide by the standard deviation
  # tmp_var_sx = split(std_data_left, features_left$clust)
  # var_sx = map(tmp_var_sx, cov)
  tmp_var_sx = split(as.data.frame(scale(features_left[,1:33])), features_left$clust)
  var_sx = map(tmp_var_sx, cov)
  
  side_sx = rep("left", n_left)
  clust = levels(fac_left)
  patient =  rep(features_left$patient[1], n_left)
  centroids_left = data.frame(tmp_sx, side = side_sx, patient = patient,  clust = clust)
  
  # right
  features_right = features[features$side == "right",]
  fac_right = factor(features_right$clust)
  n_right = length(levels(fac_right))
  tmp_dx = apply(features_right[,1:33], 2, tapply, fac_right, mean)
  
  # std_data_right = sweep(features_right[,1:33], 2, mean_right)  # Subtract the mean
  # std_data_right = sweep(std_data_right, 2, sd_right, FUN = "/")   # Divide by the standard deviation
  # tmp_var_dx = split(std_data_right, features_right$clust)
  # var_dx = map(tmp_var_dx, cov)
  tmp_var_dx = split(as.data.frame(scale(features_right[,1:33])), features_right$clust)
  var_dx = map(tmp_var_dx, cov)
  
  side_dx = rep("right", n_right)
  clust = levels(fac_right)
  patient =  rep(features_right$patient[1], n_right)
  centroids_right = data.frame(tmp_dx, side = side_dx, patient = patient,  clust = clust)
  
  
  centroids = rbind(centroids_left, centroids_right)
  rownames(centroids) = 1:dim(centroids)[1]
  return(list(centroids = as.data.frame(centroids), var_sx = var_sx, var_dx = var_dx))
}

############## FIND REPRESENTATIVE

# find_representative_index = function(features_patient_j, centroids)
# {
#   cat("o")
#   j = features_patient_j$patient [1]
#   features_patient_left = (features_patient_j)[which(features_patient_j$side=="left"),]
#   features_patient_right = (features_patient_j)[which(features_patient_j$side=="right"),]
# 
#   centroid_patient_left = (centroids[[(j+1)]])[which(centroids[[(j+1)]]$side=="left"),]
#   centroid_patient_right = (centroids[[(j+1)]])[which(centroids[[(j+1)]]$side=="right"),]
#   
#   # Compute indexes of the representative streamlines of the left tract
#   col_cluster= which(names(centroid_patient_left)=="clust")
#   representative_indexes_left = as.numeric(apply(centroid_patient_left, 1, find_index_nearest_streamline, features_tot=features_patient_left, col_cluster))
#   # Compute indexes of the representative streamlines of the right tract
#   representative_indexes_right = as.numeric(apply(centroid_patient_right,1, find_index_nearest_streamline, features_tot=features_patient_right, col_cluster))
#   
#   representative_indexes_left = as.matrix( rbind(centroid_patient_left[,col_cluster],  
#                                                  representative_indexes_left) )
# 
#   representative_indexes_right = as.matrix( rbind(centroid_patient_right[,col_cluster],
#                                                   representative_indexes_right) )
#   
#   return (list( representative_indexes_left, representative_indexes_right ) )
# }




# centroid = Centro cluster k (sx o dx)
# features_tot = features paziente j (sx o dx)
# find_index_nearest_streamline = function(centroid, features_tot, col_cluster) {  
#   cluster_indexes = which(features_tot$clust==centroid[col_cluster])  
#   # Indici delle streamline del paziente j che appartengono al cluster k
#   
#   features_cluster = features_tot[cluster_indexes,] 
#   # Features delle streamline del paziente j che appartengono al cluster k
#   
#   distances = as.numeric(apply(features_cluster, 1, distance_from_centroid, centroid=centroid))
#   index_fc_representative = which.min(distances)
#   index_ft_representative = cluster_indexes[index_fc_representative]
#   
#   return (index_ft_representative)
# }

distance_from_centroid = function(features_row, centroid) {
  return( sqrt (sum ( ( as.numeric(features_row[1:33])- as.numeric(centroid[1:33]) )^2 ) ))
}

# find_representative = function (cst, ind_rep_streamlines) { # cst[[1]]=cst_left, cst[[2]]=cst_right
#   j =  as.numeric(cst[[1]]$case)
#   n_points= dim(cst[[1]]$data[[1]])[1]
#   
#   index_left = ind_rep_streamlines[[(j+1)]] [[1]][2,]  
#   streamline_left = list()
#   count=1
#   for (i in index_left) {
#     streamline = cbind(cst[[1]]$data[[i]],     # Devo mettere as.data.frame davanti?
#                        side = rep("left", n_points), 
#                        clust= rep(ind_rep_streamlines[[(j+1)]][[1]][1,count], n_points))
#     streamline_left = rbind(streamline_left, list(streamline))
#     count=count+1
#   }
#   
#   index_right = ind_rep_streamlines[[(j+1)]][[2]][2,]
#   streamline_right = list()
#   count=1
#   for (i in index_right) {
#     streamline = cbind(cst[[2]]$data[[i]], 
#                        side = rep("right", n_points), 
#                        clust= rep(ind_rep_streamlines[[(j+1)]][[2]][1,count], n_points))
#     streamline_right = c(streamline_right, list(streamline))
#     count=count+1
#   }
#   
#   return (c(streamline_left, streamline_right))
# }


## Plot representative
# plot_representatives = function(indexes_representative_streamline, cst_list){
#   open3d()
#   primo=1
#   n_cluster = dim(indexes_representative_streamline[[1]][[1]])[2]
#   for (i in 1:length(cst_list)){   # Per ogni paziente
#     for (s in 1:2){   # Left e right
#       for (j in 1:n_cluster){   # Per ogni cluster
#         k = indexes_representative_streamline[[i]][[s]][2,j]
#         x= cst_list[[i]][[s]]$data[[k]]$x
#         y= cst_list[[i]][[s]]$data[[k]]$y
#         z= cst_list[[i]][[s]]$data[[k]]$z
#         
#         X=cbind(x,y,z)
#         
#         lines3d(X, asp=1, size=0.5, col=rainbow(n_cluster)[j]) 
#         
#         if (primo) {
#           axes3d()
#           title3d(xlab='x', ylab='y', zlab='z')
#           primo=0
#         }
#       }
#     }
#   }
# }


# representative_streamlines lista di 20. Ogni elemento è una lista di 9 dataset di streamline (sx e dx insieme)
# plot_streamlines_from_list = function (representative_streamlines) {
#   open3d()
#   primo=1
#   n_cluster = length(representative_streamlines[[1]])/2
#   for (i in 1:length(representative_streamlines)){   # Per ogni paziente
#     for (j in 1:length(representative_streamlines[[i]])){   
#       x= representative_streamlines[[i]][[j]]$x
#       y= representative_streamlines[[i]][[j]]$y
#       z= representative_streamlines[[i]][[j]]$z
#       
#       X=cbind(x,y,z)
#       
#       lines3d(X, asp=1, size=0.5, col=rainbow(n_cluster)[as.numeric(representative_streamlines[[i]][[j]]$clust)]) 
#       
#       if (primo) {
#         axes3d()
#         title3d(xlab='x', ylab='y', zlab='z')
#         primo=0
#       }
#     }
#   }
#   
# }

### Per ogni cluster rappresento la streamline fittizia data dalla media delle features nei 20 rappresentanti di quel cluster

# plot_representatives_mean = function(indexes_representative_streamline, cst_list){
#   open3d()
#   primo=1
#   n_cluster = dim(indexes_representative_streamline[[1]][[1]])[2]
#   for (j in 1:n_cluster){
#     for (s in 1:2){
#       X = NULL
#       Y = NULL
#       Z = NULL
#       for (i in 1:length(cst_list)){
#         k = indexes_representative_streamline[[i]][[s]][2,j]
#         x= cst_list[[i]][[s]]$data[[k]]$x
#         y= cst_list[[i]][[s]]$data[[k]]$y
#         z= cst_list[[i]][[s]]$data[[k]]$z
#         X = rbind(X, x)
#         Y = rbind(Y, y)
#         Z = rbind(Z, z)
#       }
#       
#       x = colMeans(X)
#       y = colMeans(Y)
#       z = colMeans(Z)
#       
#       X = cbind(x,y,z)
#       
#       lines3d(X, asp=1, size=0.5, col=rainbow(n_cluster)[j],axes=FALSE) 
#       
#       if (primo) {
#         axes3d()
#         title3d(xlab='x', ylab='y', zlab='z')
#         primo=0
#       }
#     }
#   }
# }


find_old_representative_index = function (indexes_matrix) {
  for (j in 1:dim(indexes_matrix[[1]])[2]) {  # Per ogni cluster sinistro
    indexes_matrix [[1]] [2,j] = indexes_matrix[[3]] [indexes_matrix [[1]] [2,j]]   # Indici rappresentanti
  }
  for (j in 1:dim(indexes_matrix[[2]])[2]) {  # Per ogni cluster destro
    indexes_matrix [[2]] [2,j] = indexes_matrix[[4]] [indexes_matrix [[2]] [2,j]]   # Indici rappresentanti
  }
  return ( list(indexes_matrix[[1]], indexes_matrix[[2]]))
}


old_indixes_of_no_outliers = function (orig_out_indexes) {
  return ((1:orig_out_indexes[[1]]) [-orig_out_indexes[[2]]] )
}

num_of_streamline_patient = function (cst) {
  return (num_of_streamline_tract(cst$lhs) + num_of_streamline_tract(cst$rhs))
}

num_of_streamline_tract = function (tract) {
  return (length(tract$Streamlines))
}

# NEW: 20/11/18 (non usata)
num_of_streamline_tract_sx_dx = function (tract_sx_dx) {  # tract_sx_dx[[1]]= tract_left  tract_sx_dx[[2]]= tract_right
  return (c(length(tract_sx_dx[[1]]$data), length(tract_sx_dx[[2]]$data)))
}

# NEW: 21/11/18 
num_of_streamline_from_tensor_list_cst = function (tensor_list_cst) { 
  return (c(length(tensor_list_cst$lhs), length(tensor_list_cst$rhs)))
}

