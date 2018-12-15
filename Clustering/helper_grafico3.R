

shift_index = function(patient, idx_streamline, idx_ouliers_patient, num_stream_pat_side){
  vec_of_indexes_tot = rep(0, num_stream_pat_side) 
  vec_of_indexes_tot[-idx_ouliers_patient] = 1:(num_stream_pat_side-length(idx_ouliers_patient))
  idx = which(vec_of_indexes_tot%in%idx_streamline==TRUE)
  return (data.frame(idx, patient))
}

# NEW: 21/11/18 (corretto uso pmap_dfr)
find_original_index_representative_cluster = function (indexes_plot1_cluster, idx_outlier_left, idx_outlier_right, num_stream_pat_side) {
  ## Left
  patient_left = as.list(indexes_plot1_cluster$lhs$patient)
  idx_left = as.list(indexes_plot1_cluster$lhs$idx)
  left_shifted = pmap_dfr(list(patient_left, idx_left, idx_outlier_left, num_stream_pat_side$lhs), shift_index)
  
  ## Right
  patient_right = as.list(indexes_plot1_cluster$rhs$patient)
  idx_right = as.list(indexes_plot1_cluster$rhs$idx)
  right_shifted = pmap_dfr(list(patient_right, idx_right, idx_outlier_right, num_stream_pat_side$rhs), shift_index)
  
  return (list(lhs = list(idx = left_shifted$idx, patient = left_shifted$patient),
               rhs= list(idx = right_shifted$idx, patient = right_shifted$patient)))
}

# Restituisce indici nella stessa forma di indexes_plot1, ma shiftati
find_original_index_representative = function (indexes_plot1, idx_outlier_left, idx_outlier_right, num_stream_pat_side) {
  old_index = map(indexes_plot1, find_original_index_representative_cluster, 
                  idx_outlier_left = idx_outlier_left, idx_outlier_right = idx_outlier_right, num_stream_pat_side = num_stream_pat_side)
  return(old_index)
}


mean_tensors = function(old_indexes, list_tensors){
  return (map(old_indexes, mean_cluster, list_tensors = list_tensors))
}


# old_indexes_cluster_side lista (idx, patient)
mean_cluster = function(old_indexes_cluster, list_tensors){
  p = dim(list_tensors[[1]]$lhs[[1]][[1]])[1]
  
  # LHS
  matrix = matrix(0,length(list_tensors[[1]]$lhs[[1]]),p*(p-1)/2)
  for(i in 1:length(old_indexes_cluster$lhs$patient)){
    matrix = matrix + map_dfr(list_tensors[[old_indexes_cluster$lhs$patient[i]]]$lhs[[old_indexes_cluster$lhs$idx[i]]],
                              matrix_to_vec)
  }
  mean_matrix_left = matrix/length(old_indexes_cluster$lhs$patient)
  mean_matrix_left = plyr::alply(mean_matrix_left, 1, vec_to_matrix)
  
  
  # RHS
  matrix = matrix(0,length(list_tensors[[1]]$rhs[[1]]),p*(p-1)/2)
  for(i in 1:length(old_indexes_cluster$rhs$patient)){
    matrix = matrix + map_dfr(list_tensors[[old_indexes_cluster$rhs$patient[i]]]$rhs[[old_indexes_cluster$rhs$idx[i]]],
                              matrix_to_vec)
  }
  mean_matrix_right = matrix/length(old_indexes_cluster$rhs$patient)
  mean_matrix_right = plyr::alply(mean_matrix_right, 1, vec_to_matrix)
  return(list(lhs = mean_matrix_left, rhs = mean_matrix_right))
}



### Arguments:
# - D: nxn matrix (in Sym(p))
### Value: n*(n+1)/2 vector with the elements of the upper triangual part of D (diagonal included), saved row-wise
matrix_to_vec = function(D){
  d = rep(0,dim(D)[1]*(dim(D)[1]+1)/2) 
  k = 1
  for (i in 1:dim(D)[1]){
    for (j in i:dim(D)[1]){
      d[k] = D[i,j]
      k = k+1
    }
  }
  return  (d)
}

### Arguments:
# - d: n*(n+1)/2 vector 
### Value: nxn matrix (in Sym(p)). The symmetric matrix obtained putting, row-wise, in the upper triangular part (diagonal included) the elements of d
vec_to_matrix = function(d){
  n = (sqrt(8*length(d)+1)-1)/2
  D = matrix(0,n,n)
  k = 1
  for (i in 1:n){
    for (j in i:n){
      D[i,j] = d[k]
      D[j,i] = d[k]
      k = k+1
    }
  }
  return (D)
}


plot_from_indexes = function (cst_list_healty, indexes_plot) {
  open3d()
  primo=1
  n_cluster = length(indexes_plot)
  for (j in 1:n_cluster){
    for (s in 1:2){   # Left e right
      patient = indexes_plot[[j]][[s]]$patient
      index = indexes_plot[[j]][[s]]$idx
      for (i in 1: length(patient)) {
        x= cst_list_healty[[patient[i]]][[s]]$data[[index[i]]]$x
        y= cst_list_healty[[patient[i]]][[s]]$data[[index[i]]]$y
        z= cst_list_healty[[patient[i]]][[s]]$data[[index[i]]]$z
        
        X = cbind(x,y,z)
        
        lines3d(X, asp=1, size=0.5, col=rainbow(n_cluster)[j],axes=FALSE)
        
        if (primo) {
          axes3d()
          title3d(xlab='x', ylab='y', zlab='z')
          primo=0
        }
      }
    }
  }
}

# NEW : 14/12/18
get_streamline = function(row_df, side, cst_list){
  pat = row_df[2]
  idx = row_df[1]
  if(side == "left")  stream = cst_list[[pat]]$lhs$Streamlines[[idx]]
  else if (side == "right") stream = cst_list[[pat]]$rhs$Streamlines[[idx]]
  else stop("Error side")
  return (stream)
}