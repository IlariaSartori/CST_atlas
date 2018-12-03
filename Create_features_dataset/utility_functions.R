library(tidyverse)
library(devtools)
library(readr)
library(boot)
library(gtools)
library(Gmedian)
library(sfsmisc)
library(tidyr)


# ________________________________________________________________________________________________
# Getter diffusivity information GIA INCLUSE NEL PACCHETTO

# get_axial_diffusivity <- function(tensor)
# {
#   eigenvalues = eigen(tensor, symmetric = TRUE)$values
#   lambda_max = eigenvalues[1]
#   return (lambda_max)
# }
# 
# get_axial_diffusivity_streamline <- function(streamline, validate = TRUE)
# {
#   if (validate) {
#     if (!is_streamline(streamline))
#       stop("The input dataset is not of class streamline.")
#   }
#   return (map_dbl(streamline$diffusion, get_axial_diffusivity))
# }
# 
# 
# get_radial_diffusivity <- function(tensor)
# {
#   eigenvalues = eigen(tensor, symmetric = TRUE)$values
#   return ((eigenvalues[2]+eigenvalues[3])/2)
# }
# 
# get_radial_diffusivity_streamline <- function(streamline, validate = TRUE)
# {
#   if (validate) {
#     if (!is_streamline(streamline))
#       stop("The input dataset is not of class streamline.")
#   }
#   
#   return (map_dbl(streamline$diffusion, get_radial_diffusivity))
# }
# 
# get_mean_diffusivity_streamline <- function(streamline, validate = TRUE)
# {
#   if (validate) {
#     if (!is_streamline(streamline))
#       stop("The input dataset is not of class streamline.")
#   }
#   
#   return (map_dbl(streamline$diffusion, get_mean_diffusivity))
# }
# 
# get_fractional_anisotropy_streamline <- function(streamline, validate = TRUE)
# {
#   if (validate) {
#     if (!is_streamline(streamline))
#       stop("The input dataset is not of class streamline.")
#   }
#   
#   return (map_dbl(streamline$diffusion, get_fractional_anisotropy))
# }





# ________________________________________________________________________________________________
# Position information


# Barycenter:

# NEW 22/11/18
get_barycenter_streamline <- function(streamline, validate = TRUE) {
  # Given a streamline, it returns the 3D coordinates of the barycenter of the curve
  if (validate) {
    if (!is_streamline(streamline)) 
      stop("The input dataset is not of class streamline.")
  }
  streamline_coords = select(streamline, x, y, z)
  bar=colMeans(streamline_coords)
  bar=data_frame(x_Bar=bar[1], y_Bar=bar[2], z_Bar=bar[3])
  return (bar)
}

# NEW 22/11/18
get_barycenter <- function (streamlist) {
  # Creates the feature Barycenter of a give tract, whose streamlines are collected in data
  
  # bar=NULL
  # for (i in 1:length(streamlist)) {
  #   bar=rbind(bar, get_barycenter_streamline(streamlist[[i]]))
  # }
  bar = map_df(streamlist, get_barycenter_streamline)
  return (bar)
}



# Spatial Median:

# NEW 22/11/18
get_sp_median_streamline = function(streamline, validate = TRUE) {
  # Given a streamline, it returns the 3D coordinates of the spatial median of the curve
  if (validate) {
    if (!is_streamline(streamline)) 
      stop("The input dataset is not of class streamline.")
  }
  streamline_coords = select(streamline, x, y, z)
  spatial_med = Gmedian(streamline_coords)
  spatial_med = data_frame(x_SpMed=spatial_med[1], y_SpMed=spatial_med[2], z_SpMed=spatial_med[3])
  return(spatial_med)
}

# NEW 22/11/18
get_spatial_median = function (streamlist){
  # Creates the feature Spatial Median of a give tract, whose streamlines are collected in data
  # sp_median = NULL
  # for (i in 1:length(streamlist)){
  #   sp_median = rbind(sp_median,get_sp_median_streamline(streamlist[[i]][,2:4]))
  # }
  # sp_median = data.frame(x_SpMed = sp_median[,1],y_SpMed = sp_median[,2],z_SpMed = sp_median[,3] )
  sp_median = map_df(streamlist, get_sp_median_streamline)
  return (sp_median)
}


# Distance spatial_median - barycenter

# distance_centers <- function(data, barycenter, spatial_median){
#   # Returns the distance between the barycenters and the spatial medians of the streamlines
#   # of a given tract
#   dist = NULL
#   for (i in 1:length(data))
#   {
#     deltaX = (barycenter[i,1]-spatial_median$x_SpMed[i])^2
#     deltaY = (barycenter[i,2]-spatial_median$y_SpMed[i])^2
#     deltaZ = (barycenter[i,3]-spatial_median$z_SpMed[i])^2
#     dist = c(dist, sqrt(deltaX+deltaY+deltaZ))
#   }
#   return (dist)
# }


