library(tidyverse)
library(devtools)
library(readr)
library(fdatractography)
library(boot)
library(gtools)
library(Gmedian)
library(sfsmisc)
library(cluster)

#################################################################
################## CURVATURA e TORSIONE #########################
#################################################################

########################################################################################         
####################  Calcolo lambda ottimo usando il primo CST ########################  
########################################################################################         

# setwd("/Users/ILARIASARTORI/Google drive/Progetto_StatApp/File per paper/RData_tratti_riparametrizzati")
# setwd("/Users/ILARIASARTORI/Desktop")

# load("cst01_rep_no_outliers.RData")

# Calcolo lambda ottimo usando il primo CST
# library(fda)
# library(rgl)
# tract=cst01_left
# cat("Every 'o' is a streamline \n")
# lambda_opt_vec = map_dbl(tract$data, fda3D_evaluate_lambda)  
# lambda_opt=median(lambda_opt_vec)   # 3.187206 (con optim)   3.042841 (con optimize)



########################################################################################         
###################################  Test SMOOTHING ####################################  
########################################################################################         

# load("cst04_rep_no_outliers.RData")
# 
# library(fda)
# library(rgl)
# m=8           # spline order     #### CAMBIO QUI L'ORDINE DELLE SPLINE
# degree=m-1    # spline degree 
# Lfdobj = 4                       #### CAMBIO QUI L'ORDINE DELLA DERIVATA DA PENALIZZARE
# 
# n_points=50
# 
# i=150
# lambda = lambda_opt
# 
# s <- cst04_left$data[[i]]$s
# x <- cst04_left$data[[i]]$x
# y <- cst04_left$data[[i]]$y
# z <- cst04_left$data[[i]]$z
# 
# res <- get_functional_representation_lambda_fixed(cst04_left$data[[i]], norder = m, Lfdobj = 4, lambda)
# Obs1_left = eval.fd(s, as.fd(res$fd), Lfd=1)
# Obs2_left = eval.fd(s, as.fd(res$fd), Lfd=2)
# Obs3_left = eval.fd(s, as.fd(res$fd), Lfd=3)
# 
# grafici=TRUE
# grafici=FALSE
# if (grafici) {
#   
#   ## Grafico funzione 
#   Obs0_left = eval.fd(s, as.fd(res$fd), Lfd=0)
#   plot3d (Obs0_left, type='l')
#   points3d (x, y, z, col='red')
#   
#   ## Grafico derivata prima
#   ss=seq(range(s)[1], range(s)[2], length.out=1000)
#   hh = (max(s)-min(s))/(n_points-1)
#   
#   Obs1_left = eval.fd(ss, as.fd(res$fd), Lfd=1)
#   # Differenze finite
#   dd1_x_left = (dplyr::lead(x) - dplyr::lag(x))/(2*hh)
#   dd1_y_left = (dplyr::lead(y) - dplyr::lag(y))/(2*hh)
#   dd1_z_left = (dplyr::lead(z) - dplyr::lag(z))/(2*hh)
#   
#   quartz()
#   plot(s[2:(n_points-1)],dd1_x_left[2:(n_points-1)],type='l',xlab="s",ylab="observed data", main='Derivata prima_x')
#   lines(ss,Obs1_left[,1], col='red')
#   quartz()
#   plot(s[2:(n_points-1)],dd1_y_left[2:(n_points-1)],type='l',xlab="s",ylab="observed data", main='Derivata prima_y')
#   lines(ss,Obs1_left[,2], col='red')
#   quartz()
#   plot(s[2:(n_points-1)],dd1_z_left[2:(n_points-1)],type='l',xlab="s",ylab="observed data", main='Derivata prima_z')
#   lines(ss,Obs1_left[,3], col='red')
#   
#   
#   ## Grafico derivata seconda
#   Obs2_left = eval.fd(ss, as.fd(res$fd), Lfd=2)
#   
#   dd2_x_left = (dplyr::lead(x) + dplyr::lag(x) - 2 * x)/(hh^2)
#   dd2_y_left = (dplyr::lead(y) + dplyr::lag(y) - 2 * y)/(hh^2)
#   dd2_z_left = (dplyr::lead(z) + dplyr::lag(z) - 2 * z)/(hh^2)
#   
#   quartz()
#   plot(s[2:(n_points-1)],dd2_x_left[2:(n_points-1)],type='l',xlab="s",ylab="observed data", main='Derivata seconda_x', ylim=c(-0.2,0.2))
#   lines(ss,Obs2_left[,1], col='red')
#   quartz()
#   plot(s[2:(n_points-1)],dd2_y_left[2:(n_points-1)],type='l',xlab="s",ylab="observed data", main='Derivata seconda_y', ylim=c(-0.2,0.2))
#   lines(ss,Obs2_left[,2], col='red')
#   quartz()
#   plot(s[2:(n_points-1)],dd2_z_left[2:(n_points-1)],type='l',xlab="s",ylab="observed data", main='Derivata seconda_z', ylim=c(-0.2,0.2))
#   lines(ss,Obs2_left[,3], col='red')
#   
#   ## Grafico derivata terza
#   Obs3_left = eval.fd(ss, as.fd(res$fd), Lfd=3)
#   
#   dd3_x_left = ((dplyr::lead(x,2) -2*dplyr::lead(x) + 2*dplyr::lag(x)- dplyr::lag(x,2)))/(2*hh^3)
#   dd3_y_left = ((dplyr::lead(y,2) -2*dplyr::lead(y) + 2*dplyr::lag(y)- dplyr::lag(y,2)))/(2*hh^3)
#   dd3_z_left = ((dplyr::lead(z,2) -2*dplyr::lead(z) + 2*dplyr::lag(z)- dplyr::lag(z,2)))/(2*hh^3)
#   
#   quartz()
#   plot(s[3:(n_points-2)],dd3_x_left[3:(n_points-2)],type='l',xlab="s",ylab="observed data", main='Derivata terza_x', ylim=c(-0.2,0.2))
#   lines(ss,Obs3_left[,1], col='red')
#   quartz()
#   plot(s[3:(n_points-2)],dd3_y_left[3:(n_points-2)],type='l',xlab="s",ylab="observed data", main='Derivata terza_y', ylim=c(-0.2,0.2))
#   lines(ss,Obs3_left[,2], col='red')
#   quartz()
#   plot(s[3:(n_points-2)],dd3_z_left[3:(n_points-2)],type='l',xlab="s",ylab="observed data", main='Derivata terza_z', ylim=c(-0.2,0.2))
#   lines(ss,Obs3_left[,3], col='red')
# }
# 
# # Calcolo vettore curvature
# curvatura = (sqrt((Obs2_left[,3]*Obs1_left[,2] - Obs2_left[,2]*Obs1_left[,3])^2 + (Obs2_left[,1]*Obs1_left[,3] - Obs2_left[,3]*Obs1_left[,1])^2 + (Obs2_left[,2]*Obs1_left[,1] - Obs2_left[,1]*Obs1_left[,2])^2))/(Obs1_left[,1]^2 + Obs1_left[,2]^2 + Obs1_left[,3]^2)^(3/2)
# torsione = (Obs3_left[,1]*(Obs1_left[,2]*Obs2_left[,3] - Obs2_left[,2]*Obs1_left[,3]) + Obs3_left[,2]*(Obs2_left[,1]*Obs1_left[,3] - Obs1_left[,1]*Obs2_left[,3]) + Obs3_left[,3]*(Obs1_left[,1]*Obs2_left[,2] - Obs2_left[,1]*Obs1_left[,2]))/((Obs1_left[,2]*Obs2_left[,3] - Obs2_left[,2]*Obs1_left[,3])^2 + (Obs2_left[,1]*Obs1_left[,3] - Obs1_left[,1]*Obs2_left[,3])^2 + (Obs1_left[,1]*Obs2_left[,2] - Obs2_left[,1]*Obs1_left[,2])^2)
############




########## Funzioni

# FIXED LAMBDA
get_curvature_torsion_fda3D_lambda_fixed = function (streamline, m=8, Lfdobj=4, lambda) {
  cat("o")
  s <- streamline$s
  
  res <- get_functional_representation_lambda_fixed(streamline, norder = m, Lfdobj = Lfdobj, lambda)
  Obs1_left = eval.fd(s, as.fd(res$fd), Lfd=1)
  Obs2_left = eval.fd(s, as.fd(res$fd), Lfd=2)
  Obs3_left = eval.fd(s, as.fd(res$fd), Lfd=3)
  
  # Calcolo vettore curvature
  curvatura = (sqrt(  (Obs2_left[,3]*Obs1_left[,2] - Obs2_left[,2]*Obs1_left[,3])^2 + 
                        (Obs2_left[,1]*Obs1_left[,3] - Obs2_left[,3]*Obs1_left[,1])^2 + 
                        (Obs2_left[,2]*Obs1_left[,1] - Obs2_left[,1]*Obs1_left[,2])^2) )   /
    (Obs1_left[,1]^2 + Obs1_left[,2]^2 + Obs1_left[,3]^2)^(3/2)
  torsione = (  Obs3_left[,1]*(Obs1_left[,2]*Obs2_left[,3] - Obs2_left[,2]*Obs1_left[,3]) + 
                  Obs3_left[,2]*(Obs2_left[,1]*Obs1_left[,3] - Obs1_left[,1]*Obs2_left[,3]) + 
                  Obs3_left[,3]*(Obs1_left[,1]*Obs2_left[,2] - Obs2_left[,1]*Obs1_left[,2]))   /
    (  (Obs1_left[,2]*Obs2_left[,3] - Obs2_left[,2]*Obs1_left[,3])^2 + 
         (Obs2_left[,1]*Obs1_left[,3] - Obs1_left[,1]*Obs2_left[,3])^2 + 
         (Obs1_left[,1]*Obs2_left[,2] - Obs2_left[,1]*Obs1_left[,2])^2 )
  return (c(max(curvatura), mean(curvatura), sd(curvatura), max(torsione), mean(torsione), sd(torsione), res$lambda)) 
}

get_functional_representation_lambda_fixed <- function(streamline,norder=8, Lfdobj=4, lambda) {
  s <- streamline$s   # Uso come breaks i punti stessi
  basis <- create.bspline.basis(s, norder = norder)
  Y <- array(dim = c(length(s), 1, 3))
  Y[, , 1] <- streamline$x
  Y[, , 2] <- streamline$y
  Y[, , 3] <- streamline$z

  optFDPar <- fdPar(fdobj = basis, Lfdobj = Lfdobj, lambda = lambda)
  res <- smooth.basis(s, Y, optFDPar)
  
  list(fd = res, lambda = lambda)
}


# VARIABLE LAMBDA
fda3D_evaluate_lambda = function (streamline, m=8, Lfdobj=4, lambda0=10) {
  cat("o")
  s <- streamline$s
  
  res <- get_functional_representation_evaluate_lambda(streamline, norder = m, Lfdobj = Lfdobj, lambda0 = lambda0)
  Obs1_left = eval.fd(s, as.fd(res$fd), Lfd=1)
  Obs2_left = eval.fd(s, as.fd(res$fd), Lfd=2)
  Obs3_left = eval.fd(s, as.fd(res$fd), Lfd=3)
  
  return (res$lambda) 
}

get_functional_representation_evaluate_lambda <- function(streamline,norder=8, Lfdobj=4, lambda0=10) {
  s <- streamline$s   # Uso come breaks i punti stessi
  basis <- create.bspline.basis(s, norder = norder)
  Y <- array(dim = c(length(s), 1, 3))
  Y[, , 1] <- streamline$x
  Y[, , 2] <- streamline$y
  Y[, , 3] <- streamline$z
  
  # lambda = optim(lambda0, gcv_lambda, basis=basis, s=s, Y=Y, Lfdobj=Lfdobj, method="Brent",lower = 10^(-6), upper = 50)
  lambda = optimize (gcv_lambda, c(10^(-6),50), basis=basis, s=s, Y=Y, Lfdobj=Lfdobj, maximum = FALSE)
  # lambda = optim(lambda0, gcv_lambda, basis=basis, Lfdobj=Lfdobj)

  optFDPar <- fdPar(fdobj = basis, Lfdobj = Lfdobj, lambda = lambda$minimum)
  res <- smooth.basis(s, Y, optFDPar)

  list(fd = res, lambda = lambda$minimum)
}

gcv_lambda = function (lambda, basis, Lfdobj, s, Y) {
  functionalPar <- fdPar(fdobj = basis, Lfdobj = Lfdobj, lambda = lambda)
  return (mean(smooth.basis(s, Y, functionalPar)$gcv))
}


# get_curvature_torsion_fda3D = function (streamline, m=8, Lfdobj=4, lambda0=10) {
#   cat("o")
#   s <- streamline$s
#   
#   res <- get_functional_representation(streamline, norder = m, Lfdobj = Lfdobj, lambda0 = lambda0)
#   Obs1_left = eval.fd(s, as.fd(res$fd), Lfd=1)
#   Obs2_left = eval.fd(s, as.fd(res$fd), Lfd=2)
#   Obs3_left = eval.fd(s, as.fd(res$fd), Lfd=3)
#   
#   # Calcolo vettore curvature
#   curvatura = (sqrt(  (Obs2_left[,3]*Obs1_left[,2] - Obs2_left[,2]*Obs1_left[,3])^2 + 
#                       (Obs2_left[,1]*Obs1_left[,3] - Obs2_left[,3]*Obs1_left[,1])^2 + 
#                       (Obs2_left[,2]*Obs1_left[,1] - Obs2_left[,1]*Obs1_left[,2])^2) )   /
#               (Obs1_left[,1]^2 + Obs1_left[,2]^2 + Obs1_left[,3]^2)^(3/2)
#   torsione = (  Obs3_left[,1]*(Obs1_left[,2]*Obs2_left[,3] - Obs2_left[,2]*Obs1_left[,3]) + 
#                 Obs3_left[,2]*(Obs2_left[,1]*Obs1_left[,3] - Obs1_left[,1]*Obs2_left[,3]) + 
#                 Obs3_left[,3]*(Obs1_left[,1]*Obs2_left[,2] - Obs2_left[,1]*Obs1_left[,2]))   /
#              (  (Obs1_left[,2]*Obs2_left[,3] - Obs2_left[,2]*Obs1_left[,3])^2 + 
#                 (Obs2_left[,1]*Obs1_left[,3] - Obs1_left[,1]*Obs2_left[,3])^2 + 
#                 (Obs1_left[,1]*Obs2_left[,2] - Obs2_left[,1]*Obs1_left[,2])^2 )
#   return (c(max(curvatura), mean(curvatura), sd(curvatura), max(torsione), mean(torsione), sd(torsione))) 
# }
# 
