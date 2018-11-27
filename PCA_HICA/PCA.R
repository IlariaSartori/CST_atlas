library(tidyverse)
library(devtools)
library(readr)
library(fdatractography)
library(boot)
library(gtools)
library(Gmedian)
library(sfsmisc)
library(cluster)

plot_pca = function (features, pc.features) {
  quartz()
  layout(matrix(c(2,3,1,3),2,byrow=T))
  # variance of PC
  barplot(pc.features$sdev^2, las=2, main='Principal Components',ylim=c(0,max(pc.features$sdev^2)), ylab='Variances')  #  ylim=c(0,0.3), ## MD
  # original variances 
  abline(h=1, col='blue')
  barplot(sapply(as.data.frame(features),sd)^2, las=2, main='Original Variables',ylim=c(0,max(pc.features$sdev^2)), ylab='Variances')  #  ylim=c(0,0.1) ## MD
  # proportion of explained variance
  plot(cumsum(pc.features$sdev^2)/sum(pc.features$sde^2), type='b', axes=F, xlab='number of components', 
       ylab='contribution to the total variability', ylim=c(0,1))
  abline(h=0.6, lty=2, col='blue')
  box()
  axis(2,at=0:10/10,labels=0:10/10)
  axis(1,at=1:ncol(features),labels=1:ncol(features),las=1)
  
  ## loadings
  load.features    <- pc.features$loadings
  
  # Bar plot dei loadings PC; 
  quartz()
  par( mfrow = c(4,1), mar=c(0,3,0,3))
  for(i in 1:4) barplot(load.features[,i], ylim = c(-0.5, 0.5))
}


# setwd("C:/Users/Vale/Politecnico di Milano/Aymeric Stamm - fdatractography")
setwd("/Users/ILARIASARTORI/Politecnico di Milano/Luca Torriani - Project StatApp/RData")

### FA
load("tract_tot_FA.RData")
pc.featuresFA <- princomp(tract_tot_FA, scores=T)
summary(pc.featuresFA)
plot_pca(tract_tot_FA, pc.featuresFA)

### MD
load("tract_tot_MD.RData")
pc.featuresMD <- princomp(tract_tot_MD, scores=T)
summary(pc.featuresMD)
plot_pca(tract_tot_MD, pc.featuresMD)

### RD
load("tract_tot_RD.RData")
pc.featuresRD <- princomp(tract_tot_RD, scores=T)
summary(pc.featuresRD)
plot_pca(tract_tot_RD, pc.featuresRD)

### AD
load("tract_tot_AD.RData")
pc.featuresAD <- princomp(tract_tot_AD, scores=T)
summary(pc.featuresAD)
plot_pca(tract_tot_AD, pc.featuresAD)

