library(tidyverse)
library(devtools)
library(readr)
library(fdatractography)
library(boot)
library(gtools)
library(Gmedian)
library(sfsmisc)
library(cluster)


# setwd("C:/Users/Vale/Politecnico di Milano/Aymeric Stamm - fdatractography")
setwd("/Users/ILARIASARTORI/Google drive/Progetto_StatApp/File per paper/HICA")

### FA
load("tract_tot_FA.RData")

features = tract_tot_MD

pc.features <- princomp(features, scores=T)
summary(pc.features)

quartz()
layout(matrix(c(2,3,1,3),2,byrow=T))
# variance of PC
barplot(pc.features$sdev^2, las=2, main='Principal Components', ylim=c(0,0.3), ylab='Variances')
# original variances 
abline(h=1, col='blue')
barplot(sapply(as.data.frame(features),sd)^2, las=2, main='Original Variables', ylim=c(0,0.1), ylab='Variances')
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








### MD
load("tract_tot_MD.RData")

features = tract_tot_MD

pc.features <- princomp(features, scores=T)
summary(pc.features)

quartz()
layout(matrix(c(2,3,1,3),2,byrow=T))
# variance of PC
barplot(pc.features$sdev^2, las=2, main='Principal Components', ylim=c(0,max(pc.features$sdev^2)), ylab='Variances')
# original variances 
abline(h=1, col='blue')
barplot(sapply(as.data.frame(features),sd)^2, las=2, main='Original Variables', ylim=c(0,max(pc.features$sdev^2)), ylab='Variances')
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
