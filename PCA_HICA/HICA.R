get_HICA_loadings <- function (dataset, K, title, coda = FALSE)
{
  
  if (coda)
  {
    dataset = dataset[,26:50]
    lev = 24
  }
  
  else 
  {
    lev = 49
  }
  
  
  library(fastHICA)
  
  basis <- basis_hica(as.matrix(dataset))
  energy <- energy_hica(basis, maxcomp = K)
  hica <- extract_hica(energy, comp = K, level=lev)
  
  
  hica.load <- hica$C
  rownames(hica.load) = as.character(c(1:50))
  
  # x11()
  quartz()
  par(mar = c(1,4,0,2), oma=c(0,0,3,0), mfrow = c(K,1))
  for(i in 1:K)
  {
    barplot(hica.load[,i], ylim = c(-1, 1), col=rainbow(lev+1))
    abline(h=0)
  }
  title(paste("HICA for ", title), outer = T)  
  
}


setwd("/Users/ILARIASARTORI/Politecnico di Milano/Luca Torriani - Project StatApp/RData")

##### MD
load("tract_tot_MD.RData")
# Plot loadings
get_HICA_loadings(tract_tot_MD, 4, "Mean Diffusivity tracts")

##### AD
load("tract_tot_AD.RData")
# Plot loadings
get_HICA_loadings(tract_tot_AD, 4, "Axial Diffusivity tracts")


##### RD
load("tract_tot_RD.RData")
# Plot loadings
get_HICA_loadings(tract_tot_RD, 4, "Radial Diffusivity tracts")


##### FA
load("tract_tot_FA.RData")
# Plot loadings
get_HICA_loadings(tract_tot_FA, 4, "Fractional Anisotropy tracts")
