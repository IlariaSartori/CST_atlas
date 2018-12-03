compute_streamline_features = function (streamline) {
  curv_torsNOSTRA = get_curvature_torsion_fda3D_lambda_fixed(streamline, lambda=lambda_opt)
  # Curvature
  curvature = get_curvature(streamline)$curvature
  curv_max = max(curvature)      
  curv_mean = mean(curvature)
  curv_sd = sd(curvature)
  # curv_maxNOSTRA = curv_torsNOSTRA[1]     
  # curv_meanNOSTRA = curv_torsNOSTRA[2]  
  # curv_sdNOSTRA = curv_torsNOSTRA[3]  
  curv_maxDIFF= curv_max- curv_torsNOSTRA[1]
  curv_meanDIFF = curv_mean - curv_torsNOSTRA[2]
  curv_sdDIFF = curv_sd - curv_torsNOSTRA[3]
  
  # Torsion
  torsion = get_torsion(streamline)$torsion
  tors_max = max(torsion)      
  tors_mean = mean(torsion)
  tors_sd = sd(torsion)
  # tors_maxNOSTRA = curv_torsNOSTRA[4]     
  # tors_meanNOSTRA = curv_torsNOSTRA[5]  
  # tors_sdNOSTRA = curv_torsNOSTRA[6] 
  tors_maxDIFF = tors_max - curv_torsNOSTRA[4]
  tors_meanDIFF = tors_mean - curv_torsNOSTRA[5]
  tors_sdDIFF = tors_sd - curv_torsNOSTRA[6]
  
  clength = get_curvilinear_length(streamline, validate = F)
  elength = get_euclidean_length(streamline, validate = F)
  sinuosity = clength/elength
  
  barycenter = get_barycenter_streamline(streamline)
  x_barycenter = barycenter[1]
  y_barycenter = barycenter[2]
  z_barycenter = barycenter[3]
  spatial_median = get_sp_median_streamline(streamline)
  x_SpMed = spatial_median[1]
  y_SpMed = spatial_median[2]
  z_SpMed = spatial_median[3]
  dist_cent = norm_vec2(t(barycenter-spatial_median))
  
  MD_sectors = MD_sectors(streamline)
  RD_sectors = RD_sectors(streamline)
  AD_sectors = AD_sectors(streamline)
  FA_sectors = FA_sectors(streamline)
  
  FA_sector1 = FA_sectors[1]
  FA_sector2 = FA_sectors[2]
  FA_sector3 = FA_sectors[3]
  FA_sector4 = FA_sectors[4]
  FA_sector5 = FA_sectors[5]
  MD_sector1 = MD_sectors[1]
  MD_sector2 = MD_sectors[2]
  MD_sector3 = MD_sectors[3]
  MD_sector4 = MD_sectors[4]
  RD_sector1 = RD_sectors[1]
  RD_sector2 = RD_sectors[2]
  RD_sector3 = RD_sectors[3]
  RD_sector4 = RD_sectors[4]
  AD_sector1 = AD_sectors[1]
  AD_sector2 = AD_sectors[2]
  AD_sector3 = AD_sectors[3]
  AD_sector4 = AD_sectors[4]
  
  return(data.frame(curv_max, curv_mean, curv_sd, 
                    # curv_maxNOSTRA, curv_meanNOSTRA, curv_sdNOSTRA,
                    tors_max, tors_mean, tors_sd, 
                    # tors_maxNOSTRA, tors_meanNOSTRA, tors_sdNOSTRA,
                    curv_maxDIFF, curv_meanDIFF, curv_sdDIFF,
                    tors_maxDIFF, tors_meanDIFF, tors_sdDIFF,
                    clength, elength, sinuosity,
                    x_barycenter, y_barycenter, z_barycenter,
                    x_SpMed, y_SpMed, z_SpMed, 
                    dist_cent,
                    FA_sector1, FA_sector2, FA_sector3, FA_sector4, FA_sector5,
                    MD_sector1, MD_sector2, MD_sector3, MD_sector4, 
                    RD_sector1, RD_sector2, RD_sector3, RD_sector4,
                    AD_sector1, AD_sector2, AD_sector3, AD_sector4
                    )
         )
}

norm_vec2 <- function(x){sqrt(crossprod(x))}

create_dataset_new = function(tract, side_label, standardized = T, lambda_opt)
{
  features = map_df(tract$Streamlines, compute_streamline_features)
  
  n = length(tract$Streamlines)
  patient = rep (as.numeric(tract$PatientId), n)
  side = rep(side_label,n)

  if (standardized == T){
    features = scale(features)
    features = as.data.frame(features)
  }
  
  features = mutate(features, side, patient)
  
  return(features)
}



MD_sectors = function(streamline){
  md_vector = streamline$md
  sector1 = (mean(md_vector[1:25]))
  sector2 = (mean(md_vector[26:35]))
  sector3 = (mean(md_vector[36:39]))
  sector4 = (mean(md_vector[40:50]))
  return (cbind(sector1, sector2, sector3, sector4))
}

RD_sectors = function(streamline, sector){
  rd_vector = streamline$rd
  sector1 = (mean(rd_vector[1:16]))
  sector2 = (mean(rd_vector[17:32]))
  sector3 = (mean(rd_vector[33:36]))
  sector4 = (mean(rd_vector[37:50]))
  return (cbind(sector1, sector2, sector3, sector4))
}

AD_sectors = function(streamline){
  ad_vector = streamline$rd
  sector1 = (mean(ad_vector[1:17]))
  sector2 = (mean(ad_vector[18:28]))
  sector3 = (mean(ad_vector[29:34]))
  sector4 = (mean(ad_vector[35:50]))
  return (cbind(sector1, sector2, sector3, sector4))
}

FA_sectors = function(streamline){
  fa_vector = streamline$fa
  sector1 = (mean(fa_vector[1:10]))
  sector2 = (mean(fa_vector[11:18]))
  sector3 = (mean(fa_vector[19:28]))
  sector4 = (mean(fa_vector[29:34]))
  sector5 = (mean(fa_vector[35:50]))
  return (cbind(sector1, sector2, sector3, sector4, sector5))
}
