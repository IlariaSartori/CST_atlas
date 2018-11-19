#setwd("C:/Users/User/Google Drive/Progetto_StatApp/File per paper")

source("../Create_features_dataset/utility_functions.R")

read_csv = function(folder, name, case, scan, side = NA_character_){
  setwd(paste(getwd(),folder, sep=""))
  setwd(paste(getwd(),"/DIFF_30DIR_CST", sep=""))
  
  cst <- read_tract("DIFF_30DIR_CST_0.csv", name = name, case = case, scan = scan, side = side)
  return(cst)
}


read_tract <- function(path, name, case, scan, side = NA_character_) {
  data <- readr::read_csv(path)
  
  if (ncol(data) < 5L)
    stop("The input CSV file should contain at least 5 columns.")
  
  required_vars <- c("X", "Y", "Z", "PointId", "StreamlineId")
  if (!all(required_vars %in% names(data)))
    stop("The CSV file should contain at least the variables X, Y, Z, PointId and StreamelineId.")
  
  if (nrow(data) == 0)
    return(tract(
      name = name,
      case = case,
      scan = scan,
      side = side,
      data = list()
    ))
  
  
  data <- data %>%
    dplyr::arrange(StreamlineId, PointId) %>%
    dplyr::mutate(
      Tensors = purrr::pmap(list(`Tensors#0`, `Tensors#1`, `Tensors#2`,
                                 `Tensors#3`,`Tensors#4`, `Tensors#5`), c) %>%
        purrr::map(as_tensor)
      )
  
  data = data %>%
    dplyr::group_by(StreamlineId) %>%
    dplyr::do(streamlines = streamline(
      x = .$X, y = .$Y, z = .$Z, diffusion = .$Tensors
    )) %>%
    dplyr::ungroup()
  

  tract = tract(name = name, case = case, scan = scan, side = side, data = data$streamlines)
  
  AD_list = map(tract$data, get_axial_diffusivity_streamline)
  RD_list = map(tract$data, get_radial_diffusivity_streamline)
  for(i in 1:length(tract$data)){
    tract$data[[i]] = tract$data[[i]] %>%
                        mutate(
                          ad = AD_list[[i]],
                          rd = RD_list[[i]]
                        )
  }
  
  lower_rd <- 0.00001
  upper_rd <- 0.001
  lower_ad <- 0.001
  upper_ad <- 0.0024
  
  data_filtered = map(tract$data, ~dplyr::filter(., lower_rd < rd, rd < upper_rd, lower_ad < ad, ad < upper_ad) %>% as_streamline())
  
  for(i in 1:length(tract$data)){
    tract$data[[i]] = select(data_filtered[[i]], -ad, -rd)
  }

  
  return (tract)
            
} 


divide_cst = function(cst){
  # cst_left <- cst_right <- cst
  # cst_left$data <- cst$data[map_lgl(cst$data, ~ (.$x[1] < 0))]
  # cst_right$data <- cst$data[map_lgl(cst$data, ~ (.$x[1] > 0))]
  
  data_left <-cst$data[map_lgl(cst$data, ~ all(.$x < 0))]  
  data_right <- cst$data[map_lgl(cst$data, ~ all(.$x > 0))]
  cst_left = tract(name = cst$name[1], case = cst$case[1], scan = cst$scan[1], side = "left", data = data_left) 
  cst_right = tract(name = cst$name[1], case = cst$case[1], scan = cst$scan[1], side = "right", data = data_right) 

  return (list(lhs = cst_left, rhs = cst_right))
}

get_list_streamline_tensor = function(streamline){
  streamline_rep = approx_tensors(streamline$s, streamline$diffusion)
  # x_rep = approx(streamline$s, streamline$x, xout = streamline_rep$x)$y
  # y_rep = approx(streamline$s, streamline$y, xout = streamline_rep$x)$y
  # z_rep = approx(streamline$s, streamline$z, xout = streamline_rep$x)$y
  # df = cbind(s = streamline_rep$x, x = x_rep, y = y_rep, z = z_rep, tensor = streamline_rep$y)
  # return (as.data.frame(df))
  return(streamline_rep$y)
}

get_list_cst_tensor = function(cst){
  cst_two_side = divide_cst(cst)
  left_rep = map(cst_two_side$lhs$data, get_list_streamline_tensor)
  right_rep = map(cst_two_side$rhs$data, get_list_streamline_tensor)
  return (list(lhs = left_rep, rhs = right_rep))
}



# # ESEMPIO DI UTILIZZO
# setwd("C:/Users/User/Politecnico di Milano/Aymeric Stamm - fdatractography")
# cst01_prova = read_csv("/06001", name = "patient1", case = "1", scan = "01")



