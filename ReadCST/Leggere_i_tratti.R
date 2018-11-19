source("read_tract_modificata.R")
source("tensor_interpolation.R")

setwd("C:/Users/Vale/Politecnico di Milano/Aymeric Stamm - fdatractography")
setwd("/Users/ILARIASARTORI/Politecnico di Milano/Aymeric Stamm - fdatractography")
setwd("C:/Users/user/Politecnico di Milano/Aymeric Stamm - fdatractography")

cst = read_csv("/06001", name = "patient1", case = "01", scan = "01") 

######################################################################################
################################ Lista tensori #######################################
######################################################################################

tensor_list_2 = get_list_cst_tensor(cst)

setwd("/Users/ILARIASARTORI/Desktop/Script Progetto StatApp - locale/File per paper")
save (tensor_list_2, file = "tensor_list_2.Rdata")




######################################################################################
############################## Cst reparametrized ####################################
######################################################################################

# cst01_left <- cst01_right <- cst01   ### VECCHIO COSTRUTTORE TRATTI
# cst01_left$data <- cst01$data[map_lgl(cst01$data, ~ (.$x[1] < 0))]
# cst01_right$data <- cst01$data[map_lgl(cst01$data, ~ (.$x[1] > 0))]


data_left <-cst$data[map_lgl(cst$data, ~ all(.$x < 0))]    ### NUOVO COSTRUTTORE TRATTI
data_right <- cst$data[map_lgl(cst$data, ~ all(.$x > 0))]
cst_left = tract(name = cst$name[1], case = cst$case[1], scan = cst$scan[1], side = "left", data = data_left) 
cst_right = tract(name = cst$name[1], case = cst$case[1], scan = cst$scan[1], side = "right", data = data_right) 
cst02_left = cst_left
cst02_right = cst_right

# setwd("/Users/ILARIASARTORI/Google drive/Progetto_StatApp/File per paper")
setwd("C:/Users/user/Google drive/Progetto_StatApp/File per paper")
save(cst1_left=cst02_left, cst1_right=cst02_right, file="cst1.RData")

# Far girare il file tensor_interpolation
n_points=50
cst02_left = reparametrize_with_interpolation(cst02_left, n_points)
cst02_right = reparametrize_with_interpolation(cst02_right, n_points)

setwd("C:/Users/user/Google drive/Progetto_StatApp/File per paper")
setwd("C:/Users/Vale/Dropbox/Poli/CST Project/File per paper/RData streamline riparametrizzate")
save(cst02_left, cst02_right, file="cst2.RData")
rm(cst02, cst02_right, cst02_left)
rm(cst, cst_left, cst_right)

