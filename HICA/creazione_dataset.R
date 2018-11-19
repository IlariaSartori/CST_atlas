# setwd("C:/Users/Vale/Google Drive/Progetto_StatApp/File per paper/RData streamline riparametrizzate")

setwd("/Users/ILARIASARTORI/Google drive/Progetto_StatApp/File per paper/RData_tratti_riparametrizzati")

load("cst01_reparametrized.RData")
load("cst02_reparametrized.RData")
load("cst03_reparametrized.RData")
load("cst04_reparametrized.RData")
load("cst05_reparametrized.RData")
load("cst06_reparametrized.RData")
load("cst07_reparametrized.RData")
load("cst08_reparametrized.RData")
load("cst09_reparametrized.RData")
load("cst10_reparametrized.RData")
load("cst11_reparametrized.RData")
load("cst12_reparametrized.RData")
load("cst13_reparametrized.RData")
load("cst14_reparametrized.RData")
load("cst15_reparametrized.RData")
load("cst16_reparametrized.RData")
load("cst17_reparametrized.RData")
load("cst18_reparametrized.RData")
load("cst19_reparametrized.RData")
load("cst20_reparametrized.RData")

patients_left = list ("patient01_left" = cst01_left,
                      "patient02_left" = cst02_left,
                      "patient03_left" = cst03_left,
                      "patient04_left" = cst04_left,
                      "patient05_left" = cst05_left,
                      "patient06_left" = cst06_left,
                      "patient07_left" = cst07_left,
                      "patient08_left" = cst08_left,
                      "patient09_left" = cst09_left,
                      "patient10_left" = cst10_left,
                      "patient11_left" = cst11_left,
                      "patient12_left" = cst12_left,
                      "patient13_left" = cst13_left,
                      "patient14_left" = cst14_left,
                      "patient15_left" = cst15_left,
                      "patient16_left" = cst16_left,
                      "patient17_left" = cst17_left,
                      "patient18_left" = cst18_left,
                      "patient19_left" = cst19_left,
                      "patient20_left" = cst20_left)

patients_right = list ("patient01_right" = cst01_right,
                      "patient02_right" = cst02_right,
                      "patient03_right" = cst03_right,
                      "patient04_right" = cst04_right,
                      "patient05_right" = cst05_right,
                      "patient06_right" = cst06_right,
                      "patient07_right" = cst07_right,
                      "patient08_right" = cst08_right,
                      "patient09_right" = cst09_right,
                      "patient10_right" = cst10_right,
                      "patient11_right" = cst11_right,
                      "patient12_right" = cst12_right,
                      "patient13_right" = cst13_right,
                      "patient14_right" = cst14_right,
                      "patient15_right" = cst15_right,
                      "patient16_right" = cst16_right,
                      "patient17_right" = cst17_right,
                      "patient18_right" = cst18_right,
                      "patient19_right" = cst19_right,
                      "patient20_right" = cst20_right)


save(patients_left, patients_right, file = "lists_patients.RData")
