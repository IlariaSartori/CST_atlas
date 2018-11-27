###########  Lettura tratto
cst = read_csv("/06007", case = "007", scan = "001") # Controllare i suoi rdata

###########  Pulizia rispetto a RD e AD
cst$Streamlines = map(cst$Streamlines, remove_point_outliers) 

###########  Outlier detection
cst_divided = divide_cst(cst)

outliers_left = get_outliers_distance(cst_divided$lhs)
cst_divided$lhs$Streamlines = cst_divided$lhs$Streamlines[-outliers_left]

outliers_right = get_outliers_distance(cst_divided$rhs)
cst_divided$rhs$Streamlines = cst_divided$rhs$Streamlines[-outliers_right]

cst07 = cst_divided

###########  Reparametrization
cst07$lhs = reparametrize_with_interpolation(cst07$lhs)
cst07$rhs = reparametrize_with_interpolation(cst07$rhs)

save(cst07, file = "cst07_rep_no_outliers.RData")


