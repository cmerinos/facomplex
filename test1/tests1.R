library(facomplex)

BSI(data = ex1_fl)

devtools::document()

#######################################
Hofmann(data = fa.load)
HofmannFac(data = fa.load)

#######################################
 ex1_fl <- data.frame(
   F1 = c(0.536, 0.708, 0.600, 0.673, 0.767, 0.481, -0.177, 0.209, -0.097, -0.115, 0.047, 0.024),
   F2 = c(-0.110, 0.026, 0.076, 0.011, -0.160, 0.106, 0.668, 0.438, 0.809, 0.167, 0.128, 0.041),
   F3 = c(-0.100, 0.036, 0.086, 0.021, -0.150, 0.116, 0.678, 0.448, 0.819, 0.577, 0.738, 0.751) )

simload(data = ex1_fl, 
     items_target = list(F1 = c(1, 2, 3, 4, 5, 6),
                         F2 = c(7, 8, 9),
                         F3 = c(10, 11, 12)))

 #######################################
 KC(data = ex1_fl)
 
 #######################################
 facomplex::LSIglobal(ex1_fl)
 
 #######################################
targetLoad2 <- matrix(data = 0, nrow = 12, ncol = 3)  
targetLoad2[1:6, 1]  <- 1
targetLoad2[7:9, 2]  <- 1
targetLoad2[10:12, 3]  <- 1
 
targetLoad2
 
SSindices(loadings = ex1_fl, 
           target = targetLoad2, per.factor = T)
 
 
#####################################
bmcload.table3 <- read.table(file =  "clipboard", sep = "\t", header=T)

##### 0.2 Transformation to numeric dataframe #####

bmcload.table3 <- as.data.frame(sapply(bmcload.table3, as.numeric))

bmcload.table3[3,2] <- -.051
bmcload.table3[4,3] <- -.043

BSI(data = bmcload.table3)
HofmannFac(bmcload.table3)


targetLoad <- matrix(data = 0, nrow = 16, ncol = 3)  
targetLoad[1:6, 1]  <- 1
targetLoad[7:11, 2]  <- 1
targetLoad[12:16, 3]  <- 1

targetLoad


SSindices(loadings = bmcload.table3, target = targetLoad, per.factor = F)
SSindices(loadings = bmcload.table3, target = targetLoad, per.factor = T)


entropy_index(loadings_matrix = as.matrix(load.table3[, 2:4]), base = )
