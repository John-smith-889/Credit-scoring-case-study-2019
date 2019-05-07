
################################################################################
# Feature engineering #       
#!#####################

#########
# Setwd #
#########

getwd()

setwd("path/to/new_project_root")

rm(list=ls())
dev.off()


################
# Loading data #
################

dane <- read.csv2("final_data.csv", header = T, sep = ",", dec = ".")
# str(dane) # 100k obs


######################
# Correlation Matrix #
######################

# install.packages("corrplot")
library(corrplot)

# str(dane)

# Create data matrix from df
mat <- data.matrix(dane)

# corelation matrix computation
mydata.cor = cor(mat)
corrplot(mydata.cor, tl.cex=0.1)


##############################
# PCA for DPD_lag1:DPD_lag12 #
##############################

pca_dpd <- prcomp(select(dane, DPD_t0:DPD_lag12), scale = TRUE, center = TRUE)
#str(pca_dpd)
# result of "precomp":
pca_dpd$sdev   # - sdev of each principal compound
# pca_dpd$rotation
# pca_dpd$center
# pca_dpd$scale
# pca_dpd$x <- in this variable are 12 PCA compounds of all 100k dataset

# -> we neet few best componuds with best st dev (first are the best)
summary(pca_dpd)


##################################################
# PCA for NotionalValue_lag1:NotionalValue_lag12 #
##################################################

pca_nv <- prcomp(select(dane, NotionalValue_lag1:NotionalValue_lag12), scale = TRUE, center = TRUE)
summary(pca_nv)


######################################################
# PCA for NotionalOverdue_lag1:NotionalOverdue_lag12 #
######################################################
library(dplyr)

pca_no <- prcomp(select(dane, NotionalOverdue_lag1:NotionalOverdue_lag12), scale = TRUE, center = TRUE)
summary(pca_no)

# show structure of data
str(dane, list.len=ncol(dane))

# Adding new variables to dataset
dane <- dane %>% 
  cbind(pca_dpd$x[,c(1:3)]) %>% # add first 3 components from pca_dpd
  rename( dpd_pc1 = PC1, dpd_pc2 = PC2, dpd_pc3 = PC3) %>% 
  cbind(pca_nv$x[,c(1,2)]) %>% # add first 2 components from nval_pc2
  rename(nv_pc1 = PC1, nv_pc2 = PC2)  %>% 
  cbind(pca_no$x[,c(1:4)]) %>% # add first 4 components from pca_novd
  rename(no_pc1 = PC1, no_pc2 = PC2, no_pc3 = PC3, no_pc4 = PC4)

# deleting old PCA-ed variables
dane <- dane[, ! colnames(dane) %in% c("NotionalValue_t0",
                                      "NotionalValue_lag1",
                                      "NotionalValue_lag2",
                                      "NotionalValue_lag3",
                                      "NotionalValue_lag4",
                                      "NotionalValue_lag5",
                                      "NotionalValue_lag6",
                                      "NotionalValue_lag7",
                                      "NotionalValue_lag8",
                                      "NotionalValue_lag9",
                                      "NotionalValue_lag10",
                                      "NotionalValue_lag11",
                                      "NotionalValue_lag12",
                                      "DPD_lag1",
                                      "DPD_lag2",
                                      "DPD_lag3",
                                      "DPD_lag4",
                                      "DPD_lag5",
                                      "DPD_lag6",
                                      "DPD_lag7",
                                      "DPD_lag8",
                                      "DPD_lag9",
                                      "DPD_lag10",
                                      "DPD_lag11",
                                      "DPD_lag12",
                                      "NotionalOverdue_lag1",
                                      "NotionalOverdue_lag2",
                                      "NotionalOverdue_lag3",
                                      "NotionalOverdue_lag4",
                                      "NotionalOverdue_lag5",
                                      "NotionalOverdue_lag6",
                                      "NotionalOverdue_lag7",
                                      "NotionalOverdue_lag8",
                                      "NotionalOverdue_lag9",
                                      "NotionalOverdue_lag10",
                                      "NotionalOverdue_lag11",
                                      "NotionalOverdue_lag12")]
                                                                           
str(dane)


###################
# Dummy variables #
###################

#install.packages('fastDummies')
library(fastDummies)

# Creating dummy variables
dane2 <- fastDummies::dummy_cols(dane, select_columns = c(
                                                          "Job_type",
                                                          "Marital_status",
                                                          "Home_status",
                                                          "Car_status",
                                                          "Credit_purpose"))

# deleting old pre-dummy variables
dane2 <- dane2[,! colnames(dane2) %in% c("Job_type",
                                         "Marital_status",
                                         "Home_status",
                                         "Car_status",
                                         "Credit_purpose")]

str(dane2)

###########################
# - deletion in var names # (from var names, couldnt add vars to model)
###########################
# (those - was created with dummy variables)

# Change all "-" to "_" in variable names
names(dane2) <- sub("-","_", names(dane2)) 
# str(dane2)

