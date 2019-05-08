
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
# Correlation Matrix # 1
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
# PCA for DPD_lag1:DPD_lag12 # 1
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
# PCA for NotionalValue_lag1:NotionalValue_lag12 # 2
##################################################

pca_nv <- prcomp(select(dane, NotionalValue_lag1:NotionalValue_lag12), scale = TRUE, center = TRUE)
summary(pca_nv)


######################################################
# PCA for NotionalOverdue_lag1:NotionalOverdue_lag12 # 3
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


############################
# Variables standarization #
############################

#head(dane2)

dane2$Average_income <- as.numeric(dane2$Average_income)
dane2$Divorce_number_1000 <- as.numeric(dane2$Divorce_number_1000)

# Standarization of choosen variables ( (e(x) - xn) /std )
dane3 <- dane2 %>% mutate_each_(funs(scale(.) %>% as.vector), 
                                     vars=c("GEO_region",
                                            "Age",
                                            "Household_children",
                                            "Monthly_Income",
                                            "Monthly_Spendings",
                                            
                                            "Credit_amount",
                                            "Number_of_installments",
                                            "Divorce_number_1000",
                                            "Personal_car_number",
                                            "Truck_number",
                                            "Tractor_number",
                                            "Agricultural_tractor_number",
                                            "Building_permit_number",
                                            "Building_permit_individual_number",
                                            "Building_project_subbmission_number",
                                            "Apartment_project_subbmission_number",
                                            "Apartment_project_subbmission_area",
                                            "Employed_number_total",
                                            "Employed_number_men",
                                            "Employed_number_women",
                                            "Employed_agricultural_number",
                                            "Employed_industry_number",
                                            "Emplyed_trade_transport_number",
                                            "Employed_finance_number",
                                            "Employed_other_number",
                                            "Average_income",
                                            "Total_population_age_0_14_years",
                                            "Total_population_age_15_29_years",
                                            "Total_population_age_30_44_years",
                                            "Total_population_age_45_59_years",
                                            "Total_population_age_60_years_or_older",
                                            "Spending_food",
                                            "Spending_clothing",
                                            "Spending_footwear",
                                            "Spending_household",
                                            "Spending_glassware",
                                            "Spending_personal_care",
                                            "Spending_catering",
                                            "Spending_electronics",
                                            "Spending_recreational_and_cultural",
                                            "Total_population",
                                            "Working_age_population",
                                            "Unemployed_total",
                                            "Unemployed_vocational_number",
                                            "Unemployed_highschool_and_lower_number",
                                            "DPD_t0",
                                            "NotionalOverdue_t0"
                                            ))

str(dane3)


################
# IV computing #
################
# < 0.02	useless for prediction
# 0.02 to 0.1	Weak predictor
# 0.1 to 0.3	Medium predictor
# > 0.3	Strong predictor

# install.packages('scorecard')
library(scorecard)

# Hold scientific notation in df
options(scipen=999)

# IV
IV<-as.data.frame(iv(dane3, y="DefFlag", x = NULL, positive = "bad|1", order = TRUE))



#############################
# choosing best X variables #
#############################

# choosing variables with IV > 0.1
TopIV <- subset(IV, info_value>0.1 | variable == "DefFlag" ) # taking only medium+ predictors

dane4 <- dane3[,TopIV$variable]
str(dane4)
dane4$DefFlag <- dane3$DefFlag

#str(dane4)


######################
# Correlation Matrix # 2
######################

# install.packages("corrplot")
library(corrplot)

# str(dane3)

# Create data matrix from df
mat <- data.matrix(dane4)

# corelation matrix computation
mydata.cor = cor(mat)
corrplot(mydata.cor, tl.cex=0.1)



#####################
# Additional  1 PCA # 4
#####################

mat <- data.matrix(dane4[13:16]) # 1 pca
mydata.cor = cor(mat)
corrplot(mydata.cor, tl.cex=0.1) 

pca_geo_1 <- prcomp(select(dane4, 13:16), scale = TRUE, center = TRUE)
summary(pca_nv)

# str(dane4[,13:16])

dane4 <- dane4 %>% 
  cbind(pca_geo_1$x[,c(1:2)]) %>% # add first 2 components
  rename( pca_geo_1_1 = PC1, pca_geo_1_2 = PC2)
  
# str(dane4)
dane4 <- dane4[, ! colnames(dane4) %in% c("Personal_car_number",
                                       "Apartment_project_subbmission_area",
                                       "Employed_number_women",
                                       "Employed_industry_number")]
                                       

##################
# Additional PCA # 5
##################

mat <- data.matrix(dane4[20:36]) # 2 pca
mydata.cor = cor(mat)
corrplot(mydata.cor, tl.cex=0.1) 


# PCA
pca_geo_2 <- prcomp(select(dane4, 20:36), scale = TRUE, center = TRUE)
summary(pca_nv)

# look at variables which we merge
str(dane4[,20:36])

# Adding new PCs variables to data set
dane4 <- dane4 %>% 
  cbind(pca_geo_2$x[,c(1:2)]) %>% # add first 2 components
  rename( pca_geo_2_1 = PC1, pca_geo_2_2 = PC2)

# Removing old variables
dane4 <- dane4[, ! colnames(dane4) %in% c("Employed_number_total",
                                       "Emplyed_trade_transport_number",
                                       "Total_population_age_30_44_years",
                                       "Total_population_age_45_59_years",
                                       "Total_population_age_60_years_or_older",
                                       "Spending_food",
                                       "Spending_clothing",
                                       "Spending_footwear",
                                       "Spending_household",
                                       "Spending_glassware",
                                       "Spending_personal_care",
                                       "Spending_catering",
                                       "Spending_electronics",
                                       "Spending_recreational_and_cultural",
                                       "Total_population",
                                       "Working_age_population",
                                       "Unemployed_total"
                                                         )]


################
# IV computing # 
################

#install.packages('scorecard')
#library(scorecard)

# Hold scientific notation in df
options(scipen=999)

# IV
IV2<-as.data.frame(iv(dane4, y="DefFlag", x = NULL, positive = "bad|1", order = TRUE))


######################
# Correlation Matrix # 3
######################

# Lets check again correlation of variables

#install.packages("corrplot")
library(corrplot)

# Create data matrix from df
mat <- data.matrix(dane4)

# corelation matrix computation
mydata.cor = cor(mat)
corrplot(mydata.cor, tl.cex=0.1)

# look at next bunch of highly correlated variables
str(dane4[,19:22])


# Look more closely at variables correlation
# Create data matrix from df
mat <- data.matrix(dane4[19:22]) # 3 pca
# correlation matrix computation
mydata.cor = cor(mat)
corrplot(mydata.cor, tl.cex=0.1) 


##################
# Additional PCA # 6
##################

# PCA
pca_geo_3 <- prcomp(select(dane4, 19:22), scale = TRUE, center = TRUE)
summary(pca_nv)


# Adding new PCs variables to data set
dane4 <- dane4 %>% 
  cbind(pca_geo_2$x[,c(1:2)]) %>% # add first 2 components
  rename( pca_geo_3_1 = PC1, pca_geo_3_2 = PC2)


# Removing old variables
dane4 <- dane4[, ! colnames(dane4) %in% c("Employed_other_number",
                                          "Total_population_age_15_29_years",
                                          "Employed_finance_number",
                                          "Apartment_project_subbmission_number")]
                               
######################
# Correlation Matrix # 4
######################

# install.packages("corrplot")
#library(corrplot)

# Create data matrix from df
mat <- data.matrix(dane4)

# correlation matrix computation
mydata.cor = cor(mat)
corrplot(mydata.cor, tl.cex=0.1)

str(dane4[,17:21])
# lets stop pca for a while and do NNets


#################################################################
# Filther off observations without known DefFlag value attached #
#################################################################


library(dplyr)
# filtering out the data without DefFlags 
dane5 <- dane4 %>% filter(!is.na(DefFlag))
str(dane5) # 70k obs
# DefFlag - Default Flag - flag in context of not paid cash by customers risk 



############################################
# dividing on training and validation part #
############################################

# choosing rows for division
set.seed(0)
rand <- sample(1:nrow(dane5), 0.8*nrow(dane5))

# Training set
training <- dane5[rand,] # wybieramy obserwacja,wiec po comma nic nie dajemy
 str(training)

# Valid set
valid <- dane5[-rand,]
# str(train)


############################
# Data preparation for MLP #
############################

training2 <- data.matrix(training)
valid2 <- data.matrix(valid)
all_data <- data.matrix(dane4)
# automatic change of label data to nummeric data 

training2.x = training2[,! colnames(training2) %in% c("DefFlag")]
#head(training2.x)
training2.y = training2[,"DefFlag"]

valid2.x = valid2[,! colnames(valid2) %in% c("DefFlag")]

valid2.y = valid2[,"DefFlag"]

# data with all observation for further evaluation
all_x_data <- all_data[,! colnames(all_data) %in% c("DefFlag")]


#################
# Output for lm #
#################

# training

# valid

# dane4 (explanatory variables with all observations)

##########################
# Output for nnet mxnetR #
##########################

# training2.x
# training2.y

# valid2.x
# valid2.y

# all_x_data









