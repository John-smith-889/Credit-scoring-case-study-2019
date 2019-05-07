
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



