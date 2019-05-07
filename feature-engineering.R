
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



