# Credit scoring case study

## General note

Repository contains code for performing prediction of defaults using MLP in MXNetR.

Code for feature engineering and modeling is provided.

Data is not available for this project due to lack of permission.
Code for model evaluation is also restricted.

## Environment  settings and package versions

> sessionInfo()

R version 3.5.0 (2018-04-23)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows >= 8 x64 (build 9200)

other attached packages:
[1] mxnet_1.4.0       
[2] scorecard_0.2.4   
[3] fastDummies_1.3.0 
[4] dplyr_0.8.0.1     
[5] corrplot_0.84  


# Feature engineering

Separated .R file is provided for feature engineering.

Techniques which were used:
- IV (Information Value) calculating
- dummy variables
- merging correlated variables with PCA


# Credit risk modeling

Separated .R file is provided for data modeling.

File contains:
- setting up Multi Layer Perceptron
- creating custom callback function for training process monitoring
- model evaluation with gini coefficient



