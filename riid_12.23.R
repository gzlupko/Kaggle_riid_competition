#  riid EDA and prediction generations 


library(dplyr) 
library(readr) 
library(ggplot2) 


riid_train <- read.csv("train.csv", nrows = 10000) 
train <- na.omit(riid_train) 



