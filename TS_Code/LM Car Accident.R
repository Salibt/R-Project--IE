library(car)
library(psych)
setwd("C:/Users/Salibt/Desktop/Master IE/Course/Statistical Programing With R/Project/Data Set")
acc_15 <- read.csv ("Accidents_2015.csv", header=TRUE,sep=",",dec=".")
dim(acc_15)
head(acc_15)
## this is the first test
acc_16 <- read.csv ("Accidents_2016.csv", header=TRUE,sep=",",dec=".")
dim(acc_16)
head(acc_16)
#test
acc <- rbind(acc_15,acc_16)
dim(acc)
head(acc)
