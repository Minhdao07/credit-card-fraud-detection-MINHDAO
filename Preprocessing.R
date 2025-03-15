df <- read.csv("/Users/hoangminh/Downloads/creditcard.csv")
#Install and call out required libraries 
library(tidyverse)
library(ggplot2)
library(caret)
library(randomForest)
library(pROC)
#Simply get first few ideas of the data
View(df)
str(df)
summary(df)


#Check for mising data 
sum(is.na(df))
#Seems like there are no missing data. The data is ready to be discovered

#Since V1 -> V28 has already been standardized, column Amount and Time should do the same
df$Amount <- scale(df$Amount)
df$Time <- scale(df$Time)


#The data seems neat and ready to go 
