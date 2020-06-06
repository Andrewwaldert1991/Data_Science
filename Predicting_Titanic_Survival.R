# Setting the working directory
setwd("C:/Users/awaldert/Desktop/Data_Science/Titanic Dataset")

# Loading some tools 
library(dplyr)
library(rpart) 
library(ggplot2)
library(caTools)

gender_submission <- read.csv("gender_submission.csv")
test_set <- read.csv("test.csv")
train_set <- read.csv("train.csv")
str(gender_submission)
str(train_set)
str(test_set)

# Building a linear model on top of the training dataset (Cannot do as in the test set, the survived variable is missing...)

lm(Survived~., train_set) -> mod_regress 
cbind(Actual = test_set$, Predicted = result_regress) -> Final_Data
as.data.frame(Final_Data)-> Final_Data
head(Final_Data)

# Finding theb RMSE (Root Mean Squared Error)

(Final_Data$Actual- Final_Data$Predicted) -> error1
cbind(Final_Data, error) -> Final_Data
Initial_RMSE <- sqrt(mean(Final_Data$error^2))
Initial_RMSE