setwd("C:/Users/awaldert/Desktop/Data_Science")

# Installing needed pqackages 

library(dplyr)
library(rpart) 
library(ggplot2)
library(caTools)

# Learning how ML works using the diamionds Dataset 

View(diamonds)
str(diamonds)
dim(diamonds)
length(diamonds$price)

# Splitting the data into train and test sets

sample.split(diamonds$price, SplitRatio = 0.65) -> Split_Values

subset(diamonds, Split_Values==T) -> Train_Set
subset(diamonds, Split_Values==F) -> Test_Set

head(Train_Set)
dim(Train_Set)
dim(Test_Set)
 
# Building a linear model on top of the training dataset

lm(price~., Train_Set) -> mod_regress #Lets use stepwise backward regression to optimize our model and compare to the RMSE obtained before 
predict(mod_regress, Test_Set) -> result_regress
cbind(Actual = Test_Set$price, Predicted = result_regress) -> Final_Data
as.data.frame(Final_Data)-> Final_Data
head(Final_Data)

# Finding theb RMSE (Root Mean Squared Error)

(Final_Data$Actual- Final_Data$Predicted) -> error1
cbind(Final_Data, error) -> Final_Data
Initial_RMSE <- sqrt(mean(Final_Data$error^2))
Initial_RMSE

########################################################################Optional########################################################################################

# susing stepwise backward regression to optimize our model and compare to the RMSE obtained before 

reduced.mod.regress <- step(mod_regress, direction = "backward")
predict(reduced.mod.regress, Test_Set) -> red.result_regress
cbind(Actual = Test_Set$price, Predicted = red.result_regress) -> Final_Data.red.mod
as.data.frame(Final_Data.red.mod)-> Final_Data.red.mod
head(Final_Data.red.mod)

# Finding theb RMSE (Root Mean Squared Error) of the reduced  model

(Final_Data.red.mod$Actual- Final_Data.red.mod$Predicted) -> error2
cbind(Final_Data.red.mod, error2) -> Final_Data_Reduced_Model
Reduced_RMSE <- sqrt(mean(Final_Data_Reduced_Model$error2^2))
Reduced_RMSE
Initial_RMSE
summary(reduced.mod.regress)
summary(mod_regress)

# Comparing RSME based on differenc combinations and trying stepwise forward regression to optimize the model 
# Possible Option to eliminate the effect of outliers by curring bottom and top 10% of the dataset

