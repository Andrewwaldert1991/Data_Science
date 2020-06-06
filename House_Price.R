
# Setting the working directory
setwd("C:/Users/awaldert/Desktop/Data_Science")

# Loading some tools 
library(dplyr)
library(rpart) 
library(ggplot2)
library(caTools)


# Reading in the data 
data_full <- read.csv("data.csv")
head(data_full)
dim(data_full)
class(data_full)

# Most useful in sumarizing the data
str(data_full)

### Transformig the variables from data.frame to vectors for al 18 variables 

Price_V <- as.vector(unlist(data_full$price))
Bedrooms_V <- as.vector(unlist(data_full$bedrooms))
Bathrooms_V <- as.vector(unlist(data_full$bathrooms))
Sqft_Living_V <- as.vector(unlist(data_full$sqft_living))
Sqft_Lot_V <- as.vector(unlist(data_full$sqft_lot))
Waterfront_V <- as.vector(unlist(data_full$waterfront))
Condition_V <- as.vector(unlist(data_full$condition))
Street_V <- as.vector(unlist(data_full$street))
City_V <- as.vector(unlist(data_full$city))

# Assessing correlation to see wwhich variables are correlated 

par(mfrow=c(3, 2))

plot(Bedrooms_V, Price_V, ylim = c(0, 1000000))
plot(Bathrooms_V, Price_V, ylim = c(0, 1000000))
plot(Sqft_Living_V, Price_V, ylim = c(0,1000000))
plot(Sqft_Lot_V, Price_V,ylim = c(0, 1000000))
plot(Waterfront_V, Price_V, ylim = c(0, 1000000))
plot(Condition_V, Price_V,ylim = c(0, 1000000))

# Splitting the dataset into 65% Train and 35% Test by using the Price Column

sample.split(data_full$price, SplitRatio = 0.65) -> Split_Values

subset(data_full, Split_Values==T) -> Train_Set
subset(data_full, Split_Values==F) -> Test_Set

head(Train_Set)
dim(Train_Set)
dim(Test_Set)

# Building a linear model on top of the training dataset

mod_regress <- lm(price~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront + view 
                  + condition + sqft_above + sqft_basement 
                  + yr_built + yr_renovated, Train_Set)

# Alternative Regression Model to take out insignificant variables
mod_regress <- lm(price~ bedrooms + bathrooms + sqft_living + sqft_lot + view 
                  + yr_built, Train_Set)

summary(mod_regress)
predict(mod_regress, Test_Set) -> result_regress
cbind(Actual = Test_Set$price, Predicted = result_regress) -> Final_Data
as.data.frame(Final_Data)-> Final_Data
head(Final_Data)

# Finding theb RMSE (Root Mean Squared Error)

(Final_Data$Actual- Final_Data$Predicted) -> error
cbind(Final_Data, error) -> Final_Data
RMSE <- sqrt(mean(Final_Data$error^2))
par(mfrow=c(1, 1))
plot(Final_Data$Actual, Final_Data$Predicted)
plot(Final_Data$error)





############################################################################
# Eliminating insignificant Factors

full.model <- M_Regress

reduced.mode <- step(full.model, direction = "backward")

summary(reduced.mode)

par(mfrow=c(1, 1))
plot(Sqft_Living_V, Price_V, ylim = c(0,1000000), xlim = c(0, 8300))


### Boston Plot of Correlation between sqft living and prices

Data_Boston <- subset(data_full, city = "Boston")

x <- matrix(rnorm(10), 2, 5)
print(x)

apply(x, 1, sum)


