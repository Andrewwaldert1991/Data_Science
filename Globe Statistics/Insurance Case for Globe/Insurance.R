getwd() # See which directory you are currently in
setwd("C:/Users/awaldert/Desktop/Data_Science") # Set your Working Directory to your desired folder

# But first we need to load some tools for later ... 

library(dplyr)

library(rpart) 

library(ggplot2)

library(caTools)

library(pdftools)

library(plotrix)

# Reading in the Data

ins_data <- read.csv("Insurance.csv")

# Getting familiar with the data set
class(ins_data)
names(ins_data)
length(ins_data)
dim(ins_data)
head(ins_data)
tail(ins_data)
summary(ins_data)
str(ins_data)
NA_check <- is.na(ins_data)
summary(NA_check)

# Please calculate the following descriptive statistics for the variable BMI
mean(ins_data$bmi) 
median(ins_data$bmi) 

# Please calculate the following measures of dispersion for the variable age
var(ins_data$age, y=NULL)
sqrt(var(ins_data$age, y=NULL)) #St.Dev
range(ins_data$age)
IQR(ins_data$age) # The interquartile range of an observation variable is the difference of its upper and lower quartiles. It is a measure of how far apart the middle portion of data spreads in value. 
quantile(ins_data$age, c(.25, .44, .99))

# let us set the display area for more than one chart
par(mfrow = c(3, 2))

plot(ins_data$age, ins_data$charges)
plot(ins_data$sex, ins_data$charges)
plot(ins_data$bmi, ins_data$charges)
plot(ins_data$children, ins_data$charges)
plot(ins_data$smoker, ins_data$charges)
?plot#what else can we add?
cor(ins_data$age, ins_data$charges) # create some more

# Please create the following plots: 
# heatmap of both BMI and charges
bmi <- ins_data$bmi
charges <- ins_data$charges
class(bmi)

# barchart
barchart <- barplot(ins_data$bmi)
?barplot

#boxplot
boxplot(ins_data$bmi)

Distribution
# Histogram overlaid with kernel density curve
ggplot(ins_data, aes(x=bmi)) + 
  geom_histogram(aes(y=..density..),     
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") 


# Generating a linear model to estimate insurance charges
model <- lm(ins_data$charges ~ ins_data$age + ins_data$sex + ins_data$bmi + ins_data$children + ins_data$smoker)
summary(model)

# Using step-wise backward regression to optimize our model

improved.model <- step(model, direction = "backward")
summary(improved.model)

# Use machine learning to estimate insurance cost based on the improved regression model


# Splitting the data into train and test sets

sample.split(ins_data$charges, SplitRatio = 0.65) -> Split_Values
subset(ins_data, Split_Values==T) -> Train_Set
subset(ins_data, Split_Values==F) -> Test_Set
head(Train_Set)
dim(Train_Set)
dim(Test_Set)

# Building a linear model on top of the training dataset

lm(charges~., Train_Set) -> mod_regress 
predict(mod_regress, Test_Set) -> result_regress
cbind(Actual = Test_Set$price, Predicted = result_regress) -> Final_Data
as.data.frame(Final_Data)-> Final_Data
head(Final_Data)


# Finding theb RMSE (Root Mean Squared Error)


(Final_Data$Actual- Final_Data$Predicted) -> error

cbind(Final_Data, error) -> Final_Data

Initial_RMSE <- sqrt(mean(Final_Data$error^2))

RMSE
