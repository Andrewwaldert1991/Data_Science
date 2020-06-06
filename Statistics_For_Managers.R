# Statistics Lecture 1

-------------------------------------------------------------------------------------------------------------------

# Explanation of R and Gitbash 
 # 1. Installing R Studio (or even better the entire Anaconda Navigator)
  # 2. Downloading Gitbash \
    # 3. Creating A Github Acount by signing up for GitHub and cliclking the "New" Tile

# Some useful Gitbash Commands
# pwd #prints current working directory
# cd # changes the working directory
# ls # lists the items in the working direcory
# mkdir # creates a new folder in the working directory
# touch # creates a test file
# rm # stands for remove, be very carefull, as removed files cannot be retrieved
# mv # Move files between direcories 
# echo # Echos whatever argument is provided 
# date # Gives you the current date 

# Linking your GitHub account to your working directory on your local machine

# git config --global user.name "Your Name"
# git config --global user.email "Your E-mail"
# git config --list #See all changes you made
# mkdir ~/Data_Science
# git init
# git remote add origin "URL of Git Repo"

# Synching your local repo to the GitHub repo
# git add . - adds new files
# git add -U # Updates tracking of files 
# git add # does both 

# git commit -m "This is an addition I made to  my repo"
# git pull # pull the status of the online repo and check if it is in sync with the last update
# git push # push your repo to Gitup .... and DONE ;)

-------------------------------------------------------------------------------------------------------------------

# In R Studio Setting the working directory
  
getwd() # See which directory you are currently in

setwd("C:/Users/awaldert/Desktop/Data_Science") # Set your Working Directory to your desired folder

# The Laws on Probability

# Probability = Number of desired outcomes/Number of possible outomces
# Hence the probability getting a 2 from a die roll is 1/6
# Probability of event A and B which are not dependent (in odher words they are independent)
# Indenpendent means that the outcome of event A, has no impact on the probability of B occurring 

# Example Two coins being tossed
p.head <- 0.5
p.tail <- 0.5

# What is the probability of tossing a coin and getting a) two heads b) 4 tails and c) 10 heads
p.head^2
p.head^4
p.head^10

# Proving that I am correct in a) ... 
p.head <- rbinom(100000, 1, .5) #You can chose different probabilites as you please
p.tail <- rbinom(100000, 1, .5) #You can chose different probabilites as you please
mean(p.head & p.tail)

# Probability of event A or B which are not dependent 
# You toss a coin two times, what is the probability of getting either heads or tail
p.head <- 0.5
p.tail <- 0.5
p <- p.head + p.tail -(p.head*p.tail)
print(p)

# Problem: If you throw a six-sided die and then flip a coin, what is the probability that you will get either a 6 on the die or a head on the coin flip?
p.six <- 1/6
p.head <- 0.5
p <- p.head + p.six -(p.head*p.six)
print(p)

# Problem: You are in the process of applying for our first job and have sent out two applications. You rate the probability of getting Job A to be 20% and Job B to be 60%. 
# What is the probability of getting at least one of the two jobs? Note they are idependent of each other.
p.jobA <- .2
p.jobB <- .6
p.not.jobA <- .8
p.not.jobB <- .4
p <- 1-(p.not.jobB*p.not.jobA)
print(p)

-------------------------------------------------------------------------------------------------------------------

# The Normal Distribution and Z Scores
# Generally, it is observed that the collection of random data from independent sources is distributed normally. 
#We get a bell shape curve on plotting a graph with the value of the variable on the horizontal axis and the count of the values in the vertical axis. 
#The centre of the curve represents the mean of the dataset.

# First let us have a look at what a normal distribution looks like: 
x <- seq(0, 10, by = .1)
y <- dnorm(x, mean = 5, sd =1)
plot(x, y, main = "Normal Distribution", col = "blue", type = "l")

# Calculating Probabilities on the Normal Distribution using the slides and our calculator ;)!

# But first we need to load some tools for later ... 
library(dplyr)
library(rpart) 
library(ggplot2)
library(caTools)
library(pdftools)

# Enough with the boring stuff, let us get into some data!

# Reading in the House Price data
data_full <- read.csv("data.csv") # This csv file is in my working directory, to read it maje sure it is in yours and you have set your current working directory in R accordingly, and correct spelling helps
head(data_full)
dim(data_full)
class(data_full)
str(data_full)
?str()

# Descriptive Statistics 

# Definition A descriptive statistic (in the count noun sense) is a summary statistic that quantitatively describes or summarizes features from a collection of information,[1] 
# while descriptive statistics (in the mass noun sense) is the process of using and analysing those statistics.

# Descriptive Statistics and Measures of Location and Dispersion
# Measures of Location
 mean(data_full$price) # Average if the data 
 median(data_full$price) #M iddle Point of the data in ascending order
 mode(data_full$price) #M ost frequent number 
 weighted.mean(data_full$price) #The sum of all values times a weight/sum of their weights 


# Measures of Dispersion
var(data_full$price, y=NULL)
sqrt(var(data_full$price, y=NULL)) #St.Dev
range(data_full$price)
IQR(data_full$price) # The interquartile range of an observation variable is the difference of its upper and lower quartiles. It is a measure of how far apart the middle portion of data spreads in value. 
quantile(data_full$price, c(.25, .44, .99))


# Plotting
# After having looked at your preliminary data, let us have a look at it by creaing simple and not so simple plots


## Scatterplot 
## Pie Chart 
## Histogram
## Boxplot
## Heatmap
## Worldmap 


# The normal distribution 
# Calculating Probabilities on the normal distribution
# Calculating Confidence Intervals 
# Calculating Z Scores and the Z Score Table 
# Hypothesis Test (One and Two Tailed)
# Inferential Statistics (linear and Multiple Regression)


# Machine Learning (Case Example)


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

(Final_Data$Actual- Final_Data$Predicted) -> error
cbind(Final_Data, error) -> Final_Data
Initial_RMSE <- sqrt(mean(Final_Data$error^2))
RMSE
