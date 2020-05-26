
### Johns Hopkins Data Sciecen Specialization R Programming Week 1 R Console

### Simple Function  which generates 100 random numbers and calculates the mean
myfunction <- function() {
X <- rnorm(100)
mean(X)
}

### Second Function which generates noise around the entered number
mysecondfunction <- function(x) {
  x+rnorm(length(x))
}

### Defining and Printing out a Vector in R 
y <- 1:20

### Creating Vectors using c (Concat)

y <- c(2, 3, 5, "ABC", 100) # in this case the vector is reduced to the least common demominator

c <- c(2, 3, 5, 100) # here the vector is truly numeric no coercion taking place 

# Foced coercion can be done with as.numeric(x); as.logical(x) and as.character(x)

### Matrices 

m <- matrix(1:10, nrow = 2, ncol = 5, T) # by row 
m <- matrix(1:10, nrow = 2, ncol = 5, F) # by collumn
dim(m) # gives the dimensions of m
attributes(m) # gives the attributes of the matrix

### Plotting a normal distribution with regions of interest marked on the distribution
x <- seq(from = -3, to = +3, length.out = 100)#
y <- dnorm(x)
plot(x, y, main = "Normal Distribution", type = "l", ylab = "Density", xlab = "Quantile")
abline(h=0)
region.x <- x[1<= x & x <= 2]
region.y <- y[1 <= x & x <= 2]


m <- matrix(rnorm(100), nrow = 20, ncol = 5, T) # by row  of randomly generated numbers of normal distributiob

# Assigning a Matrix to a numeric array

m <- 1:100 # create an array of numbers from 1-100
dim(m) <- c(20, 5) # casted the numbers into a matrix
m # printed the matrix

# Collumn and Row Binding 
x <- 1:11
y <- 5:15
cbind(x,y) # binding the two arrays of numbers by the collumns 
rbind(x, y) # binding the two arrays of numbers by the rows 






### Read in Attrition HR Dataset: 

data <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv")


### Split Dataset into attrition people and non attrition people 

attrition_employees <- data[ which(data$Attrition == "Yes"), ] #Attrition Employees 
Loyal_employees <- data[ which(data$Attrition == "No"), ] #Loyal Employees
Salary_loyal_employees <- Loyal_employees["MonthlyIncome"] # Creating a variable, wich captures the monthly income of the loyal people
Salary_attrition_employees <- attrition_employees["MonthlyIncome"] # Creating a variable, wich captures the monthly income of the attrition people
companies_loyal_employees <- Loyal_employees["NumCompaniesWorked"]

### Plotting the Loyal Employees 

boxplot(Salary_loyal_employees, horizontal = T, main = "Salary of Loyal Employees")# Creating a boxplot 
hist(Salary_loyal_employees, breaks = 10)

### Creating DataFrame to Examine the relationship of loyal employees salary and numnber of companies worked for

comparison_data_frame <- cbind(Salary_loyal_employees,companies_loyal_employees) # binding the two arrays of numbers by the collumns 

### Loading tools: 
library(ggplot2)
library(tidr)
