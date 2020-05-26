

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