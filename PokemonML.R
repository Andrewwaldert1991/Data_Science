setwd("C:/Users/awaldert/Desktop/Data_Science")

# Installing needed pqackages 

library(dplyr)
library(rpart) 
library(ggplot2)
library(caTools)

# Reading in the file 
read.csv("pokemon.csv") -> pokemon_data

head(pokemon_data)
str(pokemon_data)
plot(pokemon_data)
View(pokemon_data)