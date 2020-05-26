
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


