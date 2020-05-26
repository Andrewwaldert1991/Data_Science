
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


