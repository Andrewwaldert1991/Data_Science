
### 1. Reading in the data 
data_full <- read.csv("data.csv")
head(data_full)
dim(data_full)
options(width = 100)
class(data_full)
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

par(mfrow=c(3, 2))

plot(Bedrooms_V, Price_V, ylim = c(0, 1000000))
plot(Bathrooms_V, Price_V, ylim = c(0, 1000000))
plot(Sqft_Living_V, Price_V, ylim = c(0,1000000))
plot(Sqft_Lot_V, Price_V,ylim = c(0, 1000000))
plot(Waterfront_V, Price_V, ylim = c(0, 1000000))
plot(Condition_V, Price_V,ylim = c(0, 1000000))

### Setting up multiple regression for the variavles 

M_Regress <- lm(Price_V ~ Bedrooms_V + Bathrooms_V +Sqft_Living_V + Sqft_Lot_V + Waterfront_V + Condition_V)
summary(M_Regress)

Sig_M_Regress <- lm(Price_V ~ Sqft_Living_V + Sqft_Lot_V)
summary(Sig_M_Regress)

### Eliminating insignificant Factors

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


