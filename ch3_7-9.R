# Chapter3.7-9
# c)
# Refer to Problem 103a-c and find the answers by means of simulations 
# (skip the expected value and variance conditional on x).

# Problem 103 ----


# A company manufactures rectangular metal plates of size 5 * 10 (inches)
# (X,Y) is a bivariate normal distribution with E[X] = 5 and E[Y]=10
# Var[X] = 0.01, Var[Y] = 0.04, Correlation Coefficient[X,Y] = 0.8
# Circumference C = 2X+2Y
# Area A = X*Y

# a)----
# Find E[C] 

set.seed(1)
library(MASS)

rho_x = 5
rho_y = 10
rho <- c(rho_x,rho_y) # mean
# covariance matrix:
# TODO Not Cov but Correlation Coefficient
# Var[Y], Correlation Coefficient
# Correlation Coefficient[X,Y], Var[Y]
var_x = 0.01
var_y = 0.04
coeff_xy = 0.8
c2 <- matrix(c(var_x,coeff_xy,coeff_xy,var_y), nrow = 2, byrow = F) # cov matrix

x <- mvrnorm(1000,mu=rho, Sigma = c2)
plot(x)








