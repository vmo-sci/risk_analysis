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
# Var[Y], Cov[X,Y]
# Cov[X,Y], Var[Y]
var_x = 0.01
var_y = 0.04
# COV[X,Y] = corr_coeff * sqrt(var_x,var_y)
cov_xy = 0.8*sqrt(var_y*var_x)
c2 <- matrix(c(var_x,cov_xy,cov_xy,var_y), nrow = 2, byrow = F) # cov matrix

# Create a bivariate normal Distribution
nr_samples <- 10000
biv_normal_dist <- mvrnorm(nr_samples,mu=rho, Sigma = c2)

# If you want you can also plot the distribution
# plot(biv_normal_dist)

# a) calculate E[C] ----
# C = 2*(X+Y)

X <- biv_normal_dist[,1]
Y <- biv_normal_dist[,2]

expected_C <- mean(2*(X+Y))
print("E[C] =")
expected_C

# b) calculate E[A] ----
# A = X*Y


expected_A <- mean(X*Y)
print("E[A] =")
expected_A


# c) calculate P(C<29) + P(C>31)

C <- 2*(X+Y)
# no. Plates with C < 29

too_small <- length(C[C<29])
too_big <- length(C[C>31])

probability_useless_plate <- (too_small + too_big)/nr_samples
probability_useless_plate

