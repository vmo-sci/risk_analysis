# Ch6.1-2
# Conduct a simulation study to check which
# is more efficient estimator of the E(X)
# the sample median? Both estimators
# are unbiased and consistent, nonetheless
# everyone is using the mean! Sensible?


# Simulation set up:

# 1. Fix, n, E(X), sd(X), R

n <- 10^3
E_X <- 10
sd_X <- 10
R <- 10^3

# 2. Generate a rth sample of n
# N(E(X),sd(X))
# 3. calc mean and median
# 4. Repeat R times 
mean_X <- c()
median_X <- c()
for (i in 1: R){
  X <- rnorm(n, E_X, sd_X)
  mean_X <- c(mean_X,mean(X))
  median_X <- c(median_X,median(X))
}

# 5. Calculate standard deviation
sd(mean_X)
sd(median_X)

# 6. Modify settings

# Ch6.1-2
# Conduct a simulation study to check which
# is more efficient estimator of the E(X)
# the sample median? Both estimators
# are unbiased and consistent, nonetheless
# everyone is using the mean! Sensible?

# Simulation set up:

# 1. Fix, n, E(X), sd(X), R

n <- 10^2
E_X <- 2
sd_X <- 8
R <- 10^4

# 2. Generate a rth sample of n
# N(E(X),sd(X))
# 3. calc mean and median
# 4. Repeat R times 
mean_X <- c()
median_X <- c()
for (i in 1: R){
  X <- rnorm(n, E_X, sd_X)
  mean_X <- c(mean_X,mean(X))
  median_X <- c(median_X,median(X))
}

# 5. Calculate standard deviation
sd(mean_X)
sd(median_X)


# Result: SD of median is always higher


