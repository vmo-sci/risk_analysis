book_sample <- c(0.0, 0.0, 0.0, 0.0, 1.7,
                 2.3, 2.9, 3.3, 4.0, 4.5, 
                 5.3, 6.1, 6.7, 7.9, 8.9, 
                 10.3, 11.1, 12.2, 13.1, 14.4,
                 16.0, 17.4, 21.0, 24.4, 29.4, 
                 33.5, 42.1, 52.2, 93.9, 353.7)
my_sample <- c(book_sample, 14.1, 143.6)
# for debugging: checking out the values of the book
# my_sample <- book_sample

# 1. normal approximation ----

z <- 1.96 #95% quantile
n <- length(my_sample)
sample_mean <- mean(my_sample)
lb <- sample_mean - z*(sd(my_sample)/sqrt(n))
ub <- sample_mean + z*(sd(my_sample)/sqrt(n))

# Mean, lower bound, upper bound
print(c(sample_mean, lb, ub))

# 2. Bootstrap ----
set.seed(10)
br <- 1000 # Number of bootstrap sample
br_means <- c()
for (i in 1:br){
  # Generate a bootraps sample of size n
  current_sample <- sample(my_sample, n, replace = TRUE)
  # Calculate the mean
  br_means <- c(br_means, mean(current_sample))
}
# Get mean of bootstrap means ----
br_mean_means <- mean(br_means)
br_mean_means
# Get bootstrap variance ----
br_variance_means <- var(br_means)
br_variance_means

# Standard error ----
var(my_sample)/n

# As we can see bootstrap variance and
# standard error are similar

# 3. Calculate confidence interval ----
# do a new bootstrap
br <- 1000
br_variances <- c()
for (i in 1:br) {
  # Generate a bootstrap sample of size n
  current_sample <- sample(my_sample, n, replace = T)
  # Calculate Variance
  cur_var <- var(current_sample)
  br_variances <- c(br_variances, cur_var)
  
}
# mean bootstrap variance
var_mean <- mean(br_variances)

# variance of bootstrap variance
var_var <- var(br_variances)

# confidenc interval
var_lb <- quantile(br_variances, 0.025)
var_ub <- quantile(br_variances, 0.975)

# mean, var, lower bound, upper bound of BS Var
print(c(var_mean, var_var, var_lb, var_ub))



