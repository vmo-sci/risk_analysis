b <- c(11, 12, 17, 19, 20,
       20, 24, 28, 31, 36,
       50, 51, 52, 55, 61,
       72, 73)
length(b) == 17


sum(b)

lambda <- 17/932
lambda

## Hist for a and b
n <- 10^5
A <- rexp(n, rate = 1)
hist(A, freq = T, breaks = 50)
B <- rexp(n, rate = 0.1)
hist(B, freq = T, breaks = 50)


expected <- 1/lambda
expected

# Task 2
interval <- 1.96 * (1/lambda) *(1/sqrt(20))
interval

c_interval_mean <- c(expected - interval, expected + interval)
c_interval_mean
c_interval_lambda <- 1/c_interval_mean
c_interval_lambda

# Calc P(B is worse)
z <- (800 - 20 * (expected))/((expected)*sqrt(20))
z
pnorm(z)

# Task 3
exponent <- -120*lambda
result <- exp(exponent)
result

# Simulation assuming the mean is normally distributed
n <- 10^4
set.seed(12)
x_mean <- rnorm(n, mean = expected, sd = expected)

test_21 <- c()
for (x_sample in x_mean){
  temp <-  rexp(1, 1/x_sample)
  if (is.nan(temp)){
    print(x_sample)
  }
  test_21 <- c(test_21,temp)
  
}
#result
mean(test_21 >120, na.rm = T)
