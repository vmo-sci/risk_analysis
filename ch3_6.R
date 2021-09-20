# Chapter3.6
# Problem 43 ----
# Let X and Y be independent and unif [0,1]. Find the cdf and pdf of the 
# random variables

# a) |X-Y| ----
nr_runs <- 100000
X <- runif(nr_runs)
Y <- runif(nr_runs)
Z <- abs(X - Y)

hist(Z, freq = FALSE) # pdf
plot(ecdf(Z)) # cdf

# b) X/(X+Y) ----
Z_2 <- X/(X+Y)

hist(Z_2, freq = FALSE) #pdf
plot(ecdf(Z)) #cdf


# Problem 45 ----
# In Problem 34 (Adam uniformly distributed
# between 12:30 and 13:00, Billy uniformly 
# distributed between 12:30 and 13:15), 
# compute the expected time that ...
set.seed(12)
nr_runs <- 100000
adam <- runif(nr_runs, 0, 30)
billy <- runif(nr_runs, 0, 45)

# a) Billy bob must wait for Adam ----
wait_time <- adam-billy
billy_waiting <- wait_time[wait_time > 0]
print("Billy average waiting time")
mean(billy_waiting)
# Billy will wait approx 10 minutes


# b) The first person to arrive must wait for the second ----
abs_wait_time <- abs(adam-billy)
print("Average waiting time")
mean(abs_wait_time)

