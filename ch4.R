set.seed(0)
n_runs = 10^5

sample <- rbinom(n_runs, 100, 3/4+1/16)
mean(sample>79)

# Calculate with approximation

mean_S = 100*(13/16)
var_S = 100*(13/16)*(3/16)

Z = (79-mean_S)/sqrt(var_S)
1-pnorm(Z)

# problem 15
nr_appartments <- 200
random_parking <- runif(nr_appartments)
random_parking
test <- random_parking(ifelse(TRUE, 1, 0))
test

