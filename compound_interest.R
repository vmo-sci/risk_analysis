set.seed(123)
num_trials <- 10^4

# Investment with mean 0.04 interest rate

num_years <- 30

all_investments <- c()

for (n in c(1:num_trials)){
  investment <- 1
  for (i in c(1:30))
  {
    random_interest <- rnorm(1, mean=0.04, sd = 0.04)
    investment <- investment*(1+random_interest)
  
  }

  all_investments <- append(all_investments, investment)
}

hist(all_investments, freq = FALSE, breaks = 100)
summary(all_investments)

x <- rnorm(10000, mean = 0.04, sd = 0.04)
summary(x)
hist(x, breaks = 100)
