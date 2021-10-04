rate <- 1
n <- 5*10^3
rep <- 5*10^3
result <- c()

for (i in c(1:rep)){
  result <- append(result, mean(rexp(n, rate = rate)))
}

hist(result, freq = FALSE, breaks = 50)
