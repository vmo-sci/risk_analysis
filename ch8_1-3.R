# Problem 4 ----

x <- rgeom(n = 100000, prob = 0.2)

mean(x)



P <- matrix(c(0.8, 0.1, 0.1,
         0.6, 0.2, 0.2, 
         0, 0.6, 0.4), nrow = 3, ncol = 3, byrow = T)
P

P_2 <- P %*% P
P_4 <- P_2 %*% P_2
P_4

library(expm)
P %^% 100


# Random Walk ----
set.seed(10)
start <- 5
p <- 0.6 #probability that it increases
ruin <- 0 # If lower than this we are bankrupt

n <- 10^4
end_result_20 <- c()
end_result_200 <- c()


for (i in 1:n){
  # Loading bar
  if (i %% (n/100) == 0){
    print(i/(n/100))
  }
  X <- runif(200) # create 200 steps
  steps <- 2*(X<0.6) - 1 #setup steps
  
  current <- start
  for(i in 1:length(steps)){
    # If we are above bankruptcy: take a step
    if(current >= ruin){
      current <- current + steps[i]
    }
    
    # if we took 20 steps
    if (i == 20){
      end_result_20 <- c(end_result_20,current)
    }
  } 
  end_result_200 <- c(end_result_200,current)
  
}
  
# Probability of being in business
mean(end_result_20 >= ruin)
mean(end_result_200 >= ruin)

# All not-ruined runs:
non_ruined_20 <- end_result_20[end_result_20>=ruin]
non_ruined_200 <- end_result_200[end_result_200>=ruin]

# Equity of not ruined
mean(non_ruined_20)
mean(non_ruined_200)
# Variance of not ruined
var(non_ruined_20)
var(non_ruined_200)


# Results
# % Profit with 20 steps: 0.9594
# % Profit with 200 steps: 0.9132
# Mean(Equity) if profiting 20 steps: 9.335001
# Mean(Equity) if profiting 200 steps: 46.17455
# Var(Equity) if profiting 20 steps: 16.48278
# Var(equity) if profiting 200 steps: 176.3169

