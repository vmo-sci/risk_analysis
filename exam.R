# problem 1 ----
current_p <- 0.088 + 0.068 + 0.133 + 0.237 + 0.052 + 0.105 + 0.092 + 0.008
1-current_p

# problem 2 ----
p_I_2 <- 0.133 + 0.052 + 0.008
p_I_2

# problem 3 ----
0.088 + 0.068 + 0.237 + 0.217 + 0.105 + 0.092

# problem 4 ----
f_i_03 <- 0.088 + 0.237 + 0.105 
f_d_12 <- -0.088 -0.068 -0.133 -0.237 - 0.217 - 0.052 
f_i_03 + f_d_12

# problem 5 ----
e_d_1 <- 0.237 + 0.217 + 0.052
e_d_2 <- 2 * (0.105 + 0.092 + 0.008)
e_d <- e_d_1 + e_d_2
e_d

# problem 6 ----
p_d0 <- 0.088 + 0.068 + 0.133
e_i_d0 <- (0.068*1 + 0.133*2)/p_d0
e_i_d0

# problem 7 ----
e_i_1 <- 0.068 + 0.217 + 0.092
e_i_2 <- 2*(0.133 + 0.052 + 0.008)
e_i <- e_i_1 + e_i_2
e_i

# problem 11 + 12 + 13
n <- 10**6 # number of observations
die_1 <- as.integer(runif(n, 1, 7))
die_2 <- as.integer(runif(n, 1, 7))
die_3 <- as.integer(runif(n, 1, 7))

X <- die_1+die_2+die_3
# all outcomes
sort(unique(X))

# 12
length(X[X==2])/n
# 13
length(X[X==6])/n



# problem 14 + 15 ----

n <- 1000000 # number of observations

q_1 <- rbinom(n, 15, 0.5)
q_2 <- rbinom(n, 5, 0.6)
q_3 <- rbinom(n, 22, 0.4)
q_4 <- rbinom(n, 14, 0.3)

t <- q_1 + q_2 + q_3 + q_4

print('Expected value of t:')
mean(t)

bad_outcomes <- length(t[t<20]) + length(t[t>30])
probability <- bad_outcomes/n

print('Solution for 15')
probability
