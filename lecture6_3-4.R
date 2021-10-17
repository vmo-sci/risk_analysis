# Lecture 6_3-4
# Estimate a 95% confidence interval

set.seed(630829)
# Generate 
x<-rnorm(100,1,10) #N = 100, X~N(1,10)
# Compute lower bound with (Prop 6.3.2)
q <- 0.95
z <- qnorm((1+0.95)/2)
t1 <- mean(x) - z*sd(x)/sqrt(100)
t2 <- mean(x) + z*sd(x)/sqrt(100)
mean(x)
print(c(t1,t2))

hist(x)



# Bootsstrap
br <- 999 # Bootsstrap replicates, it's easier to get 97.5 quantile with 999
mean_boot <- NULL
for(i in 1:br) {
  x_boot<-sample(x,100,replace=TRUE)
  mean_boot <- c(mean_boot,mean(x_boot))
}


print(c(mean(mean_boot),sd(mean_boot)))
# 1. Classic approach
# 1.96 is 95% quantile
t1_a<-mean(mean_boot)-1.96*sd(mean_boot)
t2_a<-mean(mean_boot)+1.96*sd(mean_boot)
print(c(t1_a,t2_a))
# 2. Bootstrap approach
quantile(mean_boot, probs=c(0.025,0.975))


# We conduct an illustration with R large if the confidence interval is 
# covering the real_mean
cover <- c()
R <- 10^4
# progress bar 
pb = txtProgressBar(min = 0, max = R, initial = 0) 
for (i in 1:R){
  setTxtProgressBar(pb,i)
  real_mean <- 1
  x<- rnorm(100,real_mean,10)
  t1 <- mean(x) - z*sd(x)/sqrt(100)
  t2 <- mean(x) + z*sd(x)/sqrt(100)
  if(t1<real_mean&t2>real_mean){
    cover <- c(cover,1)
  } else {
    cover <- c(cover,0)
  }
  
}
close(pb)
mean(cover)


