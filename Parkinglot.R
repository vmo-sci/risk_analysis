
# Generate 200 appartments
# every apparment has either 0,1 or 2 parking lots
result <- c()

for (x in c(1:10000)){
  n<-200
  
  cars<-runif(200,0,10)
  prob<-c()
  
  for (i in cars){
    # 10% have no parking lot
    if(i<=1){
      prob<-append(prob,0)
    }
    # 60% have 1 parking lot
    else if (i<=7){
      prob<-append(prob,1)
    }
    # 30% have 2 parking lots
    else {
      prob<-append(prob,2)
    }
  }
  prob
  result <- append(result,sum(prob))
}
# try out different nr_of_parkinglots until we have 95%

# start with the mean (~50%)
mean(result)

sum(result<=240)/10000

# Try out different values until you get to 95%
sum(result<=254)/10000
sum(result<=253)/10000

# Remove everything so we can see a diffrent solution
rm(list = ls())

# Kenneth solution----
#Prob Ch4:15
rep<-10000
z<-NULL
for (i in 1:rep) { # Use 1:rep instead of c(1:rep)
  x=runif(200)
  z1=(x>0.1)+(x>0.7) #this is so smart!
  z<-c(z,sum(z1)) # Use this instead of append
}
quantile(z,0.95)

# Qnorm solution----
n <- 200
e_x <- 1.2
var_x <- 0.36

# get .95 quantile
qnorm(.95, e_x*n, sqrt(n*var_x))
