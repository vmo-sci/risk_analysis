#1. Illustrating interval estimation with just ONE instance
x<-rnorm(100,1,10) 
#1.96 is derived by Z value of (1+q)/2 where q is 0.95
t1<-mean(x)-1.96*sd(x)/sqrt(100) #lower confidence limit
t2<-mean(x)+1.96*sd(x)/sqrt(100) #upper confidence limit

mean(x) #this is our estimator
print(c(t1,t2))
#2. We conduct an illustration with R large

cover<-0
for (i in 1:1000)
{
  x<-rnorm(100,1,10)
  t1<-mean(x)-1.96*sd(x)/sqrt(100) #lower confidence limit
  t2<-mean(x)+1.96*sd(x)/sqrt(100) #upper confidence limit
  print(c(t1,t2))
  #if(t1<1&t2>1) print("Covers") else print("Not Cover")
  if(t1<1&t2>1) cover<-cover+1
}

cover/1000
#3, Illustrate the Bootstrap Method

br<-999 #bootstrap replicates
mean_boot<-NULL

for (i in 1:br){
  
  x_boot<-sample(x,100,replace=T) #x is being reused from x<-rnorm(100,1,10). Takes 100 of those and saves in x_boot
  mean_boot<-c(mean_boot,mean(x_boot))} #get the means and add to the mean_boot vector

t1_a<-mean(mean_boot)-1.96*sd(mean_boot) #lower confidence limit
t2_a<-mean(mean_boot)+1.96*sd(mean_boot) #upper confidence limit

print(c(mean(mean_boot),sd(mean_boot)))
print(c(t1_a,t2_a))
