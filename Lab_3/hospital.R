#For the Assignment:
#Submit the R-code and the a desciption 
#of the answers


p <- 1/2.9 #Mean of geom dist starting at 1
stay_days <- rgeom(1000, p)
stay_days <- stay_days + 1
mean(stay_days)

# First run the system for X days
# wait until System is stable (number of patients is filled)
# Then do the simulations