#For the Assignment:
#Submit the R-code and the a desciption 
#of the answers

# Length of stay - geometric with mean(2.9), 
# with k = {1,2,3,4,...}
# Therefore: p <- 1/2.9 
# Therfore rgeom(n, 1/p) + 1 //we need to add +1
# //since R starts with 0

#Proof of concept
mean(rgeom(10^5, 1/2.9)+1) # should be 2.9

# Daily patient inflow: Poisson with mean (14.8)
# Mean = Lambda, proof of concept:
mean(rpois(10^5, 14.8)) # Should be 14.8

# Simulation for hospital
# nr_bed: how many beds has the hospital
# t.end: how much time does the simulation take
hospital_simulation <- function(nr_beds, t.end){
  if(t.end < 100){
    print("Simulate at least 100 days!")
    return(0)
  }
  set.seed(12)
  
  n <- c()        # Vector of number of patients
  beds <- c()     # days the beds are occupied
  serviced <- 0   # customers serviced
  lost <- 0       # customers lost due to full queue
  t.clock <- 0    # starting time
  
  
  while (t.clock <= t.end) {
    # Counter
    print(c("Simulating day", t.clock))
    # One Day passes ----
    t.clock <- t.clock + 1
    if(length(beds) != 0){
      # Lenght of stay goes down one day
      beds <- beds - 1
      # Kick out healthy patients
      beds <- beds[beds > 0]
    }
    # Count the number of occupied beds
    n <- c(n,length(beds))
    
    # New patients arrive ----
    new_p <- rpois(1, 14.8) # Amount of new patients
    beds_new <- rgeom(new_p, 1/2.9)+1 # Time they will stay
    # put new patients in beds
    beds <- c(beds, beds_new)
    # Kick out the patients that don't fit
    if (length(beds) > nr_beds){
      l <- (length(beds) - nr_beds) # lost today
      lost <- lost + l
      
      s <- new_p - l                # serviced today
      serviced <- serviced + s
      # Only nr_beds people can stay
      # Will only kick out people who just moved in
      beds <- beds[1:nr_beds]  
    }
  }   
  
  # Print out the Results ----
  # percentage of customers lost
  print('lost:')
  print(lost)
  print('serviced:')
  print(serviced)
  print('ratio lost')
  print(lost/ (lost+serviced))
  # Show first 100 days
  
  plot(n[1: 100],type = 'l', main = 'First 100 days', 
       xlab = 'day', ylab = 'patients')
  
  # last 100 days

  l <- length(n)
  plot(n[(l-100):l],type = 'l', main = 'Last 100 days', 
       xlab = 'day', ylab = 'patients')
  return(n)
}

# hospital_40 will contain
hospital_40 <- hospital_simulation(nr_beds = 40, t.end = 10^3)

