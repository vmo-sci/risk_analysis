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
  all_serviced <- c()
  lost <- 0       # customers lost due to full queue
  all_lost <- c()
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
    } else {
      s <- new_p
      serviced <- serviced + s
    }
    # Count the number of occupied beds
    n <- c(n,length(beds))
    # Lost patients per day
    all_lost <- c(all_lost, lost)
    all_serviced <- c(all_serviced, serviced)
    
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
  return(list(n, all_lost, all_serviced))
}

# hospital_40 will contain

hospital_40 <- hospital_simulation(nr_beds = 40, t.end = 100+365)
n <- unlist(hospital_40[1])
all_lost <- unlist(hospital_40[2])
all_serviced <- unlist(hospital_40[3])

# occupancy rate: 
n_observed <- n[101:465] # After 100 days we start observing hospital

occupancy_rate <- n_observed/40

# Solution to the first question:
plot(occupancy_rate, type = 'l')

# Solution to the second question:
hospital_40 <- hospital_simulation(nr_beds = 40, t.end = 100+10000)
lost <- unlist(hospital_40[2])
plot(lost, type = 'l')
serviced <- unlist(hospital_40[3])
plot(serviced, type = 'l')

lost[length(lost)]/(serviced[length(serviced)] + lost[length(lost)])

# 12.3% patients are lost

# Solution to the third question
mean(n/40 <0.85) # We want this number to be 0.95 or greater

end_result <- c()
for(nr_bed in 1:100){
  result <- hospital_simulation(nr_beds = nr_bed, t.end = 100)
  bed_occ <- unlist(result[1])
  occ_rate <- bed_occ/nr_bed
  
  lower_85 <- mean(occ_rate <= 0.85)
  end_result <- c(end_result, lower_85)
}

end_result
end_result[63]
end_result[64]

h_64 <- hospital_simulation(nr_beds = 64, t.end = 10^4)
nr_patients <- unlist(h_64[1])/64
mean(nr_patients <= 0.85)

# Solution to problem 3:
# We need 64 beds

# Plotting----
# function for creating file name with leading zeros
# makes it easier to process them sequentially
rename <- function(x){
  if (x < 10) {
    return(name <- paste('000',i,'plot.png',sep=''))
  }
  if (x < 100 && i >= 10) {
    return(name <- paste('00',i,'plot.png', sep=''))
  }
  if (x >= 100) {
    return(name <- paste('0', i,'plot.png', sep=''))
  }
}
# Create pictures
for(i in (1:365)){
  name <- rename(i)
  png(name) # save picture
  plot(n, 
       xlim = c(1, i),
       type = 'l',
       main = paste('Hospital simulation', i, 'days'),
       xlab = 'Days',
       ylab = 'Number of patients')
  dev.off()
}

library(gifski)
png_files <- list.files(".", pattern = "./*png$", full.names = TRUE)
gifski(png_files, gif_file = "animation.gif", delay = 0.1)


