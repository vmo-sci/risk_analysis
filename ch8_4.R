# Chapter 8.4
# Problem 44

# Online code ----
# with help from code:
# code from https://www.r-bloggers.com/2010/05/simulating-a-queue-in-r/ 


t.end   <- 10^5 # duration of sim
Ta <- 1         # poisson rate: arrival
Ts <- 1         # poisson rate: service 
max_n <- 1      # max number in system


start_simulation_waiting <- function(){
  # Reset values
  t.clock <- 0    # sim time
  t1 <- 0         # time for next arrival
  t2 <- t.end     # time for next departure
  n <- 0          # number in system
  serviced <- 0   # customers serviced
  lost <- 0       # customers lost due to full queue
  while (t.clock < t.end) {
    
    
    #################
    # arrival event #
    #################
    if (t1 < t2) {      
      t.clock <- t1 # move clock forward to t1
      t1 <- t.clock + rexp(1, Ta) # setup new arrival time
      
      # If queue is not full
      if (n < max_n) {
        n <- n + 1
        
        # If it is the first one in queue -> set next departure
        if(n == 1) { 
          t2 <- t.clock + rexp(1, Ts)  # poisson interarrival period
        }
      } else {
        lost <- lost + 1 # You lost a customer!
      }
      ###################
      # departure event #
      ###################
    } else { 
      t.clock <- t2 # move clock forward to departure time
      n <- n - 1
      if (n > 0) { 
        t2 <- t.clock + rexp(1, Ts)  # poisson service period
      }
      else { 
        t2 <- t.end # No customers in line
      }
      serviced <- serviced + 1
    }   
  }
  
  # print results
  # percentage of customers lost
  print('lost:')
  print(lost)
  print('serviced:')
  print(serviced)
  print('ratio lost')
  print(lost/ (lost+serviced))
}


start_simulation_waiting()

# Double service rate
Ts <- 2         # exp rate: service 
start_simulation_waiting()

# Now one more queue room
Ts <- 1 # reset to default

start_simulation_waiting()

# Simulation with multiple servers ----
start_simulation_servers <- function(servers){
  set.seed(12)
  # Reset values
  t.arrive <- 1   # time for next arrival
  # time for next departure
  t.depart <- c()
  for (i in 1:servers){ 
    t.depart[i] <- t.end
  }
  n <- 0          # number in system
  
  plot_n <- c()   # development of customers in system
  serviced <- 0   # customers serviced
  lost <- 0       # customers lost due to full queue
  t.clock <- 0    # starting time

    
  while (t.clock < t.end) {
    print(t.clock)
    plot_n <- c(plot_n, n)
    #################
    # arrival event #
    #################
    if (t.arrive < t.depart[1]) {
      t.clock <- t.arrive
      t.arrive <- t.clock + rexp(1, Ta) # setup new arrival time
      
      # If queue is not full
      if (n < servers) {
        n <- n + 1
        t.depart[servers] <- t.clock + rexp(1, Ts) # Setup new depart time
        t.depart <- sort(t.depart) # Invariant t.depart is always sorted
        
        serviced <- serviced + 1 # You serviced a customer
      } else {
        lost <- lost + 1 # You lost a customer!
      }
      ###################
      # departure event #
      ###################
    } else { 
      n <- n - 1
      t.clock <- t.depart[1]
      t.depart[1] <- t.end 
      t.depart <- sort(t.depart) # Invariant t.depart is always sorted
      
    }
    
  }   
  
  # print results
  # percentage of customers lost
  print('lost:')
  print(lost)
  print('serviced:')
  print(serviced)
  print('ratio lost')
  print(lost/ (lost+serviced))
  # Show system development
  plot(plot_n[(l-100):l],type = 'l', main = 'First 100 states', 
       xlab = 'states', ylab = 'customers')
  
  # last 100 development
  l <- length(plot_n)
  plot(plot_n[(l-100):l],type = 'l', main = 'Last 100 states', 
       xlab = 'states', ylab = 'customers')
  return(plot_n)
}
t.end <- 10^4
plot_n <- start_simulation_servers(servers = 2)
l <- length(plot_n)


