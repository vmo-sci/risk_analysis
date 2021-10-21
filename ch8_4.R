# Chapter 8.4
# Problem 44

# Online code ----
# with help from code:
# code from https://www.r-bloggers.com/2010/05/simulating-a-queue-in-r/ 


t.end   <- 10^5 # duration of sim
Ta <- 1         # poisson rate: arrival
Ts <- 1         # poisson rate: service 
max_n <- 1      # max number in system


start_simulation <- function(){
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
        tn <- t.clock
        
        # If it is the first one in queue -> set next departure
        if(n == 1) { 
          tb <- t.clock
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


start_simulation()
