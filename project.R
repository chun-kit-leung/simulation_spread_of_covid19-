sim_mask_covid19 <- function(mask_compliance){
  # Parameters
  death_rate <- 0.00288  # Probability of death for an infected person
  num_people <- 75000  # Total number of people in the population
  num_infected <- 10  # Initial number of infected people
  infected_prob_no_mask <- 0.02  # Probability of infection for an unmasked person who is exposed to the virus
  infection_prob_with_mask <- 0.002  # Probability of infection for a masked person who is exposed to the virus
  # mask_compliance <- 0.5   # Proportion of people who wear masks
  recovery_rate <- 0.15  # Probability of recovery for an infected person
  
  # Initialize population
  mask_status <- sample(c(0,1), num_people, replace=TRUE, prob=c(1-mask_compliance, mask_compliance))
  infection_status <- rep(0, num_people)
  infection_status[sample(1:num_people, num_infected)] <- 1
  
  # Simulation loop
  num_iterations <- 365
  num_susceptible <- numeric(num_iterations)
  num_infected <- numeric(num_iterations)
  num_recovered <- numeric(num_iterations)
  num_deaths <- numeric(num_iterations)
  
  for (i in 1:num_iterations) {
    
    # Calculate infection probabilities for each person
    infection_prob <- ifelse(mask_status == 0, infected_prob_no_mask, infection_prob_with_mask)
    
    # Infect susceptible people who are exposed to the virus
    num_susceptible[i] <- sum(infection_status == 0)
    num_infected[i] <- sum(infection_status == 1)
    num_recovered[i] <- sum(infection_status == 2)
    num_deaths[i] <- sum(infection_status == 3)
    
    for (j in 1:num_people) {
      if (infection_status[j] == 0) {
        if (runif(1) < infection_prob[j]) {
          infection_status[j] <- ifelse(runif(1) < death_rate, 3, 1) # 1: infected, 3: dead
        }
      } else if (infection_status[j] == 1) {
        if (runif(1) < recovery_rate) {
          infection_status[j] <- 2 # 2: recovered
        } else if (runif(1) < death_rate) {
          infection_status[j] <- 3 # 3: dead
        }
      }
    }
  }
  return(data.frame(num_susceptible, num_infected, num_recovered, num_deaths))
}

result_10 <- sim_mask_covid19(0.1)
result_50 <- sim_mask_covid19(0.5)
result_90 <- sim_mask_covid19(0.9)

# plot(result_10$num_deaths, col = "red", type = "b", main = "Virus spread with and without mask (deaths)", xlab = "Days", ylab = "Number of deaths")
# lines(result_50$num_deaths, col = "green", type = "b")
# lines(result_90$num_deaths, col = "blue", type = "b")
# legend("topleft", c("10%", "50%","90%"), col = c("red", "green",'blue'), lty = 1)
# 

plot(result_10$num_susceptible, col = "red", type = "b", main = "Virus spread with and without mask (susceptible)", xlab = "Days", ylab = "Number of susceptible")
lines(result_50$num_susceptible, col = "green", type = "b")
lines(result_90$num_susceptible, col = "blue", type = "b")
legend("topright", c("10%", "50%","90%"), col = c("red", "green",'blue'), lty = 1,)

