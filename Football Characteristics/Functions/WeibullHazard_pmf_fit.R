source("Football Characteristics/Functions/WeibullProcess_sim.R")

WeibullHazard_pmf_fit <- function(theta, emperical_dat, mean_val = NULL, end_time = 93, m = 1000000) {
  
  ind <- which(emperical_dat>end_time)
  
  if (!purrr::is_empty(ind)){
    emperical_dat <- emperical_dat[-ind]
  }
  
  emp_prob <- table(emperical_dat)/length(emperical_dat)
  
  if(is.null(mean_val)) {
    sim <- WeibullProcess_sim(theta, m = m)
  } else {
    theta <- c(mean_val/theta, theta)
    sim <- WeibullProcess_sim(theta, m = m)
  }
  
  sim <- (sim * end_time) %>% ceiling()
  
  probs <- table(sim)/length(sim)
  
  return(1/length(probs) * sum(sqrt((probs - emp_prob)^2)))
  
}

# test <- WeibullHazard_pmf_fit(theta = 1.12, emperical_dat = home_goals_time, mean_val = 1.536491, end_time = 93, m = 1000000)



# Optim -------------------------------------------------------------------

# tictoc::tic()
# optim(par = 1.15, fn = WeibullHazard_pmf_fit, emperical_dat = home_goals_time, mean_val = 1.536491, end_time = 93, m = 1000000, lower = 1, upper = 1.5, method = "Brent")
# tictoc::toc()



