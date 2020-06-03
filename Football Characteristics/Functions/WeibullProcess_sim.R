WeibullProcess_sim <- function (theta, m = 1000000, numCores = 8, listed = FALSE) {
  
  weib_haz <- function(t) return((theta[1]) * theta[2] * t^(theta[2]-1))
  
  if (m >= 100000) {
    par <- solve(100000, m)
    
    doParallel::registerDoParallel(numCores)
    
    sim <- foreach::foreach (i=1:par) %dopar% {
      rpgm::rinpoisson(100000, lambda = weib_haz)
    }
    
    doParallel::stopImplicitCluster()
    
  } else {
    sim <- rpgm::rinpoisson(m, lambda = weib_haz)
  }
  
  if(!listed){
    sim <- sim %>% unlist() %>% na.omit()
    ind <- which(sim > 0.9999999)
    if(!purrr::is_empty(ind)){
      sim <- sim[-ind]
    }
  }
  
  return(sim)
  
}

# WeibullProcess_sim(theta = c(1.35, 1), m = 1, listed = TRUE)

