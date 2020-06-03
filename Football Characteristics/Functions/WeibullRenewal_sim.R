WeibullRenewal_sim <- function (theta, m = 1000000, numCores = 8, listed = FALSE) {
  
  par <- solve(100000, m)
  
  doParallel::registerDoParallel(numCores)
  
  sim <- foreach::foreach (i=1:par) %dopar% {
    weibsim1 <- apply(matrix(rweibull(15*100000, shape = theta[2], scale = theta[1]), 100000, 15),1, cumsum)
    weibsim1[weibsim1 > 1] <- NA
    split(weibsim1, col(weibsim1))
  }
  
  doParallel::stopImplicitCluster()
  
  if(!listed){
    sim <- sim %>% unlist() %>% na.omit()
  }

  return(sim)
  
}
