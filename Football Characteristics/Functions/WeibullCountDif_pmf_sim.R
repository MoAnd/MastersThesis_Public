
WeibullCountDif_pmf_sim <- function(theta, m = 1000000, Ti = 1, numCores = 8) {
  
  par <- solve(200000, m)
  
  doParallel::registerDoParallel(numCores)
  
  sim <- foreach::foreach (i=1:par) %dopar% {
    
    weibsim1 <- apply(matrix(rweibull(15*200000, shape = theta[2], scale = theta[1]), 200000, 15),1, cumsum)
    weibsim1[weibsim1 > Ti] <- NA
    
    weibsim2 <- apply(matrix(rweibull(15*200000, shape = theta[4], scale = theta[3]), 200000, 15),1, cumsum)
    weibsim2[weibsim2 > Ti] <- NA
    
    ngoals1 <- colSums(weibsim1 > 0, na.rm = TRUE)
    ngoals2 <- colSums(weibsim2 > 0, na.rm = TRUE)
    
    list("ngoals1" = ngoals1, "ngoals2" = ngoals2)
    
  }
  
  doParallel::stopImplicitCluster()
  
  
  ngoals1 <- c()
  ngoals2 <- c()
  
  for (i in 1:length(sim)){
    ngoals1 <- c(ngoals1, sim[[i]][[1]])
    ngoals2 <- c(ngoals2, sim[[i]][[2]])
  }
  
  return(table(ngoals1-ngoals2))
  
}

# theta <- c(0.6632844, 0.9062415, 0.9354191, 0.8491849)
# m <- 50000000
# test <- WeibullCountDif_pmf_sim(theta)

