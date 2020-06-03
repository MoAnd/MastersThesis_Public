Weibull_renewal_match_sim <- function(theta) {
  weibsim1 <- apply(matrix(rweibull(15*1, shape = theta[2], scale = theta[1]^(-1/theta[2])), 1, 15),1, cumsum)
  weibsim1[weibsim1 > 1] <- NA
  home <- split(weibsim1, col(weibsim1))$`1`
  
  weibsim1 <- apply(matrix(rweibull(15*1, shape = theta[4], scale = theta[3]^(-1/theta[4])), 1, 15),1, cumsum)
  weibsim1[weibsim1 > 1] <- NA
  away <- split(weibsim1, col(weibsim1))$`1`
  
  return(list(Home = as.numeric(na.omit(home)), Away = as.numeric(na.omit(away))))
  
}

# Weibull_renewal_match_sim(theta)