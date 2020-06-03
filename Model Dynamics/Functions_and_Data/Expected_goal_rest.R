
Expected_goal_rest <- function (match, theta, par = 20, time, Ti) {
  
  home <- rep(NA, length(time))
  for(i in 1:length(match$Home)){
    home[first(which(match$Home[i] <= time))] <- i
  }
  home[1] <- 0
  home <- na.locf(home)
  
  away <- rep(NA, length(time))
  for(i in 1:length(match$Away)){
    away[first(which(match$Away[i] <= time))] <- i
  }
  away[1] <- 0
  away <- na.locf(away)
  
  df <- tibble(Time = time, Home = home, Away = away)
  
  ind_home <- which(c(0,diff(df$Home)) > 0)
  ind_away <- which(c(0,diff(df$Away)) > 0)
  
  inds <- c(ind_home, ind_away)
  
  Nt_last1 <- rep(NA,nrow(df))
  Nt_last1[1] <- 0
  for(i in ind_home){
    Nt_last1[i] <- df$Time[i]
  }
  Nt_last1 <- na.locf(Nt_last1)
  
  Nt_last2 <- rep(NA,nrow(df))
  Nt_last2[1] <- 0
  for(i in ind_away){
    Nt_last2[i] <- df$Time[i]
  }
  Nt_last2 <- na.locf(Nt_last2)
  
  df <- df %>% bind_cols("Nt_last_home" = Nt_last1) %>% bind_cols("Nt_last_away" = Nt_last2)
  
  sim_list <- list()
  
  for (k in 1:(length(inds)+1)) {
    
    ind <- c(1,inds)[k]
    
    Nt_last_time <- c(df$Nt_last_home[ind], df$Nt_last_away[ind])
    Nt <- c(df$Home[ind], df$Away[ind])
    
    numCores <- 6
    doParallel::registerDoParallel(numCores)
    
    sim <- foreach::foreach (i=1:par) %dopar% {
      
      # Simulate two independent Weibull Renewal Processes up to time Ti
      weibsim1 <- apply(matrix(rweibull(15*200000, shape = theta[2], scale = theta[1]^(-1/theta[2])), 200000, 15),1, cumsum)
      weibsim1[weibsim1 > (Ti - Nt_last_time[1])] <- NA
      weibsim1 <- weibsim1 + Nt_last_time[1]
      
      weibsim2 <- apply(matrix(rweibull(15*200000, shape = theta[4], scale = theta[3]^(-1/theta[4])), 200000, 15),1, cumsum)
      weibsim2[weibsim2 > (Ti - Nt_last_time[2])] <- NA
      weibsim2 <- weibsim2 + Nt_last_time[2]
      
      list("Home" = weibsim1, "Away" = weibsim2)
      
    }
    
    doParallel::stopImplicitCluster()
    
    cat("Simulation ", k, " done!")
    
    if(k == (length(inds)+1)) {
      ind2 <- nrow(df)
    } else {
      ind2 <- c(1,inds)[k+1] - 1
    }
    
    pb <- txtProgressBar(min = ind, max = ind2, style = 3)
    
    for (j in ind:ind2) {
      
      t <- df$Time[j]
      
      sim2 <- foreach::foreach (i=1:length(sim)) %do% {
        
        weibsim1 <- sim[[i]]$Home
        weibsim2 <- sim[[i]]$Away
        
        if (t == 0) {
          
          ngoals1 <- colSums(weibsim1 > 0, na.rm = TRUE)
          ngoals2 <- colSums(weibsim2 > 0, na.rm = TRUE)
          
          list("ngoals1" = ngoals1, "ngoals2" = ngoals2)
          
        } else {

          w1 <- weibsim1[,-union(which(weibsim1[1,] < t), which(weibsim2[1,] < t)), drop = FALSE]
          w2 <- weibsim2[,-union(which(weibsim1[1,] < t), which(weibsim2[1,] < t)), drop = FALSE]
          
          ngoals1 <- colSums(w1 > 0, na.rm = TRUE) + Nt[1]
          ngoals2 <- colSums(w2 > 0, na.rm = TRUE) + Nt[2]
          
          list("ngoals1" = ngoals1, "ngoals2" = ngoals2)
          
        }
      }
      
      ngoals1 <- c()
      ngoals2 <- c()
      
      for (l in 1:length(sim2)){
        ngoals1 <- c(ngoals1, sim2[[l]][[1]])
        ngoals2 <- c(ngoals2, sim2[[l]][[2]])
      }
      
      sim_list[[j]] <- list("Home" = mean(ngoals1), "Away" = mean(ngoals2))
      
      setTxtProgressBar(pb, j)
      
    }
    
    close(pb)
    
  }
  
  return(sim_list)
  
}
