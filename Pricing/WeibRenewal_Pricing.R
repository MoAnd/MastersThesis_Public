source("Pricing/OU_func.R")

pacman::p_load(parallel)
pacman::p_load(foreach)
pacman::p_load(doParallel)
pacman::p_load(tictoc)

WeibRenewal_Pricing <- function (theta = c(1/1.5, 1, 1/1.5, 1), m = 1000000, Ti = 1, t = 0, Nt = c(0,0), Nt_last_time = c(0,0), renewal_now = FALSE, cores_in_par = 8) {
  
  if ((Nt[1] == 0 & Nt_last_time[1] != 0) | (Nt[2] == 0 & Nt_last_time[2] != 0) | (Nt[1] != 0 & Nt_last_time[1] == 0) | (Nt[2] != 0 & Nt_last_time[2] == 0)) {
    stop("Number of goals and last goal minute(s) not in agreement!")
  } else if (Nt_last_time[1] > t | Nt_last_time[2] > t) {
    stop("Last goal minute(s) later than current time!")
  }
  
  if (renewal_now) { # if we're at a point in time where an event happens, then we can utilize the Weibull Count Model
    
    lamb1 <- (1/theta[1])^(theta[2])
    lamb2 <- (1/theta[3])^(theta[4])
    
    Y <- matrix(0, 20, 20)
    
    for (i in (Nt[1]+1):20){
      for (j in (Nt[2]+1):20){
        Y[j,i] <- Countr::dWeibullCount(i-1 - Nt[1], shape = theta[2], scale = lamb1, time = Ti - t) * Countr::dWeibullCount(j-1 - Nt[2], shape = theta[4], scale = lamb2, time = Ti - t)
      }
    }
    
    Home <- sum(Y[upper.tri(Y)])
    Away <- sum(Y[lower.tri(Y)])
    Draw <- sum(diag(Y))
    
    OU0 <- OU(Y, OU_num = 0)
    OU1 <- OU(Y, OU_num = 1)
    OU2 <- OU(Y, OU_num = 2)
    OU3 <- OU(Y, OU_num = 3)
    OU4 <- OU(Y, OU_num = 4)
    OU5 <- OU(Y, OU_num = 5)
    OU6 <- OU(Y, OU_num = 6)
    OU7 <- OU(Y, OU_num = 7)
    OU8 <- OU(Y, OU_num = 8)
    
    return(list(
      "Home" = Home,
      "Away" = Away,
      "Draw" = Draw,
      "O85"  = OU8$Over,
      "U85"  = OU8$Under,
      "O75"  = OU7$Over,
      "U75"  = OU7$Under,
      "O65"  = OU6$Over,
      "U65"  = OU6$Under,
      "O55"  = OU5$Over,
      "U55"  = OU5$Under,
      "O45"  = OU4$Over,
      "U45"  = OU4$Under,
      "O35"  = OU3$Over,
      "U35"  = OU3$Under,
      "O25"  = OU2$Over,
      "U25"  = OU2$Under,
      "O15"  = OU1$Over,
      "U15"  = OU1$Under,
      "O05"  = OU0$Over,
      "U05"  = OU0$Under,
      "CS00" = Y[1,1],
      "CS10" = Y[1,2], 
      "CS20" = Y[1,3], 
      "CS30" = Y[1,4], 
      "CS01" = Y[2,1], 
      "CS02" = Y[3,1], 
      "CS03" = Y[4,1], 
      "CS11" = Y[2,2], 
      "CS12" = Y[3,2], 
      "CS13" = Y[4,2], 
      "CS21" = Y[2,3],
      "CS22" = Y[3,3],
      "CS23" = Y[4,3], 
      "CS31" = Y[2,4], 
      "CS32" = Y[3,4],
      "CS33" = Y[4,4]
    ))
    
    
  } else {
    
    if (m %% 5 == 0){
      par <- solve(200000, m)
      
      numCores <- cores_in_par
      doParallel::registerDoParallel(numCores)
      
      sim <- foreach::foreach (i=1:par) %dopar% {
        
        # Simulate two independent Weibull Renewal Processes up to time Ti
        weibsim1 <- apply(matrix(rweibull(15*200000, shape = theta[2], scale = theta[1]), 200000, 15),1, cumsum)
        weibsim1[weibsim1 > (Ti - Nt_last_time[1])] <- NA
        weibsim1 <- weibsim1 + Nt_last_time[1]
        
        weibsim2 <- apply(matrix(rweibull(15*200000, shape = theta[4], scale = theta[3]), 200000, 15),1, cumsum)
        weibsim2[weibsim2 > (Ti - Nt_last_time[2])] <- NA
        weibsim2 <- weibsim2 + Nt_last_time[2]
        
        if (t == 0) {
          
          # If no time has passed then return vectors of goals
          ngoals1 <- colSums(weibsim1 > 0, na.rm = TRUE)
          ngoals2 <- colSums(weibsim2 > 0, na.rm = TRUE)
          
          list("ngoals1" = ngoals1, "ngoals2" = ngoals2)
          
        } else {
          
          # If time has passed then return vectors of goals in matches where the score is given by Nt
          w1 <- weibsim1[,-union(which(weibsim1[1,] < t), which(weibsim2[1,] < t)), drop = FALSE]
          w2 <- weibsim2[,-union(which(weibsim1[1,] < t), which(weibsim2[1,] < t)), drop = FALSE]
          
          ngoals1 <- colSums(w1 > 0, na.rm = TRUE) + Nt[1]
          ngoals2 <- colSums(w2 > 0, na.rm = TRUE) + Nt[2]
          
          list("ngoals1" = ngoals1, "ngoals2" = ngoals2)
          
        }

      }
      
      doParallel::stopImplicitCluster()
      
      ngoals1 <- c()
      ngoals2 <- c()
      
      for (i in 1:length(sim)){
        ngoals1 <- c(ngoals1, sim[[i]][[1]])
        ngoals2 <- c(ngoals2, sim[[i]][[2]])
      }
      
    } else if (m %% 2 == 0){
      par <- solve(500000, m)
      
      numCores <- cores_in_par
      doParallel::registerDoParallel(numCores)
      
      sim <- foreach::foreach (i=1:par) %dopar% {
        
        # Simulate two independent Weibull Renewal Processes up to time Ti
        weibsim1 <- apply(matrix(rweibull(15*500000, shape = theta[2], scale = theta[1]), 500000, 15),1, cumsum)
        weibsim1[weibsim1 > (Ti - Nt_last_time[1])] <- NA
        weibsim1 <- weibsim1 + Nt_last_time[1]
        
        weibsim2 <- apply(matrix(rweibull(15*500000, shape = theta[4], scale = theta[3]), 500000, 15),1, cumsum)
        weibsim2[weibsim2 > (Ti - Nt_last_time[2])] <- NA
        weibsim2 <- weibsim2 + Nt_last_time[2]
        
        if (t == 0) {
          
          # If no time has passed then return vectors of goals
          ngoals1 <- colSums(weibsim1 > 0, na.rm = TRUE)
          ngoals2 <- colSums(weibsim2 > 0, na.rm = TRUE)
          
          list("ngoals1" = ngoals1, "ngoals2" = ngoals2)
          
        } else {
          
          # If time has passed then return vectors of goals in matches where the score is given by Nt
          w1 <- weibsim1[,-union(which(weibsim1[1,] < t), which(weibsim2[1,] < t)), drop = FALSE]
          w2 <- weibsim2[,-union(which(weibsim1[1,] < t), which(weibsim2[1,] < t)), drop = FALSE]
          
          ngoals1 <- colSums(w1 > 0, na.rm = TRUE) + Nt[1]
          ngoals2 <- colSums(w2 > 0, na.rm = TRUE) + Nt[2]
          
          list("ngoals1" = ngoals1, "ngoals2" = ngoals2)
          
        }
        
      }
      
      doParallel::stopImplicitCluster()
      
      ngoals1 <- c()
      ngoals2 <- c()
      
      for (i in 1:length(sim)){
        ngoals1 <- c(ngoals1, sim[[i]][[1]])
        ngoals2 <- c(ngoals2, sim[[i]][[2]])
      }
      
    } else if (m < 1000000){
      
      # Simulate two independent Weibull Renewal Processes up to time Ti
      weibsim1 <- apply(matrix(rweibull(15*m, shape = theta[2], scale = theta[1]), m, 15),1, cumsum)
      weibsim1[weibsim1 > (Ti - Nt_last_time[1])] <- NA
      weibsim1 <- weibsim1 + Nt_last_time[1]

      weibsim2 <- apply(matrix(rweibull(15*m, shape = theta[4], scale = theta[3]), m, 15),1, cumsum)
      weibsim2[weibsim2 > (Ti - Nt_last_time[2])] <- NA
      weibsim2 <- weibsim2 + Nt_last_time[2]

      if (t == 0) {

        # If no time has passed then return vectors of goals
        ngoals1 <- colSums(weibsim1 > 0, na.rm = TRUE)
        ngoals2 <- colSums(weibsim2 > 0, na.rm = TRUE)

      } else {

        # If time has passed then return vectors of goals in matches where the score is given by Nt
        w1 <- weibsim1[,-union(which(weibsim1[1,] < t), which(weibsim2[1,] < t)), drop = FALSE]
        w2 <- weibsim2[,-union(which(weibsim1[1,] < t), which(weibsim2[1,] < t)), drop = FALSE]

        ngoals1 <- colSums(w1 > 0, na.rm = TRUE) + Nt[1]
        ngoals2 <- colSums(w2 > 0, na.rm = TRUE) + Nt[2]
        #
        # mean(ngoals1); mean(ngoals2)

      }
      
    } else {
      stop("Number of Monte Carlo simulations to high and not able to be equally parallized!")
    }
    
    Home <- mean(ngoals1 > ngoals2)
    Away <- mean(ngoals1 < ngoals2)
    Draw <- 1 - Home - Away # mean(ngoals1 == ngoals2)
    O85  <- mean(ngoals1 + ngoals2 > 8.5)
    U85  <- 1 - O85 # mean(ngoals1 + ngoals2 < 8.5)
    O75  <- mean(ngoals1 + ngoals2 > 7.5)
    U75  <- 1 - O75 # mean(ngoals1 + ngoals2 < 7.5)
    O65  <- mean(ngoals1 + ngoals2 > 6.5)
    U65  <- 1 - O65 # mean(ngoals1 + ngoals2 < 6.5)
    O55  <- mean(ngoals1 + ngoals2 > 5.5)
    U55  <- 1 - O55 # mean(ngoals1 + ngoals2 < 5.5)
    O45  <- mean(ngoals1 + ngoals2 > 4.5)
    U45  <- 1 - O45 # mean(ngoals1 + ngoals2 < 4.5)
    O35  <- mean(ngoals1 + ngoals2 > 3.5)
    U35  <- 1 - O35 # mean(ngoals1 + ngoals2 < 3.5)
    O25  <- mean(ngoals1 + ngoals2 > 2.5)
    U25  <- 1 - O25 # mean(ngoals1 + ngoals2 < 2.5)
    O15  <- mean(ngoals1 + ngoals2 > 1.5)
    U15  <- 1 - O15 # mean(ngoals1 + ngoals2 < 1.5)
    O05  <- mean(ngoals1 + ngoals2 > 0.5)
    U05  <- 1 - O05 # mean(ngoals1 + ngoals2 < 0.5)
    
    return(list(
      "Home" = Home,
      "Away" = Away,
      "Draw" = Draw,
      "O85"  = O85,
      "U85"  = U85,
      "O75"  = O75,
      "U75"  = U75,
      "O65"  = O65,
      "U65"  = U65,
      "O55"  = O55,
      "U55"  = U55,
      "O45"  = O45,
      "U45"  = U45,
      "O35"  = O35,
      "U35"  = U35,
      "O25"  = O25,
      "U25"  = U25,
      "O15"  = O15,
      "U15"  = U15,
      "O05"  = O05,
      "U05"  = U05,
      "CS00" = sum(ngoals1 == 0 & ngoals2 == 0)/length(ngoals1),
      "CS10" = sum(ngoals1 == 1 & ngoals2 == 0)/length(ngoals1), 
      "CS20" = sum(ngoals1 == 2 & ngoals2 == 0)/length(ngoals1), 
      "CS30" = sum(ngoals1 == 3 & ngoals2 == 0)/length(ngoals1), 
      "CS01" = sum(ngoals1 == 0 & ngoals2 == 1)/length(ngoals1), 
      "CS02" = sum(ngoals1 == 0 & ngoals2 == 2)/length(ngoals1), 
      "CS03" = sum(ngoals1 == 0 & ngoals2 == 3)/length(ngoals1),
      "CS11" = sum(ngoals1 == 1 & ngoals2 == 1)/length(ngoals1), 
      "CS12" = sum(ngoals1 == 1 & ngoals2 == 2)/length(ngoals1), 
      "CS13" = sum(ngoals1 == 1 & ngoals2 == 3)/length(ngoals1), 
      "CS21" = sum(ngoals1 == 2 & ngoals2 == 1)/length(ngoals1),
      "CS22" = sum(ngoals1 == 2 & ngoals2 == 2)/length(ngoals1),
      "CS23" = sum(ngoals1 == 2 & ngoals2 == 3)/length(ngoals1), 
      "CS31" = sum(ngoals1 == 3 & ngoals2 == 1)/length(ngoals1), 
      "CS32" = sum(ngoals1 == 3 & ngoals2 == 2)/length(ngoals1),
      "CS33" = sum(ngoals1 == 3 & ngoals2 == 3)/length(ngoals1)
    ))
    
  }
}


# l1 <- WeibRenewal_Pricing(theta = theta, t = t, Nt = Nt, Nt_last_time = Nt_last_time)


# tictoc::tic()
# l2 <- WeibRenewal_Pricing(theta = c(1/1.5, 1, 1/1.5, 1), t = 0.25, Nt = c(0,0), Nt_last_time = c(0,0), m = 10000000)
# tictoc::toc()
#
# tictoc::tic()
# l3 <- WeibRenewal_Pricing(theta = c(1/1.5, 1.1, 1/1.5, 1.1), t = 0.75, Nt = c(1,0), Nt_last_time = c(0.05,0), m = 10000000)
# tictoc::toc()
# 
# tictoc::tic()
# l4 <- WeibRenewal_Pricing(theta = c(1/1.5, 1.1, 1/1.5, 1.1), t = 0.75, Nt = c(1,0), Nt_last_time = c(0.3,0), m = 10000000)
# tictoc::toc()
# 
# tictoc::tic()
# l5 <- WeibRenewal_Pricing(theta = c(1/1.5, 1.1, 1/1.5, 1.1), t = 0.75, Nt = c(1,0), Nt_last_time = c(0.7,0), m = 10000000)
# tictoc::toc()

