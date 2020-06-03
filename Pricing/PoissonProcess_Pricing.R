source("Pricing/OU_func.R")

PoissonProcess_Pricing <- function (theta = c(1.5, 1.5), Ti = 1, t = 0, Nt = c(0,0)) {
  
  Y <- matrix(0, 20, 20)
  
  for (i in (Nt[1]+1):20){
    for (j in (Nt[2]+1):20){
      Y[j,i] <- dpois(i-1 - Nt[1], lambda =  theta[1] * (Ti - t)) * dpois(j-1 - Nt[2], lambda = theta[2] * (Ti - t))
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
  
}

# l <- PoissonProcess_Pricing(theta = c(0.9207174, 2.2125487), t = t, Nt = Nt)
