# Housekeeping ----------------------------------------------------------------

rm(list = ls())  # Clear all
graphics.off()   # Close all
cat("\014")      # Clears console

source("Libraries.R")
source("Pricing/WeibProcess_Pricing.R")
source("Pricing/PoissonProcess_Pricing.R")
source("Pricing/WeibRenewal_Pricing.R")
source("Pricing/Cop_WeibProcess_Pricing.R")
source("Calibration/Functions/R_Calibration_BackLay.R")
source("Calibration/Functions/clean_calibration_data.R")


# Data and final cleaning -------------------------------------------------
load("Odds_df.Rdata")
names(Odds_df) <- names(Odds_df) %>% str_replace_all("The Draw", replacement = "Draw")

Odds_df <- Odds_df[,-c(16:19, 22:25)]
Odds_df[62,5:ncol(Odds_df)] <- NA
Odds_df[3,22] <- NA
Odds_df[86,c(22,24)] <- NA
Odds_df[127,c(16,17,23,24,27)] <- NA


# Poisson Process Calibration ---------------------------------------------

Odds_df_list <- split(Odds_df, seq(nrow(Odds_df)))

tim <- seq(0,1,length.out = nrow(Odds_df) + 1)

numCores <- detectCores()

registerDoParallel(numCores)

tic()
Bourn_South_PoissonProcess_opt <- foreach (i=1:nrow(Odds_df)) %dopar% {
  theta <- c(1.5, 1.5)
  M <- Odds_df_list[[i]]
  Nt <- c(dplyr::pull(M[1,3]), dplyr::pull(M[1,4]))
  t <- tim[i]
  if (!all(is.na(M[1,5:9]))){
    optim(par = theta, fn = R_calibration_BackLay, M = M, Nt = Nt, t = t, type = "PoissonProcess")
  } else {
    NA
  }
}
toc()

stopImplicitCluster()

save(PoissonProcess_opt, file = "Calibration/CalibratedData/PoissonProcess_opt.Rdata")


# Weibull Process ---------------------------------------------------------

Odds_df_list <- split(Odds_df, seq(nrow(Odds_df)))

tim <- seq(0,1,length.out = nrow(Odds_df) + 1)

numCores <- detectCores()
numCores

registerDoParallel(numCores)

tic()
Bourn_South_WeibProcess_opt <- foreach (i=1:nrow(Odds_df)) %dopar% {
  theta <- c(1.5, 1, 1.5, 1)
  M <- Odds_df_list[[i]]
  Nt <- c(dplyr::pull(M[1,3]), dplyr::pull(M[1,4]))
  t <- tim[i]
  if (!all(is.na(M[1,5:ncol(M)]))){
    optim(par = theta, fn = R_calibration_BackLay, M = M, Nt = Nt, t = t, type = "WeibProcess")
  } else {
    NA
  }
}
toc()

stopImplicitCluster()

save(WeibProcess_opt, file = "Calibration/CalibratedData/WeibProcess_opt.Rdata")


# Copula Weibull Process Calibration --------------------------------------

Odds_df_list <- split(Odds_df, seq(nrow(Odds_df)))

tim <- seq(0,1,length.out = nrow(Odds_df) + 1)

numCores <- detectCores()
numCores

registerDoParallel(numCores)

tic()
CopulaWeibProcess_opt <- foreach (i=1:nrow(Odds_df)) %dopar% {
  theta <- c(1.5, 1, 1.5, 1, 0)
  M <- Odds_df_list[[i]]
  Nt <- c(dplyr::pull(M[1,3]), dplyr::pull(M[1,4]))
  t <- tim[i]
  if (!all(is.na(M[1,5:ncol(M)]))){
    optim(par = theta, fn = R_calibration_BackLay, M = M, Nt = Nt, t = t, type = "CopulaWeibProcess")
  } else {
    NA
  }
}
toc()

stopImplicitCluster()

save(CopulaWeibProcess_opt, file = "Calibration/CalibratedData/CopulaWeibProcess_opt.Rdata")


# Weibull Renewal Calibration ---------------------------------------------

# THIS TAKES A LOOOOONG TIME TO RUN! Don'T DO IT!

Odds_df_list <- split(Odds_df, seq(nrow(Odds_df)))

tim <- seq(0,1,length.out = nrow(Odds_df) + 1)

{ind1 <- which(c(0,diff(Odds_df$HomeScore)) > 0)
  Nt_last1 <- rep(NA,nrow(Odds_df))
  Nt_last1[1] <- 0
  for(i in ind1){
    Nt_last1[i] <- tim[i]
  }
  Nt_last1 <- na.locf(Nt_last1)}

{ind2 <- which(c(0,diff(Odds_df$AwayScore)) > 0)
  Nt_last2 <- rep(NA,nrow(Odds_df))
  Nt_last2[1] <- 0
  for(i in ind2){
    Nt_last2[i] <- tim[i]
  }
  Nt_last2 <- na.locf(Nt_last2)}

numCores <- 4

registerDoParallel(numCores)

Sys.time()
tic()
WeibRenewal_opt <- foreach (i=1:192) %dopar% {

  library(foreach)
  M <- Odds_df_list[[i]]
  Nt <- c(dplyr::pull(M[1,3]), dplyr::pull(M[1,4]))
  Nt_last_time <- c(Nt_last1[i], Nt_last2[i])
  t <- tim[i]

  if (!all(is.na(M[1,5:ncol(M)]))){
    opt_weibcount <- optim(par = c(0.85, 1.3, 0.7, 1.3), fn = R_calibration_BackLay, M = M, Nt = Nt, Nt_last_time = Nt_last_time, t = t, type = "WeibRenewal", renewal_now = TRUE)
    theta <- opt_weibcount$par
  }

  if (!all(is.na(M[1,5:ncol(M)]))){
    if (t == 0){
      c(opt_weibcount, list(par_start = c(0.9, 1.2, 0.7, 0.95)))
    } else {
      opt <- optim(par = theta, fn = R_calibration_BackLay, M = M, Nt = Nt, Nt_last_time = Nt_last_time, t = t, type = "WeibRenewal", renewal_now = FALSE)
        c(opt, list(par_start = theta))
      }
  } else {
    NA
  }
}
toc()

stopImplicitCluster()

save(WeibRenewal_opt, file = "Calibration/CalibratedData/WeibRenewal_opt.Rdata")

Sys.time()




