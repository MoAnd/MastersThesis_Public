R_calibration_BackLay <- function (theta, M, Nt, Nt_last_time = c(0,0), t, type = c("WeibProcess", "WeibRenewal", "PoissonProcess"), renewal_now = FALSE) {
  
  library(magrittr)
  
  # Check if name is home/away/draw ----
  
  nam <- names(M)[5:7]
  
  if ((nam %in% c("Home", "Away", "Draw") %>% sum()) != 3) {
    stop("Error in match odds names!")
  }
  
  # Implied probabilites ----
  
  if (any(is.na(M))) {
    ind <- which(is.na(M))
    M <- M[-ind]
  }
  
  Back <- M[c(rep(TRUE, 4), (names(M) %>% stringr::str_detect("_BestBack"))[-c(1:4)])]
  Lay <- M[c(rep(TRUE, 4), (names(M) %>% stringr::str_detect("_BestLay"))[-c(1:4)])]
  M <- M[names(M) %>% stringr::str_detect("_Best", negate = TRUE)]
  
  Back_ind <- c(names(M)[1:4], paste0(names(M)[5:ncol(M)], "_BestBack"))
  Lay_ind <- c(names(M)[1:4], paste0(names(M)[5:ncol(M)], "_BestLay"))
  
  Back <- Back[Back_ind]
  names(Back) <- names(Back) %>% stringr::str_remove_all("_BestBack")
  Lay <- Lay[Lay_ind]
  names(Lay) <- names(Lay) %>% stringr::str_remove_all("_BestLay")
  
  impl <- list()
  spread <- list()
  
  for (j in names(M)) {
    tmp <- M %>% dplyr::select(j)
    tmp2 <- Back %>% dplyr::select(j)
    tmp3 <- Lay %>% dplyr::select(j)
    if(!(names(tmp) %in% c("Time", "HomeScore", "AwayScore", "MinGamePlay"))){
      impl[[paste0(names(tmp),"_impl")]] <- (tmp %>% dplyr::pull())
      spread[[paste0(names(tmp),"_spread")]] <- dplyr::pull(tmp2 - tmp3)
    }
  }
  
  
  # Type ----
  
  if (type == "WeibProcess"){
    X <- WeibProcess_Pricing(theta, t = t, Nt = Nt)
  } else if (type == "PoissonProcess"){
    X <- PoissonProcess_Pricing(theta = theta, t = t, Nt = Nt)
  } else if (type == "WeibRenewal" & renewal_now == TRUE) {
    X <- WeibRenewal_Pricing(theta, t = t, Nt = Nt, Nt_last_time = Nt_last_time, renewal_now = TRUE)
  } else if (type == "WeibRenewal") {
    X <- WeibRenewal_Pricing(theta, m = 5000000, t = t, Nt = Nt, Nt_last_time = Nt_last_time, cores_in_par = 8)
  } else if (type == "CopulaWeibProcess") {
    X <- Cop_WeibProcess_Pricing(theta, t = t, Nt = Nt)
  }
  
  # R ----
  
  ind <- names(impl) %>% stringr::str_replace("_impl","")
  ind <- stringr::str_replace_all(ind, "Under", "U")
  ind <- stringr::str_replace_all(ind, "Over", "O")
  ind <- stringr::str_replace_all(ind, "Goals", "")
  ind <- stringr::str_replace_all(ind, "\\.", "")
  ind <- stringr::str_replace_all(ind, " ", "")
  ind <- stringr::str_replace_all(ind, "-", "")
  suppressWarnings({
    for(i in 1:length(ind)){
      if (!is.na(as.numeric(stringr::str_sub(ind[i], 1, 2)))) {
        ind[i] <- glue::glue("CS", ind[i])
      }
    }
  })
  
  names(impl) <- ind
  
  # ---
  
  ind2 <- names(spread) %>% stringr::str_replace("_spread","")
  ind2 <- stringr::str_replace_all(ind2, "Under", "U")
  ind2 <- stringr::str_replace_all(ind2, "Over", "O")
  ind2 <- stringr::str_replace_all(ind2, "Goals", "")
  ind2 <- stringr::str_replace_all(ind2, "\\.", "")
  ind2 <- stringr::str_replace_all(ind2, " ", "")
  ind2 <- stringr::str_replace_all(ind2, "-", "")
  suppressWarnings({
    for(i in 1:length(ind2)){
      if (!is.na(as.numeric(stringr::str_sub(ind2[i], 1, 2)))) {
        ind2[i] <- glue::glue("CS", ind2[i])
      }
    }
  })
  
  names(spread) <- ind2
  
  # ---
  
  prob_vec <- X[ind] %>% unlist()
  impl_vec <- impl %>% unlist()
  spread_vec <- spread %>% unlist()
  
  if (any(spread_vec == 0)) {
    ind_zero <- which(spread_vec == 0)
    R_indZero <- (impl_vec[ind_zero] - prob_vec[ind_zero])^2
    R <- sqrt( ( sum( ( (impl_vec[-ind_zero] - prob_vec[-ind_zero]) / (spread_vec[-ind_zero]) )^2) + R_indZero ) / length(impl) )
  } else {
    R <- sqrt( ( sum( ( (impl_vec - prob_vec) / (spread_vec) )^2) ) / length(impl) )
  }
  
  return(R)
  
}

# R_calibration_BackLay(theta = c(0.7792473, 1.0000000, 0.4667684, 1.0000000), M = M, Nt = Nt, Nt_last_time = Nt_last_time, t = t, type = "WeibRenewal")

