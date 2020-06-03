n_calib_checker <- function(df) {
  
  n <- rep(0,nrow(df))
  
  for (i in 1:length(n)) {
    
    M <- df[i,]
    
    nam <- names(M)[5:7]
    
    if ((nam %in% c("Home", "Away", "Draw") %>% sum()) != 3) {
      print("Error in match odds names!")
    }
    
    # Implied probabilites ----
    
    if (any(is.na(M))) {
      ind <- which(is.na(M))
      M <- M[-ind]
    }
    
    if(ncol(M) > 4) {
      
      Back <- M[c(rep(TRUE, 4), (names(M) %>% stringr::str_detect("_BestBack"))[-c(1:4)])]
      Lay <- M[c(rep(TRUE, 4), (names(M) %>% stringr::str_detect("_BestLay"))[-c(1:4)])]
      M <- M[names(M) %>% stringr::str_detect("_Best", negate = TRUE)]
      
      Back_ind <- c(names(M)[1:4], paste0(names(M)[5:ncol(M)], "_BestBack"))
      Lay_ind <- c(names(M)[1:4], paste0(names(M)[5:ncol(M)], "_BestLay"))
      
      Back <- Back[Back_ind]
      names(Back) <- names(Back) %>% stringr::str_remove_all("_BestBack")
      Lay <- Lay[Lay_ind]
      names(Lay) <- names(Lay) %>% stringr::str_remove_all("_BestLay")
      
      if(ncol(M) == ncol(Back) & ncol(M) == ncol(Lay)){
        n[i] <- ncol(M) - 4
      } else {
        n[i] <- NA
      }
    } else {
      n[i] <- 0
    }
  }
  
  return(n)
  
}
