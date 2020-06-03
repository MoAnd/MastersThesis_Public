clean_calibration_data <- function(opt, game) {
  
  n <- length(opt[[1]]$par)
  
  R <- rep(NA, length(opt))
  par <- matrix(nrow = length(opt), ncol = n)
  
  for (i in 1:length(opt)) {
    
    if (is.null(names(opt[[i]]))){
      
      for (j in 1:n){
        par[i,j] <- NA
      }
      
      R[i] <- NA
      
    } else if (names(opt[[i]])[1] == "minimum") {
      
      for (j in 1:n) {
        par[i,j] <- opt[[i]]$estimate[j]
      }
      
      R[i] <- opt[[i]]$minimum
      
    } else if (names(opt[[i]])[2] == "value"){
      
      for (j in 1:n){
        par[i,j] <- opt[[i]]$par[j]
      }
      
      R[i] <- opt[[i]]$value
      
    } else {
      
      stop("Something is wrong with the data!")
      
    }
  }
  
  MinGamePlay = game$MinGamePlay[1:length(opt)]
  
  df_par <- tibble::tibble("MinGamePlay" = MinGamePlay, tibble::as_tibble(par), enframe(R)[,2])
  
  names(df_par)[n+2] <- "CalibError"
  for(k in 1:n){
    names(df_par)[k+1] <- paste0("Par", k)
  }
  
  return(df_par)
  
}
