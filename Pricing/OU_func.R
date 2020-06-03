OU <- function(X, OU_num) {
  
  upper.anti.tri <- function(m) col(m) + row(m) < dim(m)[1] + 1
  
  U <- sum(X[1:(OU_num + 2),1:(OU_num + 2)][upper.anti.tri(X[1:(OU_num + 2),1:(OU_num + 2)])])
  O <- sum(X) - sum(X[1:(OU_num + 2),1:(OU_num + 2)][upper.anti.tri(X[1:(OU_num + 2),1:(OU_num + 2)])])
  
  return(list("Under" = U, "Over" = O))
}

# OU(Y,OU_num = 0)

