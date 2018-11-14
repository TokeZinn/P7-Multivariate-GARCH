source("./DATA/DataAndReturnFct.R")

Return_DF %>% dplyr::select(Returns_SP500, Returns_Gold, Returns_Oil) %>% as.matrix() -> X

Min_Variance_Port = function(X, Sigma = NULL){
  #browser()
  mean_matrix = function(X){
    m = c()
    for (i in 1:ncol(X)){
      m = c(m, mean(X[,i]))
    }
    return(m)
  }
  demean_matrix = function(X){
    for(i in 1:ncol(X)){
      X[,i] = X[,i]-mean(X[,i])
    }
    return(X)
  }
  
  m = mean_matrix(X)
  X = demean_matrix(X)
  
  if(is.null(Sigma)){
    Sigma = (1/(nrow(X)-1))* (t(X) %*% X)
  }
  
  
  S_inv = solve(Sigma)
  e = rep(1,3)
  
  
  w = S_inv %*% e / drop(t(e) %*% S_inv %*% e)
  
  return(w)
  
}



Optimal_Portfolio_Desired = function(X, mu_b, Sigma = NULL){
  #browser()
  mean_matrix = function(X){
    m = c()
    for (i in 1:ncol(X)){
      m = c(m, mean(X[,i]))
    }
    return(m)
  }
  demean_matrix = function(X){
    for(i in 1:ncol(X)){
      X[,i] = X[,i]-mean(X[,i])
    }
    return(X)
  }
  
  m = mean_matrix(X)
  X = demean_matrix(X)
  
  if(is.null(Sigma)){
    Sigma = (1/(nrow(X)-1))* (t(X) %*% X)
  }
  
  
  S_inv = solve(Sigma)
  e = rep(1,3)
  
  a = t(m) %*% S_inv %*% m
  b = t(m) %*% S_inv %*% e
  c = t(e) %*% S_inv %*% m
  d = t(e) %*% S_inv %*% e
  
  l1 = (t((m - 0.1*e))%*% S_inv %*% m)/(a*d - b^2)
  l2 = (-t((m - 0.1*e))%*% S_inv %*% e)/(a*d - b^2)
  
  w = S_inv %*% (drop(l1)*e + drop(l2)*m)
  
  return(w)
  
}
Optimal_Portfolio_Find = function(X, lambda, Sigma = NULL){
  #browser()
  mean_matrix = function(X){
    m = c()
    for (i in 1:ncol(X)){
      m = c(m, mean(X[,i]))
    }
    return(m)
  }
  demean_matrix = function(X){
    for(i in 1:ncol(X)){
      X[,i] = X[,i]-mean(X[,i])
    }
    return(X)
  }
  
  m = mean_matrix(X)
  X = demean_matrix(X)
  
  if(is.null(Sigma)){
    Sigma = (1/(nrow(X)-1))* (t(X) %*% X)
  }
  
  
  S_inv = solve(Sigma)
  e = rep(1,3)
  
  #Minimum Variance Portfolio
  w_min =  (1/drop(t(e) %*% S_inv %*% e))*S_inv %*% e 
  mu_min = m %*% w_min
  
  w_m = (1/drop(t(e) %*% S_inv %*% m))*S_inv %*% m
  mu_m = m %*% w_m
  
  alpha = drop(lambda*(t(m) %*% S_inv %*% e))
  
  w = (1-alpha)*w_min + alpha*w_m
  return(w)
}
Efficient_Frontier = function(X,x, Sigma = NULL, short = T){
  
  mean_matrix = function(X){
    m = c()
    for (i in 1:ncol(X)){
      m = c(m, mean(X[,i]))
    }
    return(m)
  }
  
  demean_matrix = function(X){
    for(i in 1:ncol(X)){
      X[,i] = X[,i]-mean(X[,i])
    }
    return(X)
  }
  
  m = mean_matrix(X)
  
  X = demean_matrix(X)
  
  if(is.null(Sigma)){
    Sigma = (1/(nrow(X)-1))* (t(X) %*% X)
  }
  
  S_inv = solve(Sigma)
  e = rep(1, ncol(X))
  
  alpha = drop(t(e) %*% S_inv %*% e)
  beta = drop(t(e) %*% S_inv %*% m)
  gamma = drop(t(m) %*% S_inv %*% m)
  delta = alpha*gamma - beta^2
  
  
  upper = beta / alpha + sqrt((beta / alpha) ^ 2 - (gamma - delta * x ^ 2) / (alpha))
  lower = beta / alpha - sqrt((beta / alpha) ^ 2 - (gamma - delta * x ^ 2) / (alpha))
  
  return(list(upper,lower))
}
