setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(tidyverse, mgarchBEKK, rugarch, tictoc,rmgarch,parallel)

#Functions ---------------------------------------------------------------------------------------

#Finding minimum variance portfolio

Min_Variance_Port = function(X, Sigma = NULL){

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

MV_Port_Returns_OS = function(IS, OS){
  
  n = nrow(OS)
  m = nrow(IS)
  
  weights = list()
  
  AllData = rbind(IS, OS)
  
  PortReturnsOS = rep(0, n)
  
  for (i in 1:n) {
    
    CurrentData = AllData[i:(m+i-1),]
    
    w = Min_Variance_Port(CurrentData)
    
    weights[[i]] = w
    
    PortReturnsOS[i] = OS[i,] %*% w
    
  }
  
  return(list(Returns = PortReturnsOS,
              ws = weights))
}

#Computing empirical non parametric VaR

NonParaVaR = function(series, alpha = 0.05){
  n = length(series)
  
  K = round(n*alpha)
  
  series = sort(series , decreasing = F)
  
  VaR = series[K]
  
  return(VaR)
}

#NonParaVaR(PortReturnsOS)

NonParaEmpericalVarChecker = function(IS, OS , alpha = 0.05){
  n = nrow(OS)
  m = nrow(IS)
  
  AllData = rbind(IS, OS)
  
  VaR = rep(0, n)
  ExceedCount = rep(0, n)
  
  for (i in 1:n) {
    
    CurrentData = AllData[i:(m+i-1),]
    
    w = Min_Variance_Port(CurrentData)
    
    CurrentPortfolio = CurrentData %*% w
    
    VaR[i] = NonParaVaR(CurrentPortfolio , alpha = alpha)
    
    if( OS[i,] %*% w < VaR[i]){ExceedCount[i] = 1}
    
  }
  
  Exceedance = mean(ExceedCount)
  
  return(list(VaR = VaR, 
              Exceedance = Exceedance))
  
}

#Compute empirical VaR using a list

MatrixVar = function(Matrix, w = w, alpha = alpha){
  
  VaR = qnorm(alpha)*sqrt(t(w) %*% Matrix %*% w)
  
  return(VaR)
  
}

ListEmpericalVarChecker = function( IS , OS , List , alpha = 0.05){

  n = nrow(OS)
  m = nrow(IS)
  
  AllData = rbind(IS, OS)
  
  VaR = rep(0, n)
  ExceedCount = rep(0, n)
  
  for (i in 1:n) {
    
    CurrentData = AllData[i:(m+i-1),]
    
    w = Min_Variance_Port(CurrentData)
    
    VaR[i] = MatrixVar(List[[i]] , w = w , alpha = alpha)
    
    if(OS[i,] %*% w < VaR[i]){ExceedCount[i] = 1}
    
  }
  
  Exceedance = mean(ExceedCount)
  
  return(list(VaR = VaR, 
              Exceedance = Exceedance))
}











