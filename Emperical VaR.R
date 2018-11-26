setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(tidyverse)

source("./DATA/DataAndReturnFct.R")


Return_DF %>% dplyr::select(Returns_SP500, Returns_Gold, Returns_Oil) %>% as.matrix() -> X_IS
Return_DF_OOS %>% dplyr::select(Returns_SP500_OOS, Returns_Gold_OOS, Returns_Oil_OOS) %>% as.matrix() -> X_OS

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

w = Min_Variance_Port(X_IS) %>% as.numeric()

#Computeting portfolio returns of minimum variance portfolio.

PortReturnsIS =  X_IS %*% w

PortReturnsOS = X_OS %*% w

#Computing empirical non parametric VaR

NonParaVaR = function(series, alpha = 0.05){
  n = length(series)
  
  K = round(n*alpha)
  
  series = sort(series , decreasing = F)
  
  VaR = series[K]
  
  return(VaR)
}

NonParaVaR(PortReturnsOS)

NonParaEmpericalVarChecker = function(IS, OS , alpha = 0.05){
  
  m = length(IS)
  n = length(OS)
  
  AllData = c(IS, OS)
  
  VaR = rep(0, n)
  ExceedCount = rep(0, n)
  
  for (i in 1:n) {
    
    CurrentData = AllData[i:(n+i-1)]
    
    VaR[i] = NonParaVaR(CurrentData , alpha = alpha)
    
    if(OS[i] < VaR[i]){ExceedCount[i] = 1}
    
  }
  
  Exceedance = mean(ExceedCount)
  
  return(list(VaR = VaR, 
              Exceedance = Exceedance))
  
}


#Compute empirical VaR using a list

load("./Forecasts/BEKK_forecasts.Rdata"); H_BEKK = mod; remove(mod)
load("./Forecasts/DCC_forecasts.Rdata")
load("./Forecasts/uGARCH_forecasts.Rdata")

MatrixVar = function(Matrix, w = w, alpha = alpha){
  
  VaR = qnorm(alpha)*sqrt(t(w) %*% Matrix %*% w)
  
  return(VaR)
  
}

ListEmpericalVarChecker = function(w = w, OS , List , alpha = 0.05){
  
  n = length(OS)
  
  VaR = rep(0, n)
  ExceedCount = rep(0, n)
  
  for (i in 1:n) {
    
    VaR[i] = MatrixVar(List[[i]] , w = w , alpha = alpha)
    
    if(OS[i] < VaR[i]){ExceedCount[i] = 1}
    
  }
  
  Exceedance = mean(ExceedCount)
  
  return(list(VaR = VaR, 
              Exceedance = Exceedance))
}


#Comparing emperical VaR's
alpha_test = 0.01

NonPara_EV = NonParaEmpericalVarChecker(PortReturnsIS %>% tail(n = 500), PortReturnsOS , alpha = alpha_test);NonPara_EV[[2]]
uGARCH_EV = ListEmpericalVarChecker(w = w, OS = PortReturnsOS , List = H_g, alpha = alpha_test);uGARCH_EV[[2]]
BEKK_EV = ListEmpericalVarChecker(w = w, OS = PortReturnsOS , List = H_BEKK , alpha = alpha_test);BEKK_EV[[2]]
DCC_EV = ListEmpericalVarChecker(w = w, OS = PortReturnsOS , List = H_dcc, alpha = alpha_test);DCC_EV[[2]]


plot(PortReturnsOS , type = "l" , ylim = c(-4,1.5))
lines(uGARCH_EV$VaR , col = "red")
lines(BEKK_EV$VaR , col = "blue")
lines(DCC_EV$VaR , col = "green")
lines(NonPara_EV$VaR , col = "yellow")

