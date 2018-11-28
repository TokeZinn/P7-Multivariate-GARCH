setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(tidyverse, mgarchBEKK, rugarch, tictoc,rmgarch,parallel)

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
load("./Forecasts/Benchmark_forecasts.Rdata")

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
alpha_test = 0.1

NonPara_EV = NonParaEmpericalVarChecker(PortReturnsIS %>% tail(n = 2517), PortReturnsOS , alpha = alpha_test);NonPara_EV[[2]]
Bench_EV = ListEmpericalVarChecker(w = w, OS = PortReturnsOS, List = bench , alpha = alpha_test);Bench_EV[[2]]
uGARCH_EV = ListEmpericalVarChecker(w = w, OS = PortReturnsOS , List = H_g, alpha = alpha_test);uGARCH_EV[[2]]
BEKK_EV = ListEmpericalVarChecker(w = w, OS = PortReturnsOS , List = H_BEKK , alpha = alpha_test);BEKK_EV[[2]]
DCC_EV = ListEmpericalVarChecker(w = w, OS = PortReturnsOS , List = H_dcc, alpha = alpha_test);DCC_EV[[2]]


plot(PortReturnsOS , type = "l" , ylim = c(-4,1.5))
lines(uGARCH_EV$VaR , col = "red")
lines(BEKK_EV$VaR , col = "blue")
lines(DCC_EV$VaR , col = "green")
lines(NonPara_EV$VaR , col = "yellow")
lines(Bench_EV$VaR , col = "blue")

#Trying with a smaller sample: 

#Making new lists of covariance matrices.
X_IS_short = tail(Return_DF , n = 1000) %>% .[,5:7]

#BEKK
source("./Rolling_BEKK.R")
BEKK500 = Rolling_BEKK(IS = X_IS_short , OS = X_OS , optim = "Nelder-Mead")

#DCC and uGARCH
Data = rbind(X_IS_short %>% as.matrix(), X_OS %>% as.matrix())
os = length(X_OS[,1])

xspec <- ugarchspec(variance.model = list( model = "sGARCH", garchOrder = c(1,1)),
                    mean.model = list( armaOrder = c(0,0) , include.mean = F) )
uspec <- multispec(replicate(3,xspec))
Spec <- dccspec(uspec = uspec,dccOrder = c(1, 1), distribution = 'mvnorm')

cl = makePSOCKcluster(3)
multf = multifit(uspec, Data, cluster = cl,out.sample = os,solver = "hybrid")

Fit <- dccfit(Spec, data = Data, fit.control = list(eval.se = TRUE),
              fit = multf, cluster = cl,out.sample = os,solver = "solnp")


Forecast <- dccforecast(Fit, n.roll = os-1,cluster = cl)
H_dcc500 <- Forecast@mforecast$H
for(i in 1:os){
  H_dcc500[[i]] <- H_dcc500[[i]] %>%  as.data.frame() %>% as.matrix()
}

ucast <- multiforecast(multifitORspec = multf, data = Data, n.ahead = 1, n.roll = os-1,
                       out.sample = os,cluster = cl)

g_matrix <- matrix(0,ncol = 3,nrow = os)
for(j in 1:3){
  forc <- ucast@forecast[[j]]
  g_matrix[,j] <- (forc@forecast$sigmaFor)^2
}


H_g500 = list()
for(j in 1:os){
  H_g500[[j]] <- diag(g_matrix[j,])
}

stopCluster(cl)
alpha_test = 0.1


#Making emperical VaRs
NonPara_EV500 = NonParaEmpericalVarChecker(PortReturnsIS %>% tail(n = 500), PortReturnsOS , alpha = alpha_test);NonPara_EV500[[2]]
uGARCH_EV500 = ListEmpericalVarChecker(w = w, OS = PortReturnsOS , List = H_g500, alpha = alpha_test);uGARCH_EV500[[2]]
BEKK_EV500 = ListEmpericalVarChecker(w = w, OS = PortReturnsOS , List = BEKK500 , alpha = alpha_test);BEKK_EV500[[2]]
DCC_EV500 = ListEmpericalVarChecker(w = w, OS = PortReturnsOS , List = H_dcc500, alpha = alpha_test);DCC_EV500[[2]]


plot(PortReturnsOS , type = "l" , ylim = c(-3,1.5))
lines(uGARCH_EV500$VaR , col = "red")
lines(BEKK_EV500$VaR , col = "blue")
lines(DCC_EV500$VaR , col = "green")
lines(NonPara_EV500$VaR , col = "yellow")
