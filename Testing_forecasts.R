setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(tidyverse,emdbook,readxl)
source("./DATA/DataAndReturnFct.R")
source("MVWLR.R")
SP500_Roll = read.csv("./Forecasts/SP500_Roll.csv") ; Gold_Roll = read.csv("./Forecasts/Gold_Roll.csv")
Oil_Roll = read.csv("./Forecasts/Oil_Roll.csv")
load("./Forecasts/BEKK_forecasts.Rdata") ; load("./Forecasts/DCC_forecasts.Rdata")
load("./Forecasts/Benchmark_forecasts.Rdata") ; load("./Forecasts/uGARCH_forecasts.Rdata")
H_bekk <- mod; 

DF = Return_DF[,5:7] %>% as.data.frame() %>% as.matrix()
OS = Return_DF_OOS[,5:7] %>% as.data.frame() %>% as.matrix()



f_bekk = function(x){
  b = c()
  for(i in 1:length(x[,1])){
   b = c(b,dmvnorm(x[i,],mu = rep(0,length(x[1,])),Sigma = H_bekk[[i]])) 
  }
  return(b)
}

f_dcc = function(x){
  b = c()
  for(i in 1:length(x[,1])){
    b = c(b,dmvnorm(x[i,],mu = rep(0,length(x[1,])),Sigma = H_dcc[[i]])) 
  }
  return(b)
}

Restricted = list()
for(i in 1:187){
  b = c(SP500_Roll[i,2]^2,Gold_Roll[i,2]^2, Oil_Roll[i,2]^2)
  Restricted[[i]] <- diag(b)
}

D2 = function(x){
  b = c()
  for(i in 1:length(x[,1])){
    b = c(b,dmvnorm(x[i,],mu = rep(0,length(x[1,])),Sigma = Restricted[[i]])) 
  }
  return(b)
}

Benchmark = function(IS , OS ,dim = 3){
  #browser()
  IS = IS %>% as.data.frame() ; OS = OS %>% as.data.frame() ; names(OS) <- names(IS)
  All_Data = rbind(IS,OS) %>% as.matrix()
  
  n = length(IS[,1])
  m = length(OS[,1])
  
  OneSigma = list()
  
  for (i in 1:m) {
    Current_Data = All_Data[i:(n-1+i),]
    
    OneSigma[[i]] = var(Current_Data)
    
  }
  
  return(OneSigma)
  
}

bench = Benchmark(DF,OS)
#save(bench,file = "./Forecasts/Benchmark_forecasts.Rdata")

D3 = function(x){
  b = c()
  for(i in 1:length(x[,1])){
    b = c(b,dmvnorm(x[i,],mu = rep(0,length(x[1,])),Sigma = bench[[i]])) 
  }
  return(b)
}  

f_bek = function(x){
  b = c()
  for(i in 1:length(x[,1])){
    b = c(b,dmvnorm(x[i,],mu = rep(0,length(x[1,])),Sigma = bek[[i]])) 
  }
  return(b)
}


WLR.test(OS,density1 = f_bekk,density2 = D2)
WLR.test(OS,density1 = f_bekk,density2 = D3)
WLR.test(OS,density1 = f_bekk,density2 = f_dcc)
WLR.test(OS,density1 = f_dcc,density2 = D2)
WLR.test(OS,density1 = f_dcc,density2 = D3)
WLR.test(OS,density1 = D2,density2 = D3)




#WLR.test(OS,density1 = D2,density2 = D3)
#WLR.test(OS,density1 = D1,density2 = D3)
#WLR.test(OS,density1 = D1,density2 = D2)


