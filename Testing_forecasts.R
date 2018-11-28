setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(tidyverse,emdbook,readxl)
load("Data_and_returns.Rdata")
source("MVWLR.R")
#load("./Forecasts/BEKK_forecasts.Rdata") ; H_bekk <- mod; remove(mod)
load("./Forecasts/BEKK_forecasts_BFGS.Rdata") ; H_bekk <- mod; remove(mod)
load("./Forecasts/DCC_forecasts.Rdata")
load("./Forecasts/Benchmark_forecasts.Rdata") ; load("./Forecasts/uGARCH_forecasts.Rdata")


DF = Return_DF[,5:7] %>% as.data.frame() %>% as.matrix()
OS = Return_DF_OOS[,5:7] %>% as.data.frame() %>% as.matrix()



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

#bench = Benchmark(DF,OS)
#save(bench,file = "./Forecasts/Benchmark_forecasts.Rdata")


WLR.test(OS,H1 = H_bekk,H2 = H_dcc)
WLR.test(OS,H1 = H_bekk,H2 = bench)
WLR.test(OS,H1 = H_bekk,H2 = H_g)
WLR.test(OS,H1 = H_dcc,H2 = H_g)
WLR.test(OS,H1 = H_dcc,H2 = bench)



#WLR.test(OS,density1 = D2,density2 = D3)
#WLR.test(OS,density1 = D1,density2 = D3)
#WLR.test(OS,density1 = D1,density2 = D2)


