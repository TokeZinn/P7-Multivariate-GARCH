setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(tidyverse,emdbook,readxl)
load("Data_and_returns.Rdata")
source("MVWLR.R")
# #load("./Forecasts/BEKK_forecasts.Rdata") ; H_bekk <- mod; remove(mod)
# load("./Forecasts/BEKK_forecasts_BFGS.Rdata") ; H_bekk <- mod; remove(mod)
# load("./Forecasts/DCC_forecasts.Rdata")
# load("./Forecasts/Benchmark_forecasts.Rdata") ; load("./Forecasts/uGARCH_forecasts.Rdata")
load("./Forecasts_1000/Benchmark_forecasts_1000.Rdata")
load("./Forecasts_1000/DCC_forecasts_1000.Rdata")
load("./Forecasts_1000/uGARCH_forecasts_1000.Rdata")




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

bench_1000 = Benchmark(IS,OS)
save(bench_1000,file = "Benchmark_forecasts_1000.Rdata")




WLR.test(OS,H1 = H_dcc,H2 = bench_1000,Plot = T,Dates = NULL)


WLR.test(OS,H1 = H_dcc,H2 = H_g)
WLR.test(OS,H1 = H_g,bench_1000)

