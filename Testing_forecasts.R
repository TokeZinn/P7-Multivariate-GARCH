setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(tidyverse,emdbook,readxl)

source("MVWLR.R")
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

load("./DATA/Workspace2006.Rdata")
load("./Forecasts_2006/Benchmark_forecasts_2006.Rdata")
load("./Forecasts_2006/DCC_forecasts_2006.Rdata")
load("./Forecasts_2006/uGARCH_forecasts_2006.Rdata")
# bench_2006 = Benchmark(IS,OS)
# save(bench_2006,file = "./Forecasts_2006/Benchmark_forecasts_2006.Rdata")





WLR.test(OS,H1 = H_dcc,H2 = bench_2006,Plot = T,Dates = Dates_OS)
WLR.test(OS,H1 = H_dcc,H2 = H_g,Plot = T,Dates = Dates_OS)
WLR.test(OS,H1 = H_g,bench_2006,Plot = T,Dates = Dates_OS)



load("./DATA/Workspace2012.Rdata")
# bench_2012 = Benchmark(IS,OS)
# save(bench_2012,file = "./Forecasts_2012/Benchmark_forecasts_2012.Rdata")
load("./Forecasts_2012/Benchmark_forecasts_2012.Rdata")
load("./Forecasts_2012/DCC_forecasts_2012.Rdata")
load("./Forecasts_2012/uGARCH_forecasts_2012.Rdata")

WLR.test(OS,H1 = H_dcc,H2 = bench_2012,Plot = T,Dates = Dates_OS)
WLR.test(OS,H1 = H_dcc,H2 = H_g,Plot = T,Dates = Dates_OS)
WLR.test(OS,H1 = H_g,bench_2012,Plot = T,Dates = Dates_OS)







