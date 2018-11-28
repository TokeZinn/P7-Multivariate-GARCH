setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(tidyverse,rmgarch,mgarchBEKK,tictoc)


source("Rolling_BEKK.R")
load("Data_and_returns.RData")


DF = Return_DF[,5:7] %>% as.data.frame() %>% as.matrix(); 
DF2 = Return_DF_OOS[,5:7] %>% as.data.frame() %>% as.matrix() ; 
Data <- rbind(DF,DF2);end = length(Data[,1]); OS <- Data[(1510):end,]; IS <- Data[1:1509,]
os <- length(OS[,1]) ; is <- length(IS[,1])



tic (); H_bekk = Rolling_BEKK(IS,OS,c(1,1),dim = 3,
                           optim = "Nelder-Mead");toc()
save(H_bekk,file = "BEKK_forecasts_1000_nelder.Rdata")



tic (); H_bekk = Rolling_BEKK(IS,OS,c(1,1),dim = 3,
                              optim = "BFGS");toc()
save(H_bekk,file = "BEKK_forecasts_1000_BFGS.Rdata")





