setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(tidyverse,rmgarch,mgarchBEKK,tictoc)


source("Rolling_BEKK.R")
load("./DATA/Workspace2006.RData")





tic (); H_bekk = Rolling_BEKK(IS,OS,c(1,1),dim = 3,
                           optim = "Nelder-Mead");toc()
save(H_bekk,file = "./Forecasts_2006/BEKK_forecasts_2006_nelder.Rdata")



tic (); H_bekk = Rolling_BEKK(IS,OS,c(1,1),dim = 3,
                              optim = "BFGS");toc()
save(H_bekk,file = "./Forecasts_2006/BEKK_forecasts_2006_BFGS.Rdata")


load("./DATA/Workspace2012.RData")

tic (); H_bekk = Rolling_BEKK(IS,OS,c(1,1),dim = 3,
                              optim = "Nelder-Mead");toc()
save(H_bekk,file = "./Forecasts_2012/BEKK_forecasts_2012_nelder.Rdata")



tic (); H_bekk = Rolling_BEKK(IS,OS,c(1,1),dim = 3,
                              optim = "BFGS");toc()
save(H_bekk,file = "./Forecasts_2012/BEKK_forecasts_2012_BFGS.Rdata")






