setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(cubature,emdbook,tictoc,parallel,mgarchBEKK,tidyverse,rugarch,
               matrixcalc,rmgarch,mvtnorm)
source("./DATA/DataAndReturnFct.R")
source("MC_power_DCC.R")



DF = Return_DF[,5:7] %>% as.data.frame() %>% as.matrix()
OS = Return_DF_OOS[,5:7] %>% as.data.frame() %>% as.matrix()
end = length(DF[,1]); end2 = length(OS[,1])
set.seed(1)
tic() ; Result = MC_power_DCC(in.sample = DF[(end-250):end,],
                               out.sample = OS[1:187,],B = 100); toc()



save(Result,file = "Power_DCCvUGARCH.Rdata")



stopCluster(cl)
