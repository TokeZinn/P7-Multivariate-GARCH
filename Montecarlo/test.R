setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(cubature,emdbook,MASS,mvtnorm,tictoc,parallel,mgarchBEKK,tidyverse,rugarch,matrixcalc)
source("./DATA/DataAndReturnFct.R")
cl = makePSOCKcluster(10)



DF = Return_DF[,5:7] %>% as.data.frame() %>% as.matrix()
OS = Return_DF_OOS[,5:7] %>% as.data.frame() %>% as.matrix()
end = length(DF[,1]); end2 = length(OS[,1])


Fit = BEKK(DF[(end-200):end,])

para = c(vech(t(Fit$est.params[[1]])))
for(i in 2:length(Fit$est.params)){
  para = c(para,vec(Fit$est.params[[i]]))
}




Fit$est.params[[1]]


set.seed(1)
sim = simulateBEKK(3,100,params = para)


cov(DF[(end-200):end,])







