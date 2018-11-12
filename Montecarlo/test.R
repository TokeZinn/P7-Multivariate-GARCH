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


Spec = ugarchspec(variance.model = list( model = "sGARCH", garchOrder = c(1,1)),
                  mean.model = list( armaOrder = c(0,0) , include.mean = F) )
for(j in 1:3){
  g_matrix[,j] <- ugarchroll(spec = Spec,data = rbind(DF[(end-100):end,j],OS[1:10,j]),forecast.length = 10,
                             refit.every = 10,refit.window = "moving",solver = "hybrid",
                             calculate.VaR = F,window.size = is)
}
roll = ugarchroll(spec = Spec,data = rbind(as.matrix(DF[(end-200):end,1]),as.matrix(OS[1:10,1])),
                  forecast.length = 10,
           refit.every = 10,refit.window = "moving",solver = "hybrid",
           calculate.VaR = F,window.size = length(DF[(end-200):end,1]))



