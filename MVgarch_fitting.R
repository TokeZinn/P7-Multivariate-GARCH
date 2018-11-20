setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(tidyverse,rmgarch,mgarchBEKK,tictoc)


source("Rolling_BEKK.R")
source("./DATA/DataAndReturnFct.R")


DF = Return_DF[,5:7] %>% as.data.frame() %>% as.matrix()
OS = Return_DF_OOS[,5:7] %>% as.data.frame() %>% as.matrix()


end = length(DF[,1]); end2 = length(OS[,1])
tic (); mod = Rolling_BEKK(DF,OS,c(1,1),dim = 3,
                           optim = "Nelder-Mead");toc()



fit = BEKK(rbind(DF,OS))

v = c()
for(i in 1:2703){
  v = c(v,fit$H.estimated[[i]][1,1])
}



sig1 = c()

for (i in mod){
  sig1 = c(sig1, i[1,1])
}

plot(abs(OS[,1]), type = "l", )
lines(sig1,type = "l")








