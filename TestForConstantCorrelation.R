setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(tidyverse, rugarch, tictoc,rmgarch,parallel)
source("./DATA/DataAndReturnFct.R")

DF = Return_DF[,5:7] %>% as.data.frame() %>% as.matrix(); is <- length(DF[,1])
xspec <- ugarchspec(variance.model = list( model = "sGARCH", garchOrder = c(1,1)),
                    mean.model = list( armaOrder = c(0,0) , include.mean = F) )
uspec <- multispec(replicate(3,xspec))

cl = makePSOCKcluster(3); multf = multifit(uspec, DF, cluster = cl,solver = "hybrid"); stopCluster(cl)

multf@fit[[1]] %>% .@fit$sigma

