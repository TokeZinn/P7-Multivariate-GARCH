setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(tidyverse, rugarch, tictoc,rmgarch,parallel)
source("./DATA/DataAndReturnFct.R")


DF = Return_DF[,5:7] %>% as.data.frame() %>% as.matrix(); is <- length(DF[,1])
OS = Return_DF_OOS[,5:7] %>% as.data.frame() %>% as.matrix() ; os <- length(OS[,1])
end = length(DF[,1]); end2 = length(OS[,1])
Data <- rbind(DF,OS)


xspec <- ugarchspec(variance.model = list( model = "sGARCH", garchOrder = c(1,1)),
                    mean.model = list( armaOrder = c(0,0) , include.mean = F) )
uspec <- multispec(replicate(3,xspec))
Spec <- dccspec(uspec = uspec,dccOrder = c(1, 1), distribution = 'mvnorm')

cl = makePSOCKcluster(3)
multf = multifit(uspec, Data, cluster = cl,out.sample = os,solver = "hybrid")

Fit <- dccfit(Spec, data = Data, fit.control = list(eval.se = TRUE),
              fit = multf, cluster = cl,out.sample = os,solver = "solnp")



Forecast <- dccforecast(Fit, n.roll = os-1,cluster = cl)
H_dcc <- Forecast@mforecast$H
for(i in 1:os){
  H_dcc[[i]] <- H_dcc[[i]] %>%  as.data.frame() %>% as.matrix()
}
stopCluster(cl)



#save(H_dcc,file = "./Forecasts/DCC_forecasts.Rdata")


cl = makePSOCKcluster(3)
sim <- dccsim(fitORspec = Fit,n.sim = is+os)
stopCluster(cl)








