setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(tidyverse, rugarch, tictoc,rmgarch,parallel)
load("./DATA/Workspace2006.RData")



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


ucast <- multiforecast(multifitORspec = multf, data = Data, n.ahead = 1, n.roll = os-1,
                       out.sample = os,cluster = cl)

g_matrix <- matrix(0,ncol = 3,nrow = os)
for(j in 1:3){
  forc <- ucast@forecast[[j]]
  g_matrix[,j] <- (forc@forecast$sigmaFor)^2
}


H_g = list()
for(j in 1:os){
  H_g[[j]] <- diag(g_matrix[j,])
}

save(H_g,file = "./Forecasts711/uGARCH_forecasts711.Rdata")

save(H_dcc,file = "./Forecasts711/DCC_forecasts711.Rdata")
stopCluster(cl)





