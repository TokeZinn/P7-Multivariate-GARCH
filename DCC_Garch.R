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

ufit <- list()
for(i in 1:3){
  ufit[[i]] <- ugarchfit(spec = xspec,data = Data[,i],out.sample = os,solver = "hybrid")
}
ufit[[1]]


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


g_matrix <- matrix(0,ncol = 3,nrow = os)
for(j in 1:3){
  roll = ugarchroll(spec = xspec,data = Data[,j],forecast.length = os,
                    refit.every = 1,refit.window = "moving",solver = "hybrid",
                    calculate.VaR = F,window.size = is)
  g_matrix[,j] <- (roll@forecast$density$Sigma)^2
}

H_g = list()
for(j in 1:os){
  H_g[[j]] <- diag(g_matrix[j,])
}
#save(H_dcc,file = "./Forecasts/DCC_forecasts.Rdata")

H_diff <- list()
for(i in 1:length(H_g))(
  H_diff[[i]] <- H_g[[i]] - diag(H_dcc[[i]])
)

H_diff

cl = makePSOCKcluster(3)
sim <- dccsim(fitORspec = Fit,n.sim = is+os)
stopCluster(cl)








