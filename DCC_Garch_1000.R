setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(tidyverse, rugarch, tictoc,rmgarch,parallel)
load("Data_and_returns.RData")

DF = Return_DF[,5:7] %>% as.data.frame() %>% as.matrix(); 
DF2 = Return_DF_OOS[,5:7] %>% as.data.frame() %>% as.matrix() ; 
Data <- rbind(DF,DF2);end = length(Data[,1]); OS <- Data[(1510):end,]; IS <- Data[1:1509,]
os <- length(OS[,1]) ; is <- length(IS[,1])



xspec <- ugarchspec(variance.model = list( model = "sGARCH", garchOrder = c(1,1)),
                    mean.model = list( armaOrder = c(0,0) , include.mean = F) )
uspec <- multispec(replicate(3,xspec))
Spec <- dccspec(uspec = uspec,dccOrder = c(1, 1), distribution = 'mvnorm')



cl = makePSOCKcluster(3)
multf = multifit(uspec, Data, cluster = cl,out.sample = os,solver = "hybrid")

Fit <- dccfit(Spec, data = Data, fit.control = list(eval.se = TRUE),
              fit = multf, cluster = cl,out.sample = os,solver = "solnp")
DCCtest(Data = Data,cluster = cl, n.lags = 2)



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

save(H_g,file = "./Forecasts_1000/uGARCH_forecasts_1000.Rdata")

save(H_dcc,file = "./Forecasts_1000/DCC_forecasts_1000.Rdata")
stopCluster(cl)



H_diff <- list()
for(i in 1:length(H_g))(
  H_diff[[i]] <- H_g[[i]] - diag(H_dcc[[i]])
)

H_diff

cl = makePSOCKcluster(3)
sim <- dccsim(fitORspec = Fit,n.sim = is+os)

df <- as.matrix(sim@msim$simX)
multfsim = multifit(uspec, data = df[[1]], cluster = cl,out.sample = os,solver = "hybrid")
Fitsim <- dccfit(Spec, data = df[[1]], fit.control = list(eval.se = TRUE),
                 fit = multfsim, cluster = cl,out.sample = os,solver = "solnp")

stopCluster(cl)



