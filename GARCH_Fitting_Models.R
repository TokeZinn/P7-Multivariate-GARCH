# Getting data

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pacman::p_load(tidyverse, rugarch, tictoc)

source("./DATA/DataAndReturnFct.R")

#Fitting garch models using rugarch.

Spec = ugarchspec(variance.model = list( model = "sGARCH", garchOrder = c(1,1)),
                  mean.model = list( armaOrder = c(0,0) , include.mean = F) )


SP_garch = ugarchfit(Spec, data = SP500_returns , solver = "hybrid")
#SP_garch

Gold_garch = ugarchfit(Spec, data = Gold_returns , solver = "hybrid")
#Gold_garch


Oil_garch = ugarchfit(Spec, data = Oil_returns , solver = "hybrid")
#Oil_garch


RollingForecast = function(IS , OS , Spec){
  
  All_Data = c( IS, OS)
  
  n = length(IS)
  m = length(OS)
  
  OneSigma = rep(0,m)
  
  for (i in 1:m) {
    Current_Data = All_Data[i:(n-1+i)]
  
    Fit = ugarchfit(Spec, data = Current_Data , solver = "hybrid")
    
    forecast = ugarchforecast(Fit, n.ahead = 1)
    
    OneSigma[i] = forecast@forecast$sigmaFor %>% as.numeric()
    
  }
 
  return(OneSigma)
   
}


SP500_Roll = RollingForecast(SP500_returns,SP500_returns_OOS, Spec = Spec)
Gold_Roll =  RollingForecast(Gold_returns,Gold_returns_OOS, Spec = Spec)
Oil_Roll  =  RollingForecast(Oil_returns,Gold_returns_OOS, Spec = Spec)

write.csv(SP500_Roll, file = "SP500_Roll.csv")
write.csv(Gold_Roll, file = "Gold_Roll.csv")
write.csv(Oil_Roll, file = "Oil_Roll.csv")
