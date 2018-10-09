setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(tidyverse,rmgarch,mgarchBEKK)



source("./DataAndReturnFct.R")


DF = Return_DF[,5:7] %>% as.data.frame() %>% as.matrix()
OS = Return_DF_OOS[,5:7] %>% as.data.frame() %>% as.matrix()

model = BEKK(as.matrix(DF), order = c(1, 1), params = NULL, fixed = NULL
             , method = "BFGS",
     verbose = F)


dummy = matrix(0,nrow=10,ncol=3)
for(i in 1:3){dummy[,i] = sample(1:10,10,replace=T)}


Rolling_BEKK = function(IS , OS , Spec,dim = 3){
  #browser()
  IS = IS %>% as.data.frame() ; OS = OS %>% as.data.frame() ; names(OS) <- names(IS)
  All_Data = rbind(IS,OS) %>% as.matrix()
  
  n = length(IS[,1])
  m = length(OS[,1])
  
  OneSigma = list()
  
  for (i in 1:m) {
    Current_Data = All_Data[i:(n-1+i),]
    
    Fit = BEKK(as.matrix(Current_Data),order = Spec,method = "BFGS")
    
    C = Fit$est.params[[1]]
    A = Fit$est.params[[2]]
    B = Fit$est.params[[3]]
    H = Fit$H.estimated[[n]]
    
    res = c()
    for(j in 1:dim){
      res = c(res,Fit$residuals[[j]][n])
    }
    
    
    forecast = C%*%t(C) + A%*%res%*%t(res)%*%t(A) + B%*%H%*%t(B)
    
    OneSigma[[i]] = forecast
    
  }
  
  return(OneSigma)
  
}

mod = Rolling_BEKK(DF,OS,c(1,1),dim = 3)








