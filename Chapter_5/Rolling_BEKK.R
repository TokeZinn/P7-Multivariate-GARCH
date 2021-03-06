Rolling_BEKK = function(IS , OS , Spec = c(1,1),dim = 3,rs=c(1),optim = "BFGS",refit = 1){
  #browser()
  IS = IS %>% as.data.frame() ; OS = OS %>% as.data.frame() ; names(OS) <- names(IS)
  All_Data = rbind(IS,OS) %>% as.matrix()
  
  n = length(IS[,1])
  m = length(OS[,1])
  
  OneSigma = list()
  
  for (i in 1:m) {
    Current_Data = All_Data[i:(n-1+i),]
    
    if(i %% refit == 0 | i == 1){
      Fit = BEKK(as.matrix(Current_Data),order = Spec,method = optim,verbose=F)
      
      C = Fit$est.params[[1]]
      A = Fit$est.params[[2]]
      B = Fit$est.params[[3]] 
      H = Fit$H.estimated[[n]]
      if(i %% (refit*5) == 0){
        print(c("Subiteration = ",i))
      }
    }
    else{
      H = t(C)%*%C + t(A)%*%res%*%t(res)%*%A + t(B)%*%H%*%B
    }
    
    res = Current_Data[n,]
    
    forecast = t(C)%*%C + t(A)%*%res%*%t(res)%*%A + t(B)%*%H%*%B
    
    OneSigma[[i]] = forecast

  }
  
  return(OneSigma)
  
}
