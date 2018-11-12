setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(cubature,emdbook,MASS,mvtnorm,tictoc,parallel,mgarchBEKK,tidyverse,rugarch,
               matrixcalc)
source("./DATA/DataAndReturnFct.R")


Rolling_BEKK = function(IS , OS , Spec = c(1,1),dim = 3,rs=c(1)){
  #browser()
  IS = IS %>% as.data.frame() ; OS = OS %>% as.data.frame() ; names(OS) <- names(IS)
  All_Data = rbind(IS,OS) %>% as.matrix()
  
  n = length(IS[,1])
  m = length(OS[,1])
  
  OneSigma = list()
  
  for (i in 1:m) {
    Current_Data = All_Data[i:(n-1+i),]
    
    if(i %% 5 == 0 | i == 1){
      Fit = BEKK(as.matrix(Current_Data),order = Spec,method = "BFGS",verbose=F)
      
      C = Fit$est.params[[1]]
      A = Fit$est.params[[2]]
      B = Fit$est.params[[3]] 
      H = Fit$H.estimated[[n]]
    }
    else{
      H = t(C)%*%C + t(A)%*%res%*%t(res)%*%A + t(B)%*%H%*%B
    }
    
    
    #res = c()
    #for(j in 1:dim){
    #  res = c(res,Fit$residuals[[j]][n])
    #}
    res = Current_Data[n,]
    
    forecast = t(C)%*%C + t(A)%*%res%*%t(res)%*%A + t(B)%*%H%*%B
    
    OneSigma[[i]] = forecast
    
    #print(c("Iteration = ",i),sep="\n")
  }
  
  return(OneSigma)
  
}


MC_power_Bekk <- function(in.sample,out.sample,alpha = 0.05,B = 100){
  #browser()
  {
    Reject_Matrix_cl <- matrix(data = 0, nrow = length(1) , ncol = 2)
    Reject_Matrix_csl <- matrix(data = 0, nrow = length(1) , ncol = 2)
    is <- length(in.sample[,1]) ; os <- length(out.sample[,1])
    Reject_r_count_cl <- matrix(data = 0, nrow = B , ncol = 2)
    Reject_r_count_csl <- matrix(data = 0, nrow = B , ncol = 2)
    All_data = rbind(in.sample,out.sample)
    

    for(j in 1:3){
      assign(paste("Fit",j,sep = "_"),
             ugarchfit(spec = Spec,data = All_data[,j],solver = "hybrid"))
    }
    fits <- list(Fit_1,Fit_2,Fit_3) 
    
    int1 = 1
    int2 = 1
    
    f <- function(x,H){
      d = c()
      for(num in 1:os){
        d[num] <- emdbook::dmvnorm(x,mu = rep(0,3),Sigma = H[[num]])
      }
      return(d)
    }
    
  }
  
  #sim <- list()
  for(i in 1:B){
    
    for(j in 1:3){
      assign(paste("simG",j,sep = "_"),
             ugarchsim(fit = fits[[1]],n.sim = length(All_data[,1])))
    }

    sim <- matrix(0,nrow = is+os,ncol = 3)
    sim[,1] <- simG_1@simulation$seriesSim
    sim[,2] <- simG_2@simulation$seriesSim
    sim[,3] <- simG_3@simulation$seriesSim
    
    H_f <- Rolling_BEKK(IS = sim[1:is,],OS = sim[(is+1):(is+os),])
    g_matrix <- matrix(0,ncol = 3,nrow = os)

    Spec = ugarchspec(variance.model = list( model = "sGARCH", garchOrder = c(1,1)),
                      mean.model = list( armaOrder = c(0,0) , include.mean = F) )
    
    for(j in 1:3){
      roll = ugarchroll(spec = Spec,data = sim[,j],forecast.length = os,
                        refit.every = 5,refit.window = "moving",solver = "hybrid",
                        calculate.VaR = F,window.size = is)
      g_matrix[,j] <- (roll@forecast$density$Sigma)^2
    }
    
    H_g = list()
    for(j in 1:os){
      H_g[[j]] <- diag(g_matrix[j,])
    }
    
    #CL
    {
      Indy <- 1
      S1 <- Indy*(log(f(sim[(is+1):(is+os),],H_f)/int1))
      S2 <- Indy*(log(f(sim[(is+1):(is+os),],H_g)/int2))
      
      #browser()
      
      WLR <- S1 - S2
      WLR.bar <- sum(WLR)/os
      hacsigma <- sqrt( sum(WLR^2)/os )
      
      t <- WLR.bar*sqrt(os)/(hacsigma)
      p <- pnorm(t)
      best_cl <- "Not significally different"
      if(is.na(p)){
        best_cl <- "Not significally different"
      }
      else{
        if(p<alpha/2){
          best_cl <- "Density 2"
        }
        if(p>1-alpha/2){
          best_cl <- "Density 1"
        }
      }}

    
    Reject_r_count_cl[i,1]<-ifelse(best_cl == "Density 1" , 1 , 0)
    #Reject_r_count_csl[i,1]<-ifelse(best_csl == "Density 1" , 1 , 0)
    Reject_r_count_cl[i,2] <- ifelse(best_cl == "Density 2" , 1 , 0)
    #Reject_r_count_csl[i,2] <- ifelse(best_csl == "Density 2" , 1 , 0)
    print(c("i = ", i))
  }
  #browser()
  j = 1
  Reject_Matrix_cl[j,] <- c(sum(Reject_r_count_cl[,1])/B,sum(Reject_r_count_cl[,2])/B)
  Reject_Matrix_csl[j,] <- c(sum(Reject_r_count_csl[,1])/B,sum(Reject_r_count_csl[,2])/B)
  
  return(list(Reject_Matrix_cl,Reject_Matrix_csl))
}

DF = Return_DF[,5:7] %>% as.data.frame() %>% as.matrix()
OS = Return_DF_OOS[,5:7] %>% as.data.frame() %>% as.matrix()
end = length(DF[,1]); end2 = length(OS[,1])
set.seed(1)
tic() ; Result = MC_power_Bekk(in.sample = DF[(end-100):end,],
                               out.sample = OS[1:19,],B = 10); toc()

save(Result,file = "Garch_power_GvBekk.Rdata")



