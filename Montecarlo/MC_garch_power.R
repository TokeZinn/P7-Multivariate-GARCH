setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(cubature,emdbook,MASS,mvtnorm,tictoc,parallel,mgarchBEKK,tidyverse,rugarch)
#source("../DATA/DataAndReturnFct.R")
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
cl = makePSOCKcluster(10)


Rolling_BEKK = function(IS , OS , Spec = c(1,1),dim = 3,rs=c(1)){
  #browser()
  IS = IS %>% as.data.frame() ; OS = OS %>% as.data.frame() ; names(OS) <- names(IS)
  All_Data = rbind(IS,OS) %>% as.matrix()
  
  n = length(IS[,1])
  m = length(OS[,1])
  
  OneSigma = list()
  
  for (i in 1:m) {
    Current_Data = All_Data[i:(n-1+i),]
    
    Fit = BEKK(as.matrix(Current_Data),order = Spec,method = "BFGS",verbose=F)
    
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
    
    print(c("Iteration = ",i),sep="\n")
  }
  
  return(OneSigma)
  
}
RollingForecast = function(IS , OS ){
  Spec = ugarchspec(variance.model = list( model = "sGARCH", garchOrder = c(1,1)),
                    mean.model = list( armaOrder = c(0,0) , include.mean = F) )
  All_Data = c( IS, OS)
  
  n = length(IS)
  m = length(OS)
  
  OneSigma = rep(0,m)
  
  for (i in 1:m) {
    Current_Data = All_Data[i:(n-1+i)]
    
    Fit = ugarchfit(Spec, data = Current_Data , solver = "hybrid",cluster = cl)
    
    forecast = ugarchforecast(Fit, n.ahead = 1)
    
    OneSigma[i] = forecast@forecast$sigmaFor %>% as.numeric()
    
  }
  
  return(OneSigma)
  
}


MC_power_Bekk <- function(in.sample,out.sample,alpha = 0.05,B = 100){
  #browser()
  Reject_Matrix_cl <- matrix(data = 0, nrow = length(1) , ncol = 2)
  Reject_Matrix_csl <- matrix(data = 0, nrow = length(1) , ncol = 2)
  is <- length(in.sample[,1]) ; os <- length(out.sample[,1])
  Reject_r_count_cl <- matrix(data = 0, nrow = B , ncol = 2)
  Reject_r_count_csl <- matrix(data = 0, nrow = B , ncol = 2)
  
  Fit <- BEKK(rbind(in.sample,out.sample))
  H_list <- Fit$H.estimated
  
  int1 = 1
  int2 = 1
  
  f <- function(x,H){
    d = c()
    for(num in 1:os){
      d[num] <- emdbook::dmvnorm(x,mu = rep(0,3),Sigma = H[[num]])
    }
    return(d)
  }
  
  n =  #fix
  sim <- list()
  for(i in 1:B){
    sim[[i]] = matrix(0,nrow = is+os,ncol = 3)
    for (t in 1:length(H_list)) {
      sim[[i]][t,] <- MASS::mvrnorm(1, mu = rep(0,3) , Sigma = H_list[[t]])
    }  
    
    H_f <- Rolling_BEKK(IS = sim[[i]][1:is,],OS = sim[[i]][(is+1):(is+os),])
    g_matrix <- matrix(0,ncol = 3,nrow = os)
    for(j in 1:3){
      g_matrix[,j] <- RollingForecast(IS = sim[[i]][1:is,j],OS = sim[[i]][(is+1):(is+os),j])
    }
    H_g = list()
    for(j in 1:os){
      H_g[[j]] <- diag(g_matrix[j,])
    }
    
    #CL
    {
      Indy <- 1
      S1 <- Indy*(log(f(sim[[i]][(is+1):(is+os),],H_f)/int1))
      S2 <- Indy*(log(f(sim[[i]][(is+1):(is+os),],H_g)/int2))
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
    #CSL
    {
    #   Indy_c <- 0
    #   S1 <- Indy*(log(f(sim))) + Indy_c*(log(1-int1))
    #   S2 <- Indy*(log(g(sim))) + Indy_c*(log(1-int2))
    # 
    # 
    #   WLR <- S1 - S2
    #   WLR.bar <- sum(WLR)/n
    #   hacsigma <- sqrt( sum(WLR^2)/n )
    # 
    #   t <- WLR.bar*sqrt(n)/(hacsigma)
    #   p <- pnorm(t)
    #   best_csl <- "Not significally different"
    #   if(is.na(p)){
    #     best_csl <- "Not significally different"
    #   }
    #   else{
    #     if(p<alpha/2){
    #       best_csl <- "Density 2"
    #     }
    #     if(p>1-alpha/2){
    #       best_csl <- "Density 1"
    #     }
    #   }
      }
    
    Reject_r_count_cl[i,1]<-ifelse(best_cl == "Density 1" , 1 , 0)
    #Reject_r_count_csl[i,1]<-ifelse(best_csl == "Density 1" , 1 , 0)
    Reject_r_count_cl[i,2] <- ifelse(best_cl == "Density 2" , 1 , 0)
    #Reject_r_count_csl[i,2] <- ifelse(best_csl == "Density 2" , 1 , 0)
  }
  #browser()
  j = 1
  browser()
  Reject_Matrix_cl[j,] <- c(sum(Reject_r_count_cl[,1])/B,sum(Reject_r_count_cl[,2])/B)
  Reject_Matrix_csl[j,] <- c(sum(Reject_r_count_csl[,1])/B,sum(Reject_r_count_csl[,2])/B)
  
  return(list(Reject_Matrix_cl,Reject_Matrix_csl))
}

DF = Return_DF[,5:7] %>% as.data.frame() %>% as.matrix()
OS = Return_DF_OOS[,5:7] %>% as.data.frame() %>% as.matrix()
end = length(DF[,1]); end2 = length(OS[,1])
set.seed(1)
tic() ; k = MC_power_Bekk(in.sample = DF[(end-100):end,],
                          out.sample = OS[1:5,],B = 2); toc()



rr <- 2.5
rs <- seq(from = -rr, to = rr,by = 0.1)
set.seed(771)
tic() ; Result <- MC_power(c=200,B = 10000,rs = rs,inf = 10,df=5); toc()  

#save(Result,file = "Garch_power.Rdata")

matrix = cbind(matrix(rbind(matrix(SP500_returns),matrix(SP500_returns_OOS))),
               matrix(rbind(matrix(Gold_returns),matrix(Gold_returns_OOS))),
               matrix(rbind(matrix(Oil_returns),matrix(Oil_returns_OOS))))
g_matrix = matrix(0,ncol = 3,nrow = 100)
tic() ; for(j in 1:3){
  g_matrix[,j] <- RollingForecast(IS = matrix[1:100,j],OS = matrix[101:200,j])
} ; toc()


