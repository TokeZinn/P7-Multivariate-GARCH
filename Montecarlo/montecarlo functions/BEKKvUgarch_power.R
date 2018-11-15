BEKKvUgarch_power <- function(in.sample,out.sample,alpha = 0.05,B = 100,
                              refit = 1,optim = "BFGS"){
  #browser()
  {
    Reject_Matrix_cl <- matrix(data = 0, nrow = length(1) , ncol = 2)
    Reject_Matrix_csl <- matrix(data = 0, nrow = length(1) , ncol = 2)
    is <- length(in.sample[,1]) ; os <- length(out.sample[,1])
    Reject_r_count_cl <- matrix(data = 0, nrow = B , ncol = 2)
    Reject_r_count_csl <- matrix(data = 0, nrow = B , ncol = 2)
    All_data = rbind(in.sample,out.sample)
    
    Fit <- BEKK(All_data, method = optim)
    H_list <- Fit$H.estimated
    Parameters = c(vech(t(Fit$est.params[[1]])))
    for(i in 2:length(Fit$est.params)){
      Parameters = c(Parameters,vec(Fit$est.params[[i]]))
    }
    
    
    int1 = 1
    int2 = 1
    
    f <- function(x,H){
      d = c()
      for(num in 1:os){
        d[num] <- emdbook::dmvnorm(x[num,],mu = rep(0,3),Sigma = H[[num]])
      }
      return(d)
    }
    
  }
  
  #sim <- list()
  for(i in 1:B){
    simbekk <- simulateBEKK(3,is+os,params = Parameters)
    sim <- matrix(0,nrow = is+os,ncol = 3)
    for(j in 1:3){
      sim[,j] <- simbekk$eps[[j]]
    }
    H_f <- Rolling_BEKK(IS = sim[1:is,],OS = sim[(is+1):(is+os),],refit = refit,optim = optim)
    g_matrix <- matrix(0,ncol = 3,nrow = os)
    #for(j in 1:3){
    #  g_matrix[,j] <- RollingForecast(IS = sim[1:is,j],OS = sim[(is+1):(is+os),j])
    #}
    
    Spec = ugarchspec(variance.model = list( model = "sGARCH", garchOrder = c(1,1)),
                     mean.model = list( armaOrder = c(0,0) , include.mean = F) )
    
    for(j in 1:3){
      roll = ugarchroll(spec = Spec,data = sim[,j],forecast.length = os,
                        refit.every = refit,refit.window = "moving",solver = "hybrid",
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
    print(c("i = ", i))
    print(c("Best =",best_cl))
  }
  #browser()
  j = 1
  Reject_Matrix_cl[j,] <- c(sum(Reject_r_count_cl[,1])/B,sum(Reject_r_count_cl[,2])/B)
  Reject_Matrix_csl[j,] <- c(sum(Reject_r_count_csl[,1])/B,sum(Reject_r_count_csl[,2])/B)
  
  return(list(Reject_Matrix_cl,Reject_Matrix_csl))
}




