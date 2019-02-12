uGARCHvDCC_power <- function(in.sample,out.sample,alpha = 0.05,B = 100){
  #browser()
  {
    Reject_Matrix_cl <- matrix(data = 0, nrow = length(1) , ncol = 2)
    Reject_Matrix_csl <- matrix(data = 0, nrow = length(1) , ncol = 2)
    is <- length(in.sample[,1]) ; os <- length(out.sample[,1])
    Reject_r_count_cl <- matrix(data = 0, nrow = B , ncol = 2)
    Reject_r_count_csl <- matrix(data = 0, nrow = B , ncol = 2)
    All_data = rbind(in.sample,out.sample)
    
    
    xspec <- ugarchspec(variance.model = list( model = "sGARCH", garchOrder = c(1,1)),
                        mean.model = list( armaOrder = c(0,0) , include.mean = F) )
    uspec <- multispec(replicate(3,xspec))
    Spec <- dccspec(uspec = uspec,dccOrder = c(1, 1), distribution = 'mvnorm')
    
    cl = makePSOCKcluster(3)
    multf = multifit(uspec, All_data, cluster = cl,out.sample = 0,solver = "hybrid")
    #browser()
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
  
  for(i in 1:B){
    
    for(j in 1:3){
      assign(paste("simG",j,sep = "_"),
             ugarchsim(fit = multf@fit[[j]],n.sim = length(All_data[,1])))
    }
    
    sim <- matrix(0,nrow = is+os,ncol = 3)
    sim[,1] <- simG_1@simulation$seriesSim
    sim[,2] <- simG_2@simulation$seriesSim
    sim[,3] <- simG_3@simulation$seriesSim
    
    
    multfsim = multifit(uspec, sim, cluster = cl,out.sample = os,solver = "hybrid")
    fit <- dccfit(Spec, data = sim, fit.control = list(eval.se = TRUE),
                  fit = multfsim, cluster = cl,out.sample = os,solver = "solnp")
    
    Forecast <- dccforecast(fit, n.roll = os-1,cluster = cl)
    
    H_f <- Forecast@mforecast$H
    for(j in 1:os){
      H_f[[j]] <- H_f[[j]] %>%  as.data.frame() %>% as.matrix()
    }
    
    ucast <- multiforecast(multifitORspec = multfsim, data = sim, n.ahead = 1, n.roll = os-1,
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
  
  stopCluster(cl)
  return(list(Reject_Matrix_cl,Reject_Matrix_csl))
}



