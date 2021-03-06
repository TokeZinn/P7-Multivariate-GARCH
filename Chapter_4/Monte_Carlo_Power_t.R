setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(cubature,emdbook,MASS,mvtnorm,tictoc)



MC_power <- function(c,dist = "norm",B=1e4,dim = 3,rs,alpha = 0.05,
                     inf = 10,tol = 1e-6,df = 3){
  #browser()
  Reject_Matrix_cl <- matrix(data = 0, nrow = length(rs) , ncol = 2)
  Reject_Matrix_csl <- matrix(data = 0, nrow = length(rs) , ncol = 2)
  FUN = function(x){
    return(ifelse(!all(x>=r),1,0))
  }
  FUN_c <- function(x){
    return(as.numeric(!FUN(x)))
  }
  for(j in 1:length(rs)){
    r <- rep(rs[j],dim)
    Reject_r_count_cl <- matrix(data = 0, nrow = B , ncol = 2)
    Reject_r_count_csl <- matrix(data = 0, nrow = B , ncol = 2)
    f <- function(x){
      return(emdbook::dmvnorm(x,mu = rep(0,3),Sigma = diag(3)))
    }
    g <- function(x){
      return(mvtnorm::dmvt(x,delta = rep(0,3),sigma = diag(rep(sqrt((df-2)/df),3)),log = F,df=df))
    }
    int1 <- 1 - cubature::adaptIntegrate(f,lowerLimit = r,
                                         upperLimit = rep(inf,dim),absError = tol)$integral
    int2 <- 1 - cubature::adaptIntegrate(g,lowerLimit = r,
                                         upperLimit = rep(inf,dim),absError = tol)$integral
    n <- ceiling(c/int1)
    for(i in 1:B){
      if(i %% 500 == 0){
        print(i)
      }
      #browser()
      sim <- mvtnorm::rmvt(n, sigma = diag(rep(sqrt((df-2)/df),3)),df = 5)
      {#CL
        Indy <- apply(sim,MARGIN = 1,FUN)
        S1 <- Indy*(log(f(sim)/int1))
        S2 <- Indy*(log(g(sim)/int2))
        #browser()
        WLR <- S1 - S2
        WLR.bar <- sum(WLR)/n
        hacsigma <- sqrt( sum(WLR^2)/n )
        
        t <- WLR.bar*sqrt(n)/(hacsigma)
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
      
      {#CSL
        Indy_c <- apply(sim,MARGIN = 1,FUN_c)
        S1 <- Indy*(log(f(sim))) + Indy_c*(log(1-int1))
        S2 <- Indy*(log(g(sim))) + Indy_c*(log(1-int2))
        
        
        WLR <- S1 - S2
        WLR.bar <- sum(WLR)/n
        hacsigma <- sqrt( sum(WLR^2)/n )
        
        t <- WLR.bar*sqrt(n)/(hacsigma)
        p <- pnorm(t)
        #browser()
        best_csl <- "Not significally different"
        if(is.na(p)){
          best_csl <- "Not significally different"
        }
        else{
          if(p<alpha/2){
            best_csl <- "Density 2"
          }
          if(p>1-alpha/2){
            best_csl <- "Density 1"
          }
        }}
      
      
      Reject_r_count_cl[i,1]<-ifelse(best_cl == "Density 2" , 1 , 0)
      Reject_r_count_csl[i,1]<-ifelse(best_csl == "Density 2" , 1 , 0)
      Reject_r_count_cl[i,2] <- ifelse(best_cl == "Density 1" , 1 , 0)
      Reject_r_count_csl[i,2] <- ifelse(best_csl == "Density 1" , 1 , 0)
    }
    #browser()
    Reject_Matrix_cl[j,] <- c(sum(Reject_r_count_cl[,1])/B,sum(Reject_r_count_cl[,2])/B)
    Reject_Matrix_csl[j,] <- c(sum(Reject_r_count_csl[,1])/B,sum(Reject_r_count_csl[,2])/B)
    print(c("r = ",r))
  }
  return(list(Reject_Matrix_cl,Reject_Matrix_csl))
}


#set.seed(1)
#tic() ; k = MC_power(c = 200,B = 100,rs = c(0,0.5,1),inf = 10,df = 5); toc() 

rr <- 2.5
rs <- seq(from = -rr, to = rr,by = 0.1)
set.seed(771)
tic() ; h <- MC_power(c=200,B = 10000,rs = rs,inf = 10,df=5); toc()  

save(h,"MC_power_t.Rdata")



