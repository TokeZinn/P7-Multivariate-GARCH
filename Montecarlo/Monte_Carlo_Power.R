setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(cubature,emdbook,MASS,mvtnorm,tictoc)

MC_power = function(c,dist = "norm",B=1e4,dim = 3,rs,alpha = 0.05,
                    inf = 10,tol = 1e-6,df = 3){
  #browser()
  Reject_Matrix_cl = matrix(data = 0, nrow = length(rs) , ncol = 2)
  Reject_Matrix_csl = matrix(data = 0, nrow = length(rs) , ncol = 2)
  FUN = function(x){
    return(ifelse(sum(as.numeric(ifelse(x<r,1,0)))==3,1,0))
  }
  FUN_c = function(x){
    return(as.numeric(!FUN(x)))
  }
  for(t in rs){
    Reject_r_count_cl = matrix(data = 0, nrow = B , ncol = 2)
    Reject_r_count_csl = matrix(data = 0, nrow = B , ncol = 2)
    r = rep(t,dim)
    f = function(x){
      return(emdbook::dmvnorm(x,mu = rep(0,3),Sigma = diag(3)))
    }
    g = function(x){
      return(mvtnorm::dmvt(x,delta = rep(0,3),sigma = diag(rep(sqrt((df-2)/df),3)),log = F,df=df))
    }
    int1 = cubature::adaptIntegrate(f,lowerLimit = rep(-inf,dim),
                                    upperLimit = r,absError = tol)$integral
    int2 = cubature::adaptIntegrate(g,lowerLimit = rep(-inf,dim),
                                    upperLimit = r,absError = tol)$integral
    n = sqrt(c/int1)
    for(i in 1:B){
      WLR_bar = 0
      hacsigma_cl = 0
      hacsigma_csl = 0
      if(i %% 100 == 0){
        print(i)
      }
      
      j <- 0
      while(j < n){
        sim = mvrnorm(1, mu = rep(0,3) , Sigma = diag(3))
        {#CL
          Indy <- FUN(sim)
          S1 = Indy*(log(f(sim)/int1))
          S2 = Indy*(log(g(sim)/int2))
          WLR <- S1-S2
          WLR_bar_cl <- WLR_bar+WLR
          hacsigma_cl <- hacsigma_cl + WLR^2
        }
        {#CSL
          Indy_c <- FUN_c(sim)
          S1 = Indy*(log(f(sim))) + Indy_c*(log(1-int1))
          S2 = Indy*(log(g(sim))) + Indy_c*(log(1-int2))
          WLR <- S1-S2
          WLR_bar_csl <- WLR_bar+WLR
          hacsigma_csl <- hacsigma_csl + WLR^2
        }
        j = j + 1
      }
      
      #browser()
      hacsigma_cl <- sqrt( hacsigma_cl/n )
      WLR_bar_cl <- WLR_bar_cl/n
      t <- WLR_bar_cl*sqrt(n)/(hacsigma_cl)
      p <- pnorm(t)
      best_cl = "Not significally different"
      if(!is.na(p)){
        if(p<alpha/2){
          best_cl = "Density 2"
        }
        if(p>1-alpha/2){
          best_cl = "Density 1"
        }
      }
      
      hacsigma_csl <- sqrt( hacsigma_csl/n )
      WLR_bar_csl <- WLR_bar_csl/n
      t <- WLR_bar_csl*sqrt(n)/(hacsigma_csl)
      p <- pnorm(t)
      best_csl = "Not significally different"
      if(!is.na(p)){
        if(p<alpha/2){
          best_csl = "Density 2"
        }
        if(p>1-alpha/2){
          best_csl = "Density 1"
        }
      }
      
      Reject_r_count_cl[i,1] = ifelse(best_cl == "Density 1" , 1 , 0)
      Reject_r_count_csl[i,1] = ifelse(best_csl == "Density 1" , 1 , 0)
      Reject_r_count_cl[i,2] = ifelse(best_cl == "Density 2" , 1 , 0)
      Reject_r_count_csl[i,2] = ifelse(best_csl == "Density 2" , 1 , 0)
    }
    
    Reject_Matrix_cl[r,] = c(sum(Reject_r_count_cl[,1])/B,sum(Reject_r_count_cl[,2])/B)
    Reject_Matrix_csl[r,] = c(sum(Reject_r_count_csl[,1])/B,sum(Reject_r_count_csl[,2])/B)
    print(c("r = ",r))
  }
  return(list(Reject_Matrix_cl,Reject_Matrix_csl))
}

rr = 1
rs = seq(from = -rr, to = rr,by = 0.1)
set.seed(1)
tic() ; h = MC_power(c = 5,B = 10000,rs = 0,inf = 10); toc()  
tic() ; h = MC_power(c=5,B = 10000,rs = rs,inf = 10); toc()  

save(h,file = "Power.Rdata")


