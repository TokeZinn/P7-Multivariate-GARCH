setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(cubature,emdbook,MASS,mvtnorm,tictoc)

WLR.test.power = function(data,weight = FALSE,density1,density2,alpha = 0.05,
                          int1,int2,r){
  
  n <- length(data[,1]);m <- length(data[1,])
  
  Y <- as.matrix(data)
  
  FUN = function(x){
    return(ifelse(sum(as.numeric(ifelse(x<r,1,0)))==m,1,0))
  }
  Indy <- apply(Y,MARGIN = 1,FUN)
  if(weight == "csl"){
    FUN_c = function(x){
      return(as.numeric(!FUN(x)))
    }
    Indy_c <- apply(Y,MARGIN = 1,FUN_c)
    
    S1 = Indy*(log(density1(Y))) + Indy_c*(log(1-int1))
    S2 = Indy*(log(density2(Y))) + Indy_c*(log(1-int2))
  }
  if(weight == "cl"){
    Indy <- apply(Y,MARGIN = 1,FUN)
    S1 = Indy*(log(density1(Y)/int1))
    S2 = Indy*(log(density2(Y)/int2))
  }
  WLR <- S1 - S2
  WLR.bar <- sum(WLR)/n
  hacsigma <- sqrt( sum(WLR^2)/n )
  
  t <- WLR.bar*sqrt(n)/(hacsigma)
  p <- pnorm(t)
  best = "Not significally different"
  if(p<alpha/2){
    best = "Density 2"
  }
  if(p>1-alpha/2){
    best = "Density 1"
  }
  p.value <- 2*min(c(1-pnorm(t),pnorm(t)))
  
  return(list(P_value = p.value,
              Statistic = t, 
              Best_density = best ))
}

MC_power = function(c,dist = "norm",B=1e4,dim = 3,rs,w = "cl",
                    inf = 10,tol = 1e-6,df = 3){
  browser()
  for(t in rs){
    tic()
    Reject_r_count = c()
    r = rep(t,dim)
    f = function(x){
      return(emdbook::dmvnorm(x,mu = rep(0,3),Sigma = diag(3)))
    }
    g = function(x){
      return(sqrt((df-2)/df)*mvtnorm::dmvt(x,delta = rep(0,3),sigma = diag(3),log = F,df=df))
    }
    int1 = cubature::adaptIntegrate(f,lowerLimit = rep(-inf,dim),
                                    upperLimit = r,absError = tol)$integral
    int2 = cubature::adaptIntegrate(g,lowerLimit = rep(-inf,dim),
                                    upperLimit = r,absError = tol)$integral
    n = c/int1
    for(i in 1:B){
      sim = mvrnorm(n, mu = rep(0,3) , Sigma = diag(3))  
      test = WLR.test.power(data = sim,weight = w,density1 = f,density2 = g,
                            int1 = int1,int2 = int2,r=r)
      temp = ifelse(Test$Best_density == "Density 1" , 1 , 0)
      temp2 = ifelse(Test$Best_density == "Density 2", -1 ,0)
      Reject_r_count[i] = ifelse(temp == 1 , 1 , temp2)
      
    }
    
    Reject_Matrix[r] = sum(Reject_r_count)
    print(c("r = ",r))
    toc()
  }
  return(result)  
}

rs = seq(from = -3, to = 3,by = 0.1)
set.seed(1)
tic() ; h = MC_power(c=100,B = 10000,w = "cl",rs = rs,inf = 7); toc()  

save(h,file = "cl_power.Rdata")

set.seed(711)
for(j in seq(from = -4, to = 4,by = 0.1)){
  r = rep(j,3)
  tic() ; k = MC_power(c=200,B = 100,w = "csl",r,inf = 7); toc()
  print(c("r = ",r[1]))
}

save(k,file = "csl_power.Rdata")


