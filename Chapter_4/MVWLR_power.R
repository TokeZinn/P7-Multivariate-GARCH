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