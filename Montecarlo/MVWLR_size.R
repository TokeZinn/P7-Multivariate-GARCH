pacman::p_load(cubature,emdbook, tidyverse, MASS ,tictoc)

Indicator_Function = function(x , r , n = 3) { 
  
  Indication = ifelse(n == (abs(x) < r) %>% sum(), 1 , 0)
  
}


WLR.test.cl.csl = function(data, density1, density2, alpha = 0.05, score , r , int1 , int2){
  #browser()
  n <- length(data[,1]);m <- length(data[1,])
  
  Y <- as.matrix(data)
  
  if(score == "cl"){
    
    Indicator = apply(Y, 1 , FUN = Indicator_Function, r = r )
    
    WLR = Indicator*( log( density1(Y) / int1 ) - log( density2(Y) / int2 ) )
    
  }else if(score == "csl"){
    Integral1 = 1 - int1
    
    Integral2 = 1 - int2
    
    Indicator1 = apply(Y, 1 , FUN = Indicator_Function , r = r)
    Indicator2 = !Indicator1 %>% as.numeric()
    
    WLR = Indicator1*( log( density1(Y) ) - log( density2(Y) ) ) + Indicator2*( log(Integral1) - log(Integral2) )
}

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



#WLR.test.cl.csl(data = sim , density1 = f , density2 = g , r = 2 , score = "csl" , int1 = Integral1 , int2 = Integral2)
