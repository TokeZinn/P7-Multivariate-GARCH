

Find_Level = function(mu,sigma, alpha = 0.05, distribution = "normal", ... ){
  library(cubature)
  #browser()
  class(distribution) = distribution
  
  distribution_function = function(dist){
    UseMethod("distribution_function",dist)
  }
  distribution_function.normal = function(dist){
    f = function(x){
      mvtnorm::dmvnorm(x,mean = mu, sigma = sigma)
    }
    return(f)
  }
    
  f = distribution_function(distribution) 
  
  a = mu
  b = mu - 10 
  c = (a+b)/2
  
  
  f_a = function(x){
    f(x)*(f(x)>=f(a))
  }
  f_b = function(x){
    f(x)*(f(x)>=f(b))
  }
  f_c = function(x){
    f(x)*(f(x)>=f(c))
  }
  
  F_a = adaptIntegrate(f_a,rep(-10,length(mu)), rep(10,length(mu)),tol = 1e-3)$integral - (1-alpha)
  F_b = adaptIntegrate(f_b,rep(-10,length(mu)), rep(10,length(mu)),tol = 1e-3)$integral - (1-alpha)
  F_c = adaptIntegrate(f_c,rep(-10,length(mu)), rep(10,length(mu)),tol = 1e-3)$integral - (1-alpha)
  
  if(sign(F_a) >= 0){
    stop("Not possible; integral of single point is greater than 0.95")
  }
  
  
  while(sign(F_a) == sign(F_b)){
    b = b - 3
    f_b = function(x){
      f(x)*(f(x)>=f(b))
    }
    F_b = adaptIntegrate(f_b,rep(-10,length(mu)), rep(10,length(mu)))$integral - (1-alpha)
  }
  
  i = 0
  while(abs(F_c)> 0.001 & i < 100){
    if(sign(F_c) == sign(F_a)){
      a = c
      f_a = f_c
      F_a = F_c
    }else{
      b = c
      f_b = f_c
      F_b = F_c
    }
    
    c = (a+b)/2
    f_c = function(x){
      f(x)*(f(x)>=f(c))
    }
    F_c = adaptIntegrate(f_c,rep(-10,length(mu)), rep(10,length(mu)),tol = 1e-3)$integral - (1-alpha)
    
    i = i + 1
    
    cat(paste0("Iteration ",i," and F(c) is equal to ", F_c))
  }
  
  return(list("level" = c, "integral" = F_c))
}

{
tictoc::tic()
Find_Level(c(0,0,0),diag(3))
tictoc::toc()
}


