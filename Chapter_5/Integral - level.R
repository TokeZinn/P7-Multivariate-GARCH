

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
  
  F_a = integral(f_a,rep(-10,length(mu)), rep(10,length(mu))) - (1-alpha)
  F_b = integral(f_b,rep(-10,length(mu)), rep(10,length(mu))) - (1-alpha)
  F_c = integral(f_c,rep(-10,length(mu)), rep(10,length(mu))) - (1-alpha)
  
  if(sign(F_a) >= 0){
    stop("Not possible; integral of single point is greater than 0.95")
  }
  
  
  while(sign(F_a) == sign(F_b)){
    b = b - 3
    f_b = function(x){
      f(x)*(f(x)>=f(b))
    }
    F_b = integral(f_b,rep(-10,length(mu)), rep(10,length(mu))) - (1-alpha)
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
    F_c = integral(f_c,rep(-10,length(mu)), rep(10,length(mu))) - (1-alpha)
    
    i = i + 1
    
    cat(paste0("Iteration ",i," and F(c) is equal to ", F_c,"\n"))
  }
  
  return(list("level" = c, "integral" = F_c))
}

mu = c(0,0,0)
Sigma = diag(3)
f = function(x){dmvnorm(x)}

Find_Level(mu,Sigma)

Level_Integral = function(mu,sigma, alpha = 0.05, distribution = "normal", ... ){
  library(cubature)
  browser()
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
  b = mu - 9
  c = (a+b)/2
  f_a = f(a)
  f_b = f(b)
  f_c = f(c)
  
  f_tilde = function(x){
    f_x = f(x)
    f_x = f_x*(f_a >= f_x)*(f_x >= f_c)
    return(f_x)
  }
  
  F_tilde = integral(f_tilde, lower = rep(-10,length(mu)), upper = rep(10,length(mu))) 
  
  while(abs(F_tilde - (1-alpha))>= 0.001){
    if(F_tilde > 0){
      c = (a + c) / 2
      f_c = f(c)
      f_tilde = function(x){
        f_x = f(x)
        f_x = f_x*(f_a >= f_x)*(f_x >= f_c)
        return(f_x)
      }
      F_tilde = adaptIntegrate(f_tilde,rep(-9,length(mu)), rep(9,length(mu)),tol = 1e-1)$integral
    }else{
      a = c 
      c = (a + b) / 2
      f_a = f(a)
      f_c = f(a)
      f_tilde = function(x){
        f_x = f(x)
        f_x = f_x*(f_a >= f_x)*(f_x >= f_c)
        return(f_x)
      }
      F_temp = adaptIntegrate(f_tilde,rep(-9,length(mu)), rep(9,length(mu)),tol = 1e-3)$integral
      F_tilde = F_tilde + F_temp
    }
    
    
    
    
  }
  
  
  
  while(abs(F_tilde)> 0.001){
    
    
    
    
    
    
  }
}

integral(dnorm,-1.96,1.96)


integral = function(f, lower, upper,  i = 1e+7){
  #browser()
  U = matrix(0,ncol = length(upper), nrow = i)
  V = 1
  for (j in 1:length(upper)){
    U[,j] = runif(n = i,min = lower[j], max = upper[j])
    V = V*(upper[j]-lower[j])
  }
  
  f_u = f(U)
  
  return((V/i)*sum(f_u))
}


