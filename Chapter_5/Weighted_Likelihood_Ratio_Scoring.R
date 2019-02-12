scoring_rule = function(x,level,dist = "normal", alpha=0.05){
  browser()
  class(dist) = dist 
  
  distribution = function(dist){
    UseMethod("distribution",dist)
  }
  distribution.normal = function(dist){
    f = function(x){mvtnorm::dmvnorm(x)}
    return(f)
  }
  
  f = distribution(dist)
  
  I = f(x) >= f(level)
  
  S = I*log(f(x)) + (!I)*log(1-alpha)
  
  return(S)
}

