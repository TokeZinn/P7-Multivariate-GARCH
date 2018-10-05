

WLR.test = function(data,weight,density1,density2,mu,sigma,alpha = 0.05){
  #browser()
  n <- length(data)
  Y <- (data-mu)/sigma
  w <- weight(Y)
  
  WLR <- w(Y)*(log(density1)-log(density2))
  WLR.bar <- sum(WLR)/n
  hacsigma <- sum(WLR^2)/n
  t <- WLR.bar*sqrt(n)/(hacsigma)
  p <- pnorm(t)
  best = "Not significally different"
  if(p<alpha/2){
    best = "Density 2"
  }
  if(p>1-alpha/2){
    best = "Density 1"
  }
  p.value <- pnorm(1-abs(t))
  
  return(list(c("P-value =", p.value),c("Statistic =",t),c("Best density is",best)))
}


WLR.test(y,w,density1 = dt(y,df=500),density2 = dnorm(y),mu = 0,sigma=1)

set.seed(1);y <- rnorm(100)

w = function(x){
  return(1)
}








