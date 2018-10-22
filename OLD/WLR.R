setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("DataAndReturnFct.R")

WLR.test = function(data,weight,density1,density2,mu=0,sigma=1,alpha = 0.05){
  #browser()
  n <- length(data)
  Y <- (data-mu)/sigma
  w <- weight(Y)
  
  WLR <- w(Y)*(log(density1)-log(density2))
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
  p.value <- pnorm(1-abs(t))
  
  return(list(P_value = p.value,
              Statistic = t, 
              Best_density = best ))
}


set.seed(912)

y <- rnorm(1000, sd = 1.01)

w = function(x){
  return(1)
}

WLR.test(y, w, density1 = dnorm(y,sd = 1.01), density2 = dnorm(y), mu = 0, sigma=1)



forecast = read.csv("SP500_Roll.csv",sep = ",",header = T)[,2]

y = SP500_returns_OOS
x = forecast[1:(length(forecast)/2)]
z = c(x,x,1)
WLR.test(y,w,density1 = dnorm(y,mean=0,sd=forecast),
         density2 = dnorm(y,mean=0,sd=z))

plot(dnorm(y),dnorm(y,mean=0,sd=forecast))





