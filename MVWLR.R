setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(mvtnorm,tidyverse,emdbook)

WLR.test = function(data,weight = function(x){rep(1,length(x))},density1,
                    density2,alpha = 0.05){
  #browser()
  n <- length(data[,1]);m <- length(data[1,])
  
  Y <- as.matrix(data)
  #Y <- (data-mu)%*%solve(Hsqr)
  
  w <- weight(Y)
  WLR <- w*(log(density1(Y))-log(density2(Y)))
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
  p.value <- 2*min(c(1-pnorm(t),pnorm(t)))
  
  return(list(c("P-value =", p.value),c("Statistic =",t),c("Best density is",best)))
}


set.seed(711)
k = rmvnorm(3,sigma = diag(1,nrow=100)) %>% t()
d1 = function(x){
  dmvnorm(x,mu = rep(0,times = 3),
                   Sigma=diag(1,nrow=length(x[1,]),ncol=length(x[1,])))
}
d3 = function(x){
  dmvnorm(x,mu = rep(0,times = 3),
          Sigma=diag(1.4,nrow=length(x[1,]),ncol=length(x[1,])))
}
d2 = function(x){
  dmvt(x,delta = rep(0,times = 3),
                   sigma=diag(1,nrow=length(x[1,]),ncol=length(x[1,])),
       df = 3, log = F)
}





WLR.test(data = k,density1 = d1,density2 = d2)

