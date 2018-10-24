setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(cubature,emdbook,mvtnorm)


f = function(x){
  return(emdbook::dmvnorm(x,mu = rep(0,3),Sigma = diag(3)))
}
g = function(x){
  return(sqrt((3-2)/3)*mvtnorm::dmvt(x,delta = rep(0,3),sigma = diag(3),log = F,df=3))
}
g2 = function(x){
  return(mvtnorm::dmvt(x,delta = rep(0,3),sigma = diag(3),log = F,df=3))
}
adaptIntegrate(f,lowerLimit = c(-10,-10,-10),upperLimit = rep(10,3),absError = 1e-7)
adaptIntegrate(g,lowerLimit = rep(-10,3),upperLimit = rep(10,3),absError = 1e-7)
adaptIntegrate(g2,lowerLimit = rep(-10,3),upperLimit = rep(10,3),absError = 1e-7)



adaptIntegrate(g,lowerLimit = c(-1,-1,-1),upperLimit = c(1,1,1))


200/(1-adaptIntegrate(f,lowerLimit = rep(-3,3),upperLimit = rep(10,3),absError = 1e-6)$integral)



