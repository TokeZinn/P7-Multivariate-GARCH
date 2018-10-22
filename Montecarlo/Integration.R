setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(cubature,emdbook)

f = function(x){
  return(dmvnorm(x,mu = rep(0,3),Sigma = diag(3)))
}
g = function(x){
  return(x)
}
adaptIntegrate(f,lowerLimit = c(-10,-10,-10),upperLimit = c(-1,-1,-1),absError = 1e-7)
adaptIntegrate(f,lowerLimit = c(-3,-3,-3),upperLimit = c(-1,-1,-1),absError = 1e-7)



adaptIntegrate(g,lowerLimit = c(-1,-1,-1),upperLimit = c(1,1,1))







