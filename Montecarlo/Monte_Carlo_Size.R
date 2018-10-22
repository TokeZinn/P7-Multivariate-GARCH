setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("MVWLR_size.R")

f = function(x){
  return(emdbook::dmvnorm(x, mu = rep(1,3), Sigma = diag(3) ) )
}

g = function(x){
  return(emdbook::dmvnorm(x, mu = rep(-1,3), Sigma = diag(3) ) )
}


alphas = c(0.01, 0.05 , 0.10)

Monte_Carlo_Size = function(data, density1, density2, alphas = )
  

sim = mvrnorm(500, mu = rep(0,3) , Sigma = diag(3))  
  
Integral1 = adaptIntegrate(f , lowerLimit = c(-r,-r,-r), upperLimit = c(r,r,r), absError = 1e-4)$integral
Integral2 = adaptIntegrate(g , lowerLimit = c(-r,-r,-r), upperLimit = c(r,r,r), absError = 1e-4)$integral