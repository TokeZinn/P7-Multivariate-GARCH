setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("MVWLR_size.R")


#Initial settings:
set.seed(711)

f = function(x){
  return(emdbook::dmvnorm(x, mu = rep(1,3), Sigma = diag(3) ) )
}

g = function(x){
  return(emdbook::dmvnorm(x, mu = rep(-1,3), Sigma = diag(3) ) )
}

alphas = c(0.01, 0.05 , 0.10)

rs = seq(0.1,4 , by = 0.1)

Monte_Carlo_Size = function(density1, density2, rs, alphas, N, n ){
  Reject_Matrix = matrix(data = 0, nrow = length(rs) , ncol = length(alphas ))
    
    for(r in rs){
      Reject_r_count = matrix(0, nrow = N , ncol = length(alphas ) )
      
      Integral1 = adaptIntegrate(f , lowerLimit = c(-r,-r,-r), upperLimit = c(r,r,r), absError = 1e-6)$integral
      Integral2 = adaptIntegrate(g , lowerLimit = c(-r,-r,-r), upperLimit = c(r,r,r), absError = 1e-6)$integral
      
      for(i in 1:N){
      
        sim = mvrnorm(n, mu = rep(0,3) , Sigma = diag(3))  
        
        for (a in 1:length(alphas)) {
          Test = WLR.test.cl.csl(data = sim , density1 = f , density2 = g , r = 2 , 
                                 score = "cl" , int1 = Integral1 , int2 = Integral2, alpha = alphas[a])
          
          Reject_r_count[i,a] = ifelse(Test$Best_density != "Not significally different" , 1 , 0)
          cat("i=" ,i, "\n" ," r= ",r)
        }
        
      }
     
      for(a in 1:length(alphas) ){Reject_Matrix[r,a] = sum(Reject_r_count[,a])/N}
      
    }
  
  return(Reject_Matrix)  

}


MC_Matrix = Monte_Carlo_Size(density1 = f, density2 = g, rs = rs, alphas = alphas , N = 1000 , n = 500) 
save(MC_Matrix, file = "Size.Rdata")