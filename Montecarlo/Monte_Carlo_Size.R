setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("MVWLR_size.R")

#Initial settings:
set.seed(712)

f = function(x){
  return(emdbook::dmvnorm(x, mu = rep(1,3), Sigma = diag(3) ) )
}

g = function(x){
  return(emdbook::dmvnorm(x, mu = rep(-1,3), Sigma = diag(3) ) )
}

alphas = c(0.01, 0.05 , 0.10)

rs = seq(0.1, 4 , by = 0.1)

Monte_Carlo_Size = function(density1, density2, rs, alphas, N, n ){
  Reject_Matrix = matrix(data = 0, nrow = length(rs) , ncol = length(alphas ))
    
    for(r in 1:length(rs) ){
      Reject_r_count = matrix(0, nrow = N , ncol = length(alphas ) )
      
      Integral1 = adaptIntegrate(f , lowerLimit = c(-rs[r],-rs[r],-rs[r]), upperLimit = c(rs[r],rs[r],rs[r]), absError = 1e-6)$integral
      Integral2 = adaptIntegrate(g , lowerLimit = c(-rs[r],-rs[r],-rs[r]), upperLimit = c(rs[r],rs[r],rs[r]), absError = 1e-6)$integral
      
      for(i in 1:N){
      
        sim = mvrnorm(n, mu = rep(0,3) , Sigma = diag(3))  
        
        for (a in 1:length(alphas)) {
          Test = WLR.test.cl.csl(data = sim , density1 = f , density2 = g , r = rs[r], 
                                 score = "cl" , int1 = Integral1 , int2 = Integral2, alpha = alphas[a])
          
          Reject_r_count[i,a] = ifelse(Test$Best_density != "Not significally different" , 1 , 0)
          cat("i=" ,i, "\n" ," r= ",rs[r])
        }
        
      }
     
      for(a in 1:length(alphas) ){Reject_Matrix[r,a] = sum(Reject_r_count[,a])/N}
      
    }
  
  return(Reject_Matrix)  

}


#MC_Matrix = Monte_Carlo_Size(density1 = f, density2 = g, rs = rs, alphas = alphas , N = 10000 , n = 500) 
#save(MC_Matrix, file = "Size.Rdata")
load("Size.Rdata")

Plot_DF = data.frame(rs, MC_Matrix) %>% .[-(1:5),]; names(Plot_DF) = c("r","0.01","0.05","0.1")


Plot_DF %>% gather(key = "Stock", value = "Value",-r) %>% 
  mutate(Color = case_when(grepl(pattern = "0.01", Stock) ~ "0.01",
                           grepl(pattern = "0.05", Stock) ~ "0.05",
                           grepl(pattern = "0.1", Stock) ~ "0.1")) %>% 
  ggplot(mapping = aes(x = r, y = Value, col = Color, group = Stock)) + geom_line() + 
  geom_hline(yintercept = 0.01 , linetype = "dashed") + 
  geom_hline(yintercept = 0.05 , linetype = "dashed") +
  geom_hline(yintercept = 0.1 , linetype = "dashed") +
  scale_color_manual(values = c("#FF0000", "#282088","#169312")) +  labs(color = "Significance\nLevels")
  
