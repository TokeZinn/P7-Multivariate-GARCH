library(tidyverse)
load("2012_20pct_Integral.RData")
load("./DATA/Workspace2012.RData")
load("./Forecasts_2012/BEKK_forecasts_2012_BFGS.RData")
load("./Forecasts_2012/Benchmark_forecasts_2012.RData")
load("./Forecasts_2012/DCC_forecasts_2012.RData")
load("./Forecasts_2012/uGARCH_forecasts_2012.RData")


extract_level = function(List){
  list_new = list()
  for(i in 1:length(List)){
    list_new[[i]] = List[[i]]$level
  }
  return(list_new)
}
build_densities = function(Sigma){
  f = function(x){mvtnorm::dmvnorm(x,sigma = Sigma)}
  return(f)
}
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



densities_BENCH = lapply(bench_2012, build_densities)
densities_BEKK = lapply(H_bekk, build_densities)
densities_DCC = lapply(H_dcc, build_densities)
densities_g = lapply(H_g, build_densities)
Result %>% extract_level() -> levels


Conditional_WLR = function(Y,f,g,level,alpha = 0.20,debug = F, level_functions = densities_BENCH){
  if(debug){
    browser()
  }
  
  S = function(x, func, level,level_func){
    
    if(debug){
      browser()
    }
    
    f_l = level_func(level)
    f_x = level_func(x)
    I = f_x >= f_l
    
    
    
    if(I){
      f_tilde = function(x){(level_func(x) >= level_func(level))*func(x)}
      J = integral(f_tilde,lower = rep(-10,length(x)),upper = rep(10,length(x)))
      s = log(J)
    }else{
      s = log(func(x))
    }
    
    return(s)
  }
  
  Y = as.matrix(Y)
  n = nrow(Y)
  
  d = list()
  
  
  for(i in 1:n){
    
    S_f = S(Y[i,], f[[i]], level[[i]], level_functions[[i]]) 
    S_g = S(Y[i,], g[[i]], level[[i]], level_functions[[i]]) 
    
    d[[i]] = S_f - S_g
    
    cat(paste("Evaluating Y_",i,"\n",sep = ""))
  }
  
  #browser()
  
  WLR = sum(unlist(d))/n
  HAC = sum(unlist(d)^2)/n
  t = WLR/(sqrt(HAC)/sqrt(n))
  
  reject = (abs(t) > qnorm(1-alpha/2))
  
  result = list("Statistic" = t, "Result" = "Not Statistically Different", "Diff" = d)
  
  if(reject & (sign(WLR) == 1)){
    result[["Result"]] = "f is the best density"
  }
  
  if(reject & (sign(WLR) == -1)){
    result[["Result"]] = "g is the best density"
  }
  
  result[["p-value"]] = 1 - pnorm(abs(t))
  
  class(result) = c("WLR")
  
  return(result)
  
  
}


BENCH_BEKK_2012 = Conditional_WLR(OS,densities_BENCH,densities_BEKK,levels,debug = F)
BENCH_DCC_2012 = Conditional_WLR(OS,densities_BENCH,densities_DCC,levels,debug = F)
BENCH_uGARCH_2012 = Conditional_WLR(OS,densities_BENCH,densities_g,levels,debug = F)
BEKK_DCC_2012 = Conditional_WLR(OS,densities_BEKK,densities_DCC,levels,debug = F)
BEKK_uGARCH_2012 = Conditional_WLR(OS,densities_BEKK,densities_g,levels,debug = F)
DCC_uGARCH_2012 = Conditional_WLR(OS,densities_DCC,densities_g,levels,debug = F)
