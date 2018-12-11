library(readxl)

IBM = read_excel("C:/Users/toke/Desktop/IBM.xlsx")

IBM %>% pull(Close) %>% (function(x){
  res = c()
  for(i in 2:length(x)){
    res = c(res, log(x[i])-log(x[i-1]))
  }
  return(res)
}) -> log_returns


mu = mean(log_returns)
sigma = sd(log_returns)

simulate_bm = function(drift, diffusion, length.out, sim = 10000, P_0, r = 0.03){
  browser()
  
  h = 1/length.out
  end = c()
  for(j in 1:sim){
    r_k = c()
    P_k = c()
    for(i in 1:length.out){
      r_k = c(r_k, drift*h + diffusion*sqrt(h)*rnorm(1,0,1))
      P_k = c(P_k, P_0*exp(sum(r_k)))
    }
    P_n = P_0*exp(sum(r_k))
    end = c(end, max(P_k))
  }
  
  fair_price = exp(-r)*mean(end) - P_0
}

simulate_bm(mu,sigma,253,r = 0.01,P_0 = IBM$Close[[1264]])
