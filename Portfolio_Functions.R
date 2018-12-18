setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("./DATA/DataAndReturnFct_FullSample.R")

Return_DF %>% dplyr::select(Returns_SP500, Returns_Gold, Returns_Oil) %>% as.matrix() -> X

Min_Variance_Port = function(X, Sigma = NULL){
  #browser()
  mean_matrix = function(X){
    m = c()
    for (i in 1:ncol(X)){
      m = c(m, mean(X[,i]))
    }
    return(m)
  }
  demean_matrix = function(X){
    for(i in 1:ncol(X)){
      X[,i] = X[,i]-mean(X[,i])
    }
    return(X)
  }
  
  m = mean_matrix(X)
  X = demean_matrix(X)
  
  if(is.null(Sigma)){
    Sigma = (1/(nrow(X)-1))* (t(X) %*% X)
  }
  
  
  S_inv = solve(Sigma)
  e = rep(1,3)
  
  
  w = S_inv %*% e / drop(t(e) %*% S_inv %*% e)
  
  return(w)
  
}



Optimal_Portfolio_Desired = function(X, mu_b, Sigma = NULL){
  #browser()
  mean_matrix = function(X){
    m = c()
    for (i in 1:ncol(X)){
      m = c(m, mean(X[,i]))
    }
    return(m)
  }
  demean_matrix = function(X){
    for(i in 1:ncol(X)){
      X[,i] = X[,i]-mean(X[,i])
    }
    return(X)
  }
  
  m = mean_matrix(X)
  X = demean_matrix(X)
  
  if(is.null(Sigma)){
    Sigma = (1/(nrow(X)-1))* (t(X) %*% X)
  }
  
  
  S_inv = solve(Sigma)
  e = rep(1,3)
  
  a = t(m) %*% S_inv %*% m
  b = t(m) %*% S_inv %*% e
  c = t(e) %*% S_inv %*% m
  d = t(e) %*% S_inv %*% e
  
  l1 = (t((m - mu_b*e))%*% S_inv %*% m)/(a*d - b^2)
  l2 = (-t((m - mu_b*e))%*% S_inv %*% e)/(a*d - b^2)
  
  w = S_inv %*% (drop(l1)*e + drop(l2)*m)
  
  return(w)
  
}
Optimal_Portfolio_Find = function(X, lambda, Sigma = NULL){
  #browser()
  mean_matrix = function(X){
    m = c()
    for (i in 1:ncol(X)){
      m = c(m, mean(X[,i]))
    }
    return(m)
  }
  demean_matrix = function(X){
    for(i in 1:ncol(X)){
      X[,i] = X[,i]-mean(X[,i])
    }
    return(X)
  }
  
  m = mean_matrix(X)
  X = demean_matrix(X)
  
  if(is.null(Sigma)){
    Sigma = (1/(nrow(X)-1))* (t(X) %*% X)
  }
  
  
  S_inv = solve(Sigma)
  e = rep(1,3)
  
  #Minimum Variance Portfolio
  w_min =  (1/drop(t(e) %*% S_inv %*% e))*S_inv %*% e 
  mu_min = m %*% w_min
  
  w_m = (1/drop(t(e) %*% S_inv %*% m))*S_inv %*% m
  mu_m = m %*% w_m
  
  alpha = drop(lambda*(t(m) %*% S_inv %*% e))
  
  w = (1-alpha)*w_min + alpha*w_m
  return(w)
}
Efficient_Frontier = function(X,x, Sigma = NULL, short = T){
  
  mean_matrix = function(X){
    m = c()
    for (i in 1:ncol(X)){
      m = c(m, mean(X[,i]))
    }
    return(m)
  }
  
  demean_matrix = function(X){
    for(i in 1:ncol(X)){
      X[,i] = X[,i]-mean(X[,i])
    }
    return(X)
  }
  
  m = mean_matrix(X)
  
  X = demean_matrix(X)
  
  if(is.null(Sigma)){
    Sigma = (1/(nrow(X)-1))* (t(X) %*% X)
  }
  
  S_inv = solve(Sigma)
  e = rep(1, ncol(X))
  
  alpha = drop(t(e) %*% S_inv %*% e)
  beta = drop(t(e) %*% S_inv %*% m)
  gamma = drop(t(m) %*% S_inv %*% m)
  delta = alpha*gamma - beta^2
  
  
  upper = beta / alpha + sqrt((beta / alpha) ^ 2 - (gamma - delta * x ^ 2) / (alpha))
  lower = beta / alpha - sqrt((beta / alpha) ^ 2 - (gamma - delta * x ^ 2) / (alpha))
  
  return(list(upper,lower))
}


m = apply(X = X,MARGIN = 2,FUN = mean)
S = (1/nrow(X))*(t(X-m) %*% (X-m))


mu_b = seq(from = -0.01, to = 0.052, by = 0.0001)
weights = lapply(mu_b,FUN = function(x){Optimal_Portfolio_Desired(X = X,mu_b = x)})
sigma = lapply(weights, function(x){sqrt(t(x) %*% S %*% x)}) %>% unlist()


SP500_m = m[1]
Gold_m = m[2]
Oil_m = m[3]

SP500_s = sqrt(S[1,1])
Gold_s = sqrt(S[2,2])
Oil_s = sqrt(S[3,3])

w_mv = Min_Variance_Port(X)
mv_m = t(w_mv) %*% m 
mv_s = sqrt(t(w_mv) %*% S %*% w_mv)



M = c(mu_b,SP500_m,Gold_m,Oil_m,mv_m)
V = c(sigma,SP500_s,Gold_s,Oil_s,mv_s )
L = c(rep("Efficient Frontier",length(mu_b)), "SP500", "Gold","Oil","Minimum Variance")



dataframe = data.frame(M,V,L)
names(dataframe) = c("Mean","Sigma","Portfolio")
dataframe %>% mutate(Upper = Mean >= rep(mv_m,length(mu_b))) -> dataframe

c(1,2) %in% c(2,3)

plot(dataframe[,2],dataframe[,1], type = "l")

min_var

dataframe %>% filter(Portfolio %in% c("Efficient Frontier")) %>% ggplot(aes(x = Sigma, y = Mean, group = Upper)) + 
  geom_line(stat = "Identity") + 
  geom_point(data = dataframe %>% filter(!(Portfolio %in% c("Efficient Frontier"))), aes(x = Sigma, y = Mean, color = Portfolio),size = 4) +
  scale_color_manual(values = c("#CCCC00","#f44141","#333333","#619CFF")) + ggtitle("Efficient Frontier") + xlab(expression(sigma)) +
  ylab(expression(mu))
  
