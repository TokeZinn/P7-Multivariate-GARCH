library(cubature)

w = Min_Variance_Port(X)
m = apply(X = X,MARGIN = 2,FUN = mean)
S = (1/nrow(X))*(t(X-m) %*% (X-m))


#Gram-Schmidt: 
gram_schmidt = function(w){
  W = diag(length(w)); W[,1] = w
  QR = qr(W)
  Q = qr.Q(QR)
  if(all(sign(Q[,1])!=sign(w))){
    Q[,1] = -Q[,1]
  }
  
  Q[,1:length(w)] = Q[,length(w):1]
  
  return(Q)
}

R_m = gram_schmidt(w)
R = function(x,R_m){
  return(as.vector(R_m %*% x))
}
f = function(dist, mu, Sigma, R_m){
  
  f_x = function(x){
    x = R(x,R_m) 
    dist(x, mean = mu, sigma = Sigma)
  }
  return(f_x)
}

f_R = f(dmvnorm,m,S,R_m)

int_f = function(tau, f, inf = 10,d = 3){
  return(adaptIntegrate(f,lowerLimit = rep(-inf,d), upperLimit = c(rep(inf,d-1),tau)))
}


int_f(-2.15,f_R)

VaR_int = -2.15*norm(w,"2")

s_hat = sqrt(drop(t(w) %*% S %*% w))
m_hat = t(w) %*% m

VaR_calc = drop(m_hat + qnorm(0.05)*s_hat)
