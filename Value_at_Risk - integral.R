library(cubature);library(mvtnorm)

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
R = function(x,R_m){
  return(as.vector(R_m %*% x))
}
int_f = function(tau, f, inf = 10,d = 3, err = 1e-2){
  return(adaptIntegrate(f,lowerLimit = rep(-inf,d), upperLimit = c(rep(inf,d-1),tau),absError = err))
}

VaR = function(w, mu, Sigma, distribution = "normal", alpha = 0.05){
  #browser()
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
  R = function(x,M){
    return(as.vector(M %*% x))
  }
  M = gram_schmidt(w)
  class(distribution) = distribution
  rotate_distribution = function(dist){
    UseMethod("rotate_distribution")
  }
  rotate_distribution.normal = function(dist){
    f = function(x){
      x = R(x,M)
      y = mvtnorm::dmvnorm(x = x, mean = mu, sigma = Sigma)
      return(y)
    }
  }
  rotate_distribution.t = function(dist){
    f = function(x){
      x = R(x,M)
      v = 
      del = (mu/sqrt(v/2))*(gamma(v/2)/gamma((v-1)/2))
      y = mvtnorm::dmvt(x = x,delta = mu, sigma = ((v-2)/v)*Sigma, df = v,log = F)
      return(y)
    }
    return(f)
  }
  
  f = rotate_distribution(distribution)
  
  int_f = function(tau, f, inf = 10,d = length(w), err = 1e-2){
    return(adaptIntegrate(f,lowerLimit = rep(-inf,d), upperLimit = c(rep(inf,d-1),tau),absError = err))
  }
  
  tau_a = drop(crossprod(w,mu))
  tau_b = drop(crossprod(w,mu) - 10*crossprod(w,Sigma %*% w))
  
  f_a = int_f(tau_a,f)
  f_b = int_f(tau_b,f)
  
  F_a = f_a$integral - alpha
  F_b = f_b$integral - alpha
  
  if (sign(F_a) == sign(F_b)){
    tau_b = tau_b - 10
    f_b = int_f(tau_b,f)
    F_b = f_b$integral - alpha
  }
  
  tau_c = mean(c(tau_a,tau_b))
  f_c = int_f(tau_c,f)
  F_c = f_c$integral - alpha
  
  i = 1
  while(abs(F_c)>0.001){
    if(sign(F_c) == sign(F_a)){
      tau_a = tau_c
      F_a = F_c
    }else{
      tau_b = tau_c
      F_b = F_c
    }
    
    tau_c = mean(c(tau_a,tau_b))
    f_c = int_f(tau_c,f,err = max(1e-6,1e-3*(abs(F_c-0.05))))
    F_c = f_c$integral - alpha
    i = i+1 
    if(i>100){
      break
    }
  }
  
  return(tau_c * norm(w,"2"))
}

VaR(w,m,S, alpha = 0.3)


