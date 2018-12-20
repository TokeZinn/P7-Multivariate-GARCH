#### Region Bias #### 

wrapper = function(f){
  f_inner = f
  f_new = function(X){
    x = X[,1]
    y = X[,2]
    return(f_inner(x,y))
  }
  return(f_new)
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



#X = mvtnorm::rmvt(2000,sigma = cbind(c(1.4,0.4),c(0.4,1)), 8)


S_true = cbind(c(2,0.1),c(0.1,1.5))
X = mvtnorm::rmvnorm(2500,mean = c(0,0), sigma = S_true)

S = cov(X)
S_0 = cbind(c(var(X[,1])+0.3,0),c(0,var(X[,2])+0.3))

#f_true = function(x,y){
#  X = cbind(x,y)
#  return(mvtnorm::dmvt(X,sigma = S_true,df = 8,log = F))
#}

f_true = function(x,y){
  X = cbind(x,y)
  return(mvtnorm::dmvnorm(X,sigma = S_true))
}

f_sample = function(x,y){
  X = cbind(x,y)
  return(mvtnorm::dmvnorm(X,sigma = S))
}

f_0 = function(x,y){
  X = cbind(x,y)
  return(mvtnorm::dmvnorm(X,sigma = S_0))
}

f_0 %>% wrapper -> f_0_matrix
f_sample %>% wrapper -> f_sample_matrix
f_true %>% wrapper -> f_true_matrix


z_true = outer(X = seq(from = -4, to = 4, length.out = 1000),
               Y = seq(from = -4, to = 4, length.out = 1000),
               f_true)
z_sample = outer(X = seq(from = -4, to = 4, length.out = 1000),
                 Y = seq(from = -4, to = 4, length.out = 1000),
                 f_sample)
z_0 = outer(X = seq(from = -4, to = 4, length.out = 1000),
            Y = seq(from = -4, to = 4, length.out = 1000),
            f_0)


level =  drop(t(c(2.533,0)) %*% solve(S_true) %*% c(2.533,0)) 
level_true = qchisq(p = 0.8, df = 2)

x_level = c(2.533,0) 
points(X[diag(X %*% solve(S_true) %*% t(X)) >= level_true,],col = "red")
points(X[f_true_matrix(X) <= f_true_matrix(t(as.matrix(x_level))),],col = "blue")

K_level = f_true_matrix(t(as.matrix(x_level)))

integral_wrapper = function(f,f_true,K){
  f_inner = f
  f_inner_true = f_true
  f_new = function(X){
    return((f_inner_true(X)<= K)*f_inner(X))
  }
  return(f_new)
}

I_true = f_true_matrix %>% integral_wrapper(f_true_matrix,K_level) %>% integral(lower = rep(-5,2),upper = rep(5,2))
I_sample = f_sample_matrix %>% integral_wrapper(f_true_matrix,K_level) %>% integral(lower = rep(-5,2),upper = rep(5,2))
I_0 = f_0_matrix %>% integral_wrapper(f_true_matrix,K_level) %>% integral(lower = rep(-5,2),upper = rep(5,2))


X_new = mvtnorm::rmvnorm(2500,mean = c(0,0), sigma = S_true)

f_true_matrix(t(as.matrix(X_new[3,])))

S_cl = function(X,f,g,I_f, I_g, f_t = f_true_matrix, K = K_level){
  browser()
  d = list()
  Indicator = f_t(X) <= K
  for(i in 1:nrow(X)){
    if(Indicator[i]){
      s_f = log(f(t(as.matrix(X[i,])))/I_f)
      s_g = log(g(t(as.matrix(X[i,])))/I_f)
      d[[i]] = s_f - s_g
    }else{
      d[[i]] = 0
    }
  }
  return(d)
}

S = function(X,dens1,dens2){
  browser()
  d = list()
  for(i in 1:nrow(X)){
    s_f = log(dens1(t(as.matrix(X[i,]))))
    s_g = log(dens2(t(as.matrix(X[i,]))))
    d[[i]] = s_f - s_g
  }
  return(d)
}

result = S_cl(X_new,f_sample_matrix,f_0_matrix, I_sample,I_0)
result_2 = S(X_new,f_sample_matrix,f_0_matrix)

plot(cumsum(unlist(result_2)), type = "l")


