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


S_true = cbind(c(2,0.8),c(0.8,1.5))
set.seed(711)
X = mvtnorm::rmvnorm(2500,mean = c(0,0), sigma = S_true)

S = S_true #+ cbind(c(0,0.3),c(0.3,0))
S_0 = cbind(c(var(X[,1])+0.5,0),c(0,var(X[,2])+0.5))

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


level =  drop(t(c(2.2504,0)) %*% solve(S_true) %*% c(2.2504,0)) 
level_true = qchisq(p = 0.8, df = 2)

x_level = c(2.2504,0) 

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

f_sample_matrix(t(as.matrix(X_new[3,])))

S_cl = function(X,f,g,I_f, I_g, f_t = f_true_matrix, K = K_level){
  #browser()
  d = list()
  Indicator = f_t(X) <= K
  for(i in 1:nrow(X)){
    if(Indicator[i]){
      s_f = log(f(t(as.matrix(X[i,])))/I_f)
      s_g = log(g(t(as.matrix(X[i,])))/I_g)
      d[[i]] = s_f - s_g
    }else{
      d[[i]] = 0
    }
  }
  return(d)
}

S_csl = function(X,f,g,I_f, I_g, f_t = f_true_matrix, K = K_level){
  #browser()
  d = list()
  Indicator = f_t(X) <= K
  for(i in 1:nrow(X)){
    if(Indicator[i]){
      s_f = log(f(t(as.matrix(X[i,]))))
      s_g = log(g(t(as.matrix(X[i,]))))
      d[[i]] = s_f - s_g
    }else{
      s_f = log(1-I_f)
      s_g = log(1-I_g)
      d[[i]] = s_f - s_g
    }
  }
  return(d)
}

S_l = function(X,f,g){
  #browser()
  f_inner = f 
  g_inner = g
  d = list()
  for(i in 1:nrow(X)){
    s_f = log(f_inner(t(X[i,])))
    s_g = log(g_inner(t(X[i,])))
    d[[i]] = s_f - s_g
  }
  return(d)
}

result_1 = S_l(X,f_sample_matrix,f_0_matrix)
result_2 = S_csl(X,f_sample_matrix,f_0_matrix, I_sample,I_0)
result_3 = S_cl(X,f_sample_matrix,f_0_matrix, I_sample,I_0)

test_1 = cumsum(unlist(result_1))
test_2 = cumsum(unlist(result_2))
test_3 = cumsum(unlist(result_3))
test = data.frame(cbind(test_1,test_2,test_3,1:length(test_1)));names(test) = c("Logarithmic","Censored Likelihood" ,"Conditional Likelihood","Index")

test %>% gather(key = "Scoring Rule", value = "Cumulative Sum",-Index) -> gg_test

gg_test %>% ggplot(aes(x = Index, y = `Cumulative Sum`, color = `Scoring Rule`)) + geom_line()


plot(cumsum(unlist(result_2)), type = "l")
lines(cumsum((unlist(result_1))), col = "red")
f_0_matrix(t(X[1,]))


