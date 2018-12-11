library('gtools')

Weighted_LR = function(Y,f,g,S,alpha = 0.05,debug = F){
  if(debug){
    browser()
  }
  #Format data
  Y = as.matrix(Y)
  n = nrow(Y)
  
  #calculate D 
  d = S(f(Y)) - S(g(Y))
  
  #Calculate test sizes
  WLR = sum(d)/n 
  HAC = sum(d^2)/n
  t = WLR/(sqrt(HAC)/sqrt(n))
  
  #Truth value of test
  reject = (abs(t) > qnorm(alpha/2))
  
  result = list("Statistic" = t, "Result" = "Not Statistically Different")
  
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
print.WLR = function(WLR){
  cat("##### Results #####\n \n")
  cat(paste(WLR$Result,"\n \n",sep = " "))
  cat(paste("Statistic:", round(WLR$Statistic,4),"\n \n",sep = " "))
  cat(paste("p-value:", round(WLR$`p-value`,4),"\n \n",sep = " "))
  cat("###################")
}
Y = cbind(rnorm(100),rnorm(100))
f = function(x){mvtnorm::dmvnorm(x)}
g = function(x){mvtnorm::dmvt(x, df = 3)}
S = function(x){log(x)}

Weighted_LR(X,f,g,S)






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

permutations(n = 3)

stratified_integral = function(f, lower, upper){
  browser()
  #Construct Sub Boxes
  m = length(lower)
  Sub_Interval = list()
  for(i in 1:m){
    Sub_Interval[[i]] = list()
    sequence = seq(from = lower[i], to = upper[i], length.out = 11)
    for(j in 1:10){
      Sub_Interval[[i]][[j]] = list("lower" = sequence[j], upper = sequence[j+1])
    }
  }
  
  P = permutations(n = 10, m,repeats.allowed = T)
  f_u = c()
  F_u = 0
  h = nrow(P)
  for(i in 1:h){
    lower_i = c()
    upper_i = c()
    U = matrix(0, nrow = 2*h, ncol = m)
    
    for(j in 1:m){
      U[,j] = runif(h,min = Sub_Interval[[j]][[P[i,j]]]$lower, max = Sub_Interval[[j]][[P[i,j]]]$upper)
    }
    
    f_u = c(f_u, f(U))
    F_u = F_u + sum(f(U))
  }
    
  V = prod(upper-lower)
  
  i = 2*h^2
  
  I = (V/i)*F_u
  
  
  
  
  
  
  
  dummy = 0
  
  
  
}

f = function(x){dnorm(x)}

stratified_integral(f,c(-1.96),c(1.96))

qnorm(0.025)


permutations(n = 10, r = 3, v = 1:10, repeats.allowed = T)









## Testing Area
Y_1 = rnorm(1000)
Y_2 = rt(1000)
Y = c(Y_1,Y_2); Y = as.matrix(Y[sample(1:length(Y))])
f = function(x){dnorm(x)}
g = function(x){dt(x,3)}
S = function(x){log(x)}




