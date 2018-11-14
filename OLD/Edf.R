edf = function(x,X){
  browser()
  sorted = sort(X)
  n = length(X)
  
  s = 0 
  for (i in 1:n){
    s = s + (sorted[i] <= x)
  }
  
  s = s/(n+1)
  return(s)
}




test = rt(1000, 3)

edf_given = function(x){edf(x,test)}

curve(edf_given, from = -2, to = 2)

hist(edf_given(test))

hist(qnorm(edf_given(test)))

hist(qexp(edf_given(test)))

x = qnorm(edf_given(test))


tseries::jarque.bera.test(x[which(x != Inf)])
