WLR.test = function(data,density1,
                    density2,alpha = 0.05){
  #browser()
  n <- length(data[,1]);m <- length(data[1,])
  
  Y <- as.matrix(data)

  WLR <- (log(density1(Y))-log(density2(Y)))

  WLR.bar <- sum(WLR)/n
  hacsigma <- sqrt( sum(WLR^2)/n )
  
  t <- WLR.bar*sqrt(n)/(hacsigma)
  p <- pnorm(t)
  best = "Not significally different"
  if(p<alpha/2){
    best = "Density 2"
  }
  if(p>1-alpha/2){
    best = "Density 1"
  }
  p.value <- 2*min(c(1-pnorm(t),pnorm(t)))
  
  return(list(P_value = p.value,
              Statistic = t, 
              Best_density = best ))
}
