corrected_statistic = function(object, alpha = 0.05){
  #browser()
  n = length(unlist(object$Diff))
  WLR = mean(unlist(object$Diff))
  HAC = mean(unlist(object$Diff)^2)
  
  t_new = WLR/(sqrt(HAC)/sqrt(n))
  t = object$Statistic
  print(t_new - t)
  d = unlist(object$Diff)
  
  object$Statistic = t_new
  t = t_new
  
  reject = abs(t) > qnorm(1-(alpha/2)) 
  
  object$Result = "Not Statistically Different"
  
  if(reject & (sign(t) == 1)){
    object$Result = "f is the best density"
  }
  if(reject & (sign(t) == -1)){
    object$Result = "g is the best density"
  }
  
  object$`p-value` = 2*(1 - pnorm(abs(t)))
  object[["Cumulative Sum"]] = cumsum(d)
  return(object)
}

corrected_statistic(BEKK_uGARCH_2012_CL) -> test

sum(unlist(test$Diff) != 0)

print.WLR = function(object, draw = T){
  if(draw){
    plot(object$`Cumulative Sum`, type = "l")
  }
  print("### WLR TEST ###")
  print("p-value:")
  print(object$`p-value`)
  cat("\n")
  print("Cumulative Sum")
  print(object$`Cumulative Sum`[length(object$`Cumulative Sum`)])
}


#Correction for NaN
BEKK_DCC_2006_CL$Diff[[590]] = BEKK_uGARCH_2006_CL$Diff[[590]] - DCC_uGARCH_2006_CL$Diff[[590]]


BEKK_DCC_2006_CL %>% corrected_statistic() %>% print
BEKK_uGARCH_2006_CL %>% corrected_statistic() %>% print
BENCH_BEKK_2006_CL %>% corrected_statistic() %>% print
BENCH_DCC_2006_CL %>% corrected_statistic() %>% print
BENCH_uGARCH_2006_CL %>% corrected_statistic() %>% print
DCC_uGARCH_2006_CL %>% corrected_statistic() %>% print

#Correction for NaN 
BEKK_DCC_2006_CL$Diff[[590]] = BEKK_uGARCH_2006_CL$Diff[[590]] - DCC_uGARCH_2006_CL$Diff[[590]]
