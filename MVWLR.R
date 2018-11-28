WLR.test = function(data,H1,
                    H2,alpha = 0.05,Plot = F,Dates = NULL){
  #browser()
  n <- length(data[,1]);m <- length(data[1,])
  
  Y <- as.matrix(data)
  
  f <- function(x,H){
    d = c()
    for(num in 1:n){
      d[num] <- emdbook::dmvnorm(x[num,],mu = rep(0,3),Sigma = H[[num]])
    }
    return(d)
  }  
  
  density1 <- f(Y,H1)
  density2 <- f(Y,H2)
  
  
  WLR <- (log(density1)-log(density2))
  cumsum <- cumsum(WLR)
  
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
  
  if(Plot){
    if(is.null(Dates)){
      Dates <- 1:n
    }
    Data <- cbind(Dates,cumsum) %>% as.data.frame(); colnames(Data) <- c("Date","Cumulative_Log_Score_Diff")
    
    g <- ggplot(data = Data) + 
      geom_line(aes(x = Dates,y=Cumulative_Log_Score_Diff),size = 1) + 
      ylab("Cumulative Log score difference")#+ 
      #geom_line(aes(x=rs,y=Power$CSL_Power,colour = "CSL"),linetype="dashed",size = 1) +
      #scale_y_continuous(limits = c(0.8,1)) + xlab("r") + ylab("Rejection rate") +
      #scale_color_manual(values = c("#FF0000", "#282088")) +
      #theme(legend.title=element_blank()) + ggtitle("Cumulative log score difference") +
      #theme(legend.position="bottom")
  }
  
  
  return(list(P_value = p.value,
              Statistic = t, 
              Best_density = best,g ))
}
