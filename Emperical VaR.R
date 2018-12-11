setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(tidyverse, mgarchBEKK, rugarch, tictoc,rmgarch,parallel)

#Functions ---------------------------------------------------------------------------------------

#Finding minimum variance portfolio

Min_Variance_Port = function(X, Sigma = NULL){

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

MV_Port_Returns_OS = function(IS, OS){
  
  n = nrow(OS)
  m = nrow(IS)
  
  weights = list()
  
  AllData = rbind(IS, OS)
  
  PortReturnsOS = rep(0, n)
  
  for (i in 1:n) {
    
    CurrentData = AllData[i:(m+i-1),]
    
    w = Min_Variance_Port(CurrentData)
    
    weights[[i]] = w
    
    PortReturnsOS[i] = OS[i,] %*% w
    
  }
  
  return(list(Returns = PortReturnsOS,
              ws = weights))
}

#Computing empirical non parametric VaR

NonParaVaR = function(series, alpha = 0.05){
  n = length(series)
  
  K = round(n*alpha)
  
  series = sort(series , decreasing = F)
  
  VaR = series[K]
  
  return(VaR)
}

#NonParaVaR(PortReturnsOS)

NonParaEmpericalVarChecker = function(IS, OS , alpha = 0.05){
  n = nrow(OS)
  m = nrow(IS)
  
  AllData = rbind(IS, OS)
  
  VaR = rep(0, n)
  ExceedCount = rep(0, n)
  
  for (i in 1:n) {
    
    CurrentData = AllData[i:(m+i-1),]
    
    w = Min_Variance_Port(CurrentData)
    
    CurrentPortfolio = CurrentData %*% w
    
    VaR[i] = NonParaVaR(CurrentPortfolio , alpha = alpha)
    
    if( OS[i,] %*% w < VaR[i]){ExceedCount[i] = 1}
    
  }
  
  Exceedance = mean(ExceedCount)
  
  return(list(VaR = VaR, 
              Exceedance = Exceedance))
  
}

#Compute empirical VaR using a list

MatrixVar = function(Matrix, w = w, alpha = alpha){
  
  VaR = qnorm(alpha)*sqrt(t(w) %*% Matrix %*% w)
  
  return(VaR)
  
}

ListEmpericalVarChecker = function( IS , OS , List , alpha = 0.05){

  n = nrow(OS)
  m = nrow(IS)
  
  AllData = rbind(IS, OS)
  
  VaR = rep(0, n)
  ExceedCount = rep(0, n)
  
  for (i in 1:n) {
    
    CurrentData = AllData[i:(m+i-1),]
    
    w = Min_Variance_Port(CurrentData)
    
    VaR[i] = MatrixVar(List[[i]] , w = w , alpha = alpha)
    
    if(OS[i,] %*% w < VaR[i]){ExceedCount[i] = 1}
    
  }
  
  Exceedance = mean(ExceedCount)
  
  return(list(VaR = VaR, 
              Exceedance = Exceedance))
}

#2006 ---------------------------------------------------------

load("./DATA/Workspace2006.Rdata")


load("./Forecasts_2006/BEKK_forecasts_2006_BFGS.Rdata")
load("./Forecasts_2006/DCC_forecasts_2006.Rdata")
load("./Forecasts_2006/uGARCH_forecasts_2006.Rdata")
load("./Forecasts_2006/Benchmark_forecasts_2006.Rdata")


alpha_test = 0.05

NonPara_EV = NonParaEmpericalVarChecker(IS = IS, OS = OS , alpha = alpha_test);NonPara_EV[[2]]*100
Bench_EV = ListEmpericalVarChecker(IS = IS, OS = OS, List = bench_2006 , alpha = alpha_test);Bench_EV[[2]]*100
uGARCH_EV = ListEmpericalVarChecker(IS = IS, OS = OS , List = H_g, alpha = alpha_test);uGARCH_EV[[2]]*100
BEKK_EV = ListEmpericalVarChecker(IS = IS, OS = OS , List = H_bekk , alpha = alpha_test);BEKK_EV[[2]]*100
DCC_EV = ListEmpericalVarChecker(IS = IS, OS = OS , List = H_dcc, alpha = alpha_test);DCC_EV[[2]]*100

PortReturnsOS = MV_Port_Returns_OS(IS = IS, OS = OS)  %>% .$Returns

plot(PortReturnsOS , type = "l" )
lines(uGARCH_EV$VaR , col = "red")
lines(BEKK_EV$VaR , col = "blue")
lines(DCC_EV$VaR , col = "green")
lines(NonPara_EV$VaR , col = "yellow")
lines(Bench_EV$VaR , col = "blue")

VaR_DF = data.frame(cbind(Return_DF_OS$Date, PortReturnsOS,NonPara_EV[[1]] , Bench_EV[[1]], uGARCH_EV[[1]], BEKK_EV[[1]], DCC_EV[[1]]))
names(VaR_DF) = c("Date","Returns", "NP", "Bench", "uGARCH","BEKK","DCC")

VaR_DF %>% select("Date","Returns","Bench") %>% 
  gather(key = "VaR", value = "Value", -Date) %>% 
  ggplot(aes(x = as.Date(Date , origin = "1970-01-01"), y = Value, color = VaR, group = rev(VaR) )) + 
  geom_line() + scale_color_manual(values = c("#f00000", "#000000")) +
  theme(legend.title=element_blank()) + xlab("Date") + ylab("Returns / VaR") -> p2;p2

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(p1,p2,p3,p4,p5 , cols = 1)

# 2012 --------------------------------------------------------------------------------------------
load("./DATA/Workspace2012.Rdata")
load("./Forecasts_2012/BEKK_forecasts_2012_BFGS.Rdata")#; H_BEKK = mod; remove(mod)
load("./Forecasts_2012/DCC_forecasts_2012.Rdata")
load("./Forecasts_2012/uGARCH_forecasts_2012.Rdata")
load("./Forecasts_2012/Benchmark_forecasts_2012.Rdata")

alpha_test = 0.05

NonPara_EV = NonParaEmpericalVarChecker(IS = IS, OS = OS, alpha = alpha_test);NonPara_EV[[2]]*100
Bench_EV = ListEmpericalVarChecker(IS = IS, OS = OS, List = bench_2012 , alpha = alpha_test);Bench_EV[[2]]*100
uGARCH_EV = ListEmpericalVarChecker(IS = IS, OS = OS, List = H_g, alpha = alpha_test);uGARCH_EV[[2]]*100
BEKK_EV = ListEmpericalVarChecker(IS = IS, OS = OS, List = H_bekk , alpha = alpha_test);BEKK_EV[[2]]*100
DCC_EV = ListEmpericalVarChecker(IS = IS, OS = OS, List = H_dcc, alpha = alpha_test);DCC_EV[[2]]*100

PortReturnsOS = MV_Port_Returns_OS(IS = IS, OS = OS) %>% .$Returns

plot(PortReturnsOS , type = "l" )
lines(uGARCH_EV$VaR , col = "red")
lines(BEKK_EV$VaR , col = "blue")
lines(DCC_EV$VaR , col = "green")
lines(NonPara_EV$VaR , col = "yellow")
lines(Bench_EV$VaR , col = "blue")

VaR_DF = data.frame(cbind(Return_DF_OS$Date, PortReturnsOS,NonPara_EV[[1]] , Bench_EV[[1]], uGARCH_EV[[1]], BEKK_EV[[1]], DCC_EV[[1]]))
names(VaR_DF) = c("Date","Returns", "NP", "Bench", "uGARCH","BEKK","DCC")

VaR_DF %>% select("Date","Returns","uGARCH") %>% 
  gather(key = "VaR", value = "Value", -Date) %>% 
  ggplot(aes(x = as.Date(Date , origin = "1970-01-01"), y = Value, color = VaR, group = VaR )) + 
  geom_line(size = 0.6) + scale_color_manual(values = c("#000000", "#ff0000")) +
  theme(legend.title=element_blank()) + xlab("Date") + ylab("Returns / VaR") -> p3;p3

multiplot(p1,p2,p3,p4,p5 , cols = 1)
