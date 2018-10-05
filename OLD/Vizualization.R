library('tidyverse')
library('rugarch')
library('lubridate')
library('qqplotr')
SP500 = read.csv("./RAWDATA/^GSPC.csv", stringsAsFactors=FALSE)
AMD = read.csv("./RAWDATA/AMD.csv", stringsAsFactors=FALSE)
NVIDIA = read.csv("./RAWDATA/NVDA.csv", stringsAsFactors=FALSE)


#Function for Calculating Returns
returns = function(data, series = NULL, logreturns = T, Date = T){
  if(is.vector(data)){
    if(logreturns){
      return(c(NA,log(data[2:length(data)]) - log(data[2:length(data)-1]))) 
    }else{
      return(c(NA,(data[2:length(data)]-data[2:length(data)-1])/data[2:length(data)-1]))
    }
  }
  
  if(!is.tibble(data)|!is.data.frame(data)){
    data = as.tibble(data)
  }
  name_data = names(data)
  if(is.null(series)){
    idx = 2
    name_vector = name_data[idx]
  }else{
    idx = series 
    if(is.numeric(series)){
      name_vector = name_data[idx]
    }else{
      name_vector = series
    }
  }
  
  placeholder = as.matrix(data[,idx])
  n = dim(placeholder)[1]
  
  name_returns = c()
  for(name in name_vector){
    name_returns = c(name_returns, paste("Returns_",name, sep = ""))
  }
  #browser()
  
  if(logreturns){
    return_matrix = log(placeholder[2:n,]) - log(placeholder[2:n - 1,])
    result = as.tibble(cbind(as.matrix(data[2:n,]),return_matrix))
    names(result) = c(name_data,name_returns)
  }else{
    return_matrix = (placeholder[2:n,] - placeholder[2:n - 1,])/placeholder[2:n - 1,]
    result = as.tibble(cbind(as.matrix(data[2:n,]),return_matrix))
    names(result) = c(name_data,name_returns)
  }
  result[,-1] = lapply(result[,-1],as.numeric)
  return(result)
}

#Make Close Dataframe
Date = AMD$Date
Close = as.tibble(cbind(SP500$Close, AMD$Close, NVIDIA$Close)) %>% mutate(Date = date(as.Date(Date))) 
Close = Close[,c(4,1,2,3)]; names(Close) = c("Date","SP500","AMD","NVIDIA")

#Make Close Include Returns
Close %>% returns(series = c("SP500","AMD","NVIDIA")) %>% mutate(Date = date(as.Date(Date))) -> Close_Returns

#Close in long format for plotting
Close %>% mutate(Date = date(as.Date(Date))) %>% gather(key = "Stock", value = "Close",-Date) -> Close_Long

#Close Returns in long format for plotting with seperation for each symbol
Close %>% returns(series = c("SP500","AMD","NVIDIA")) %>% mutate(Date = date(as.Date(Date))) %>% 
  gather(key = "Stock", value = "Value",-Date) %>% mutate(Color = case_when(
    grepl(pattern = "AMD", Stock) ~ "AMD",
    grepl(pattern = "NVIDIA", Stock) ~ "NVIDIA",
    grepl(pattern = "SP500", Stock) ~ "SP500"
  )) -> Close_Returns_Long

#Plot of Stock Symbol and it's return below
P_Stock_Return = Close_Returns_Long %>% ggplot(aes(x=Date, y = Value, color = Color)) + geom_line() + facet_wrap(~as_factor(Stock, levels = level_vec), nrow = 2, ncol = 3, scales = "free_y") 

Close_Returns %>% mutate(Returns_SP500_sq = Returns_SP500^2,
                         Returns_AMD_sq = Returns_AMD^2,
                         Returns_NVIDIA_sq = Returns_NVIDIA^2) %>% gather(key = "Stock", value = "Value",-Date) %>% mutate(Color = case_when(
                           grepl(pattern = "AMD", Stock) ~ "AMD",
                           grepl(pattern = "NVIDIA", Stock) ~ "NVIDIA",
                           grepl(pattern = "SP500", Stock) ~ "SP500"
                         )) %>% ggplot(aes(x=Date, y = Value, color = Color)) + geom_line() + facet_wrap(~as_factor(Stock, levels = c(level_vec,c("Retuns_AMD_sq","Retuns_NVIDIA_sq","Retuns_SP500_sq"))), nrow = 3, ncol = 3, scales = "free_y")

names(Close_Returns)[c(2,3,5,6)] = c("SP500","AMD","SP500 Returns", "AMD Returns")

Close_Returns %>% 
  select(-Returns_NVIDIA,-NVIDIA) %>% 
  gather(key = "Stock", value = "Value", -Date) %>% 
  mutate(Color = case_when(grepl(pattern = "AMD", Stock) ~ "AMD",
                           grepl(pattern = "NVIDIA", Stock) ~ "NVIDIA",
                           grepl(pattern = "SP500", Stock) ~ "SP500")) %>% 
  ggplot(aes(x = Date, y = Value, color = Color, group = Stock)) + 
  geom_line() + 
  facet_wrap(~as_factor(Stock, levels = c("SP500", "AMD", "SP500 Returns", "AMD Returns")), ncol = 2, scales = "free_y")
  
Close_Returns %>% 
  select(-Returns_NVIDIA,-NVIDIA) %>% 
  gather(key = "Stock", value = "Value", -Date) %>% 
  mutate(Color = case_when(grepl(pattern = "AMD", Stock) ~ "AMD",
                           grepl(pattern = "NVIDIA", Stock) ~ "NVIDIA",
                           grepl(pattern = "SP500", Stock) ~ "SP500")) %>%
  filter(Stock %in% c("SP500 Returns", "AMD Returns")) -> QQ_frame

hist_SP500 = QQ_frame %>% filter(Color %in% c("SP500")) %>% mutate(Title = "Histogram of SP500 Returns") %>% {ggplot(data = ., aes(x = Value, fill = Stock)) + geom_histogram(aes(y = ..density..), bins = 60,col = "#000000", fill = "#619CFF") + 
  stat_function(fun = dnorm, args = list(mean = mean(.$Value), sd = sd(.$Value))) + labs(x = "Returns", y = "Density") +facet_grid(. ~ Title)}
hist_AMD = QQ_frame %>% filter(Color %in% c("AMD")) %>% mutate(Title = "Histogram of AMD Returns") %>% {ggplot(data = ., aes(x = Value, fill = Stock)) + geom_histogram(aes(y = ..density..), bins = 60,col = "#000000", fill = "#F8766D") + 
    stat_function(fun = dnorm, args = list(mean = mean(.$Value), sd = sd(.$Value))) + labs(x = "Returns", y = "Density") +facet_grid(. ~ Title)}

QQ_SP500 = QQ_frame %>% filter(Color %in% c("SP500")) %>% na.omit() %>%
  mutate(Title = "QQ-plot of SP500 Returns") %>% 
  {ggplot(data = ., aes(sample = Value)) + stat_qq(color = "#619CFF") + ggplot2::stat_qq_line() + 
      facet_grid(. ~ Title) + labs(x = "Theoretical", y = "Sample")
}
QQ_AMD = QQ_frame %>% filter(Color %in% c("AMD")) %>% na.omit() %>% 
  mutate(Title = "QQ-plot of AMD Returns") %>% 
  {ggplot(data = ., aes(sample = Value)) + 
    stat_qq(color = "#F8766D") +
    ggplot2::stat_qq_line() + facet_grid(. ~ Title) + labs(x = "Theoretical", y = "Sample")
  }
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

multiplot(hist_SP500, QQ_SP500,  hist_AMD, QQ_AMD, cols = 2)

SP500$Close %>% tseries::adf.test()

plot(SP500$Close,type = "l")

tests = c(moments::jarque.test, moments::skewness, moments::kurtosis, tseries::adf.test)
QQ_frame %>% filter(Color %in% c("SP500")) %>% pull(Value) %>% {for (t in tests){print(t(.))}}
QQ_frame %>% filter(Color %in% c("AMD")) %>% pull(Value) %>% {for (t in tests){print(t(.))}}

QQ_frame %>% filter(Color %in% c("SP500")) %>% pull(Value) %>% (function(x)(abs(x))) %>% acf()

test = acf(Close$SP500)

ggacf = function(data, series, sq = T, abs = T){
  cols = 1 + sq + abs
  val = data %>% select(series)
  acf_n = acf(val)
  acf_abs = acf(abs(val))
  acf_sq = acf(val^2)
  
  v_n = c()
  v_abs = c()
  v_sq = c()
  v_full = c()
  v_name = c()
  v_type = c()
  v_lag = c()
  
  
  N = acf_sq$n.used
  n = length(series)
  for (i in 1:n){
    v_n = c(acf_n$acf[,i,i])
    v_abs = c(acf_abs$acf[,i,i])
    v_sq = c(acf_sq$acf[,i,i])
    v_full = c(v_full,v_n,v_abs,v_sq)
    v_lag = c(v_lag,1:nrow(acf_n$lag[,,1]),1:nrow(acf_abs$lag[,,1]),1:nrow(acf_sq$lag[,,1]))
    v_type = c(v_type, 
               rep(paste(series[i]),nrow(acf_n$lag[,,1])), 
               rep(paste(series[i],"Absolute", sep = " "),nrow(acf_abs$lag[,,1])), 
               rep(paste(series[i],"Squared", sep = " "),nrow(acf_sq$lag[,,1])))
    v_name = c(v_name, rep(series[i],nrow(acf_n$lag[,,1]) + nrow(acf_abs$lag[,,1])+nrow(acf_sq$lag[,,1])))
  }
  
  plot_df = tibble("Lag" = v_lag, "ACF" = v_full, "Name" = v_name, "Type" = v_type)
  
  plot_df %>% ggplot(aes(x = Lag, y = ACF, color = Name, fill = Name)) + geom_bar(stat = "identity", position = "identity", color = "black") + 
    geom_hline(yintercept = 2/sqrt(N),linetype = "dashed") +
    geom_hline(yintercept = -2/sqrt(N),linetype = "dashed") + facet_wrap(~Type) -> P
  P
  return(list("Data" = plot_df, "Plot" = P))
}

Close %>% select(c("SP500","AMD")) %>% acf -> test

Close_Returns %>% ggacf(series = c("SP500 Returns", "AMD Returns"))


test$acf[,2,2]

QQ_frame %>% ggacf(series = "Value")
