setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("./DATA/DataAndReturnFct_FullSample.R")

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

Date = Gold$Date

Close = as.tibble(cbind(SP500$Close, Gold$Value, Oil$Value)) %>% mutate(Date = as.Date(Date)) 
Close = Close[,c(4,1,2,3)]; names(Close) = c("Date","SP500","Gold","Oil")

#Make tibble Include Returns
Close %>% returns(series = c("SP500","Gold","Oil")) %>% mutate(Date = as.Date(Date)) -> Close_Returns

#Tibble in long format for plotting
Close %>% mutate(Date = as.Date(Date)) %>% gather(key = "Stock", value = "Close",-Date) -> Close_Long

#Close Returns in long format for plotting with seperation for each symbol
Close %>% returns(series = c("SP500","Gold","Oil")) %>% mutate(Date = as.Date(Date)) %>% 
  gather(key = "Stock", value = "Value",-Date) %>% mutate(Color = case_when(
    grepl(pattern = "Gold", Stock) ~ "Gold",
    grepl(pattern = "Oil", Stock) ~ "Oil",
    grepl(pattern = "SP500", Stock) ~ "SP500"
  )) -> Close_Returns_Long

names(Close_Returns) = c("Date","SP500", "Gold", "Oil", "SP500 Returns", "Gold Returns" , "Oil Returns")

#Making a plot with series and returns
Close_Returns %>% 
  gather(key = "Stock", value = "Value", -Date) %>% 
  mutate(Color = case_when(grepl(pattern = "Gold", Stock) ~ "Gold",
                           grepl(pattern = "Oil", Stock) ~ "Oil",
                           grepl(pattern = "SP500", Stock) ~ "SP500")) %>% 
  ggplot(aes(x = Date, y = Value, color = Color, group = Stock)) + 
  theme(legend.position="none") +
  geom_line() + scale_color_manual(values = c("#CCCC00", "#333333","#619CFF")) + 
  facet_wrap(~as_factor(Stock, levels = c("SP500", "Gold", "Oil", "SP500 Returns", "Gold Returns" , "Oil Returns")), ncol = 3, scales = "free_y")

#Making plot with histogram and QQ-plots

Close_Returns %>% 
  gather(key = "Stock", value = "Value", -Date) %>% 
  mutate(Color = case_when(grepl(pattern = "Gold", Stock) ~ "Gold",
                           grepl(pattern = "Oil", Stock) ~ "Oil",
                           grepl(pattern = "SP500", Stock) ~ "SP500")) %>%
  filter(Stock %in% c("SP500 Returns", "Gold Returns" , "Oil Returns")) -> QQ_frame

Hist_SP500 = QQ_frame %>% filter(Color %in% c("SP500")) %>% mutate(Title = "Histogram of SP500 Returns") %>% {ggplot(data = ., aes(x = Value, fill = Stock)) + geom_histogram(aes(y = ..density..), bins = 60,col = "#000000", fill = "#619CFF") + 
    stat_function(fun = dnorm, args = list(mean = mean(.$Value), sd = sd(.$Value))) + labs(x = "Returns", y = "Density") +facet_grid(. ~ Title)}

Hist_Gold =QQ_frame %>% filter(Color %in% c("Gold")) %>% mutate(Title = "Histogram of Gold Returns") %>% {ggplot(data = ., aes(x = Value, fill = Stock)) + geom_histogram(aes(y = ..density..), bins = 60,col = "#000000", fill = "#CCCC00") + 
    stat_function(fun = dnorm, args = list(mean = mean(.$Value), sd = sd(.$Value))) + labs(x = "Returns", y = "Density") +facet_grid(. ~ Title)}

Hist_Oil = QQ_frame %>% filter(Color %in% c("Oil")) %>% mutate(Title = "Histogram of Oil Returns") %>% {ggplot(data = ., aes(x = Value, fill = Stock)) + geom_histogram(aes(y = ..density..), bins = 60,col = "#000000", fill = "#333333") + 
    stat_function(fun = dnorm, args = list(mean = mean(.$Value), sd = sd(.$Value))) + labs(x = "Returns", y = "Density") +facet_grid(. ~ Title)}

QQ_SP500 = QQ_frame %>% filter(Color %in% c("SP500")) %>% na.omit() %>%
  mutate(Title = "QQ-plot of SP500 Returns") %>% 
  {ggplot(data = ., aes(sample = Value)) + stat_qq(color = "#619CFF") + ggplot2::stat_qq_line() + 
      facet_grid(. ~ Title) + labs(x = "Theoretical", y = "Sample")
  }

QQ_Gold = QQ_frame %>% filter(Color %in% c("Gold")) %>% na.omit() %>%
  mutate(Title = "QQ-plot of Gold Returns") %>% 
  {ggplot(data = ., aes(sample = Value)) + stat_qq(color = "#CCCC00") + ggplot2::stat_qq_line() + 
      facet_grid(. ~ Title) + labs(x = "Theoretical", y = "Sample")
  }

QQ_Oil = QQ_frame %>% filter(Color %in% c("Oil")) %>% na.omit() %>%
  mutate(Title = "QQ-plot of Oil Returns") %>% 
  {ggplot(data = ., aes(sample = Value)) + stat_qq(color = "#333333") + ggplot2::stat_qq_line() + 
      facet_grid(. ~ Title) + labs(x = "Theoretical", y = "Sample")
  }

multiplot(Hist_SP500, QQ_SP500,  Hist_Gold, QQ_Gold, Hist_Oil , QQ_Oil , cols = 3)


#Making ACF plots
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
  
  plot_df %>% mutate(Type = as_factor(Type)) -> plot_df
  
  plot_df %>% ggplot(aes(x = Lag, y = ACF, color = Name, fill = Name)) + geom_bar(stat = "identity", position = "identity", color = "black") +
    scale_fill_manual(name = NULL, values = c("#CCCC00", "#333333","#619CFF")) +
    geom_hline(yintercept = 2/sqrt(N),linetype = "dashed")  +
    theme(legend.position="none") +
    geom_hline(yintercept = -2/sqrt(N),linetype = "dashed") + facet_wrap(~Type) -> P
  P
  return(list("Data" = plot_df, "Plot" = P))
}

Close_Returns %>% ggacf(series = c( "SP500 Returns", "Gold Returns" ,"Oil Returns" ))


# Statistical properties: ----
returns(SP500 ,series = "Close" , demean = F) %>% .$Returns_Close %>% mean()
var(SP500_returns)
e1071::skewness(SP500_returns)
e1071::kurtosis(SP500_returns , type = 1) + 3

returns(Gold ,series = "Value" , demean = F) %>% .$Returns_Value %>% mean()
var(Gold_returns)
e1071::skewness(Gold_returns)
e1071::kurtosis(Gold_returns , type = 1) + 3

returns(rbind(Oil,Oil_OOS) ,series = "Value" , demean = F) %>% .$Returns_Value %>% mean()
var(Oil_returns)
e1071::skewness(Oil_returns)
e1071::kurtosis(Oil_returns , type = 1) + 3
