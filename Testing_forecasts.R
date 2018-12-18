setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(tidyverse,emdbook,readxl)

source("MVWLR.R")
Benchmark = function(IS , OS ,dim = 3){
  #browser()
  IS = IS %>% as.data.frame() ; OS = OS %>% as.data.frame() ; names(OS) <- names(IS)
  All_Data = rbind(IS,OS) %>% as.matrix()
  
  n = length(IS[,1])
  m = length(OS[,1])
  
  OneSigma = list()
  
  for (i in 1:m) {
    Current_Data = All_Data[i:(n-1+i),]
    
    OneSigma[[i]] = var(Current_Data)
    
  }
  
  return(OneSigma)
  
}


## Unweighted 2006

load("./DATA/Workspace2006.Rdata")
# bench_2006 = Benchmark(IS,OS)
# save(bench_2006,file = "./Forecasts_2006/Benchmark_forecasts_2006.Rdata")
load("./Forecasts_2006/Benchmark_forecasts_2006.Rdata")
load("./Forecasts_2006/DCC_forecasts_2006.Rdata")
load("./Forecasts_2006/uGARCH_forecasts_2006.Rdata")
load("./Forecasts_2006/Bekk_forecasts_2006_BFGS.Rdata")

ggplot(data = Return_DF_OS,aes(x = Date,y = ))
test <- cbind(Dates_OS,OS) %>% as.data.frame()


dato <- Dates_OS %>% as.data.frame(); colnames(dato) <- c("Date")
cbind(dato,OS) %>% as.data.frame() %>%  gather(key = "Stock", value = "Value", -Date) %>% 
  mutate(Color = case_when(grepl(pattern = "Gold_OS", Stock) ~ "Gold",
                           grepl(pattern = "Oil_OS", Stock) ~ "Oil",
                           grepl(pattern = "SP500_OS", Stock) ~ "SP500")) %>% 
  ggplot(., aes(x = Date, y = Value, color = Color, group = Stock)) + 
  theme(legend.position="none") +
   geom_line() + scale_color_manual(values = c("#CCCC00", "#333333","#619CFF")) + 
   facet_wrap(~as_factor(Stock, levels = c("SP500", "Gold", "Oil", "SP500 Returns", "Gold Returns" , "Oil Returns")), ncol = 3, scales = "free_y")



test1 <- WLR.test(OS,H1 = H_g,bench_2006,Plot = T,Dates = Dates_OS, 
                  title = "uGARCH and Benchmark")
test2 <- WLR.test(OS,H1 = H_dcc,H2 = bench_2006,Plot = T,Dates = Dates_OS,
                  title = "DCC and Benchmark")
test3 <- WLR.test(OS,H1 = H_bekk,H2 = bench_2006,Plot = T,Dates = Dates_OS,
                       title = "Bekk and Benchmark")

test4 <- WLR.test(OS,H1 = H_dcc,H2 = H_g,Plot = T,Dates = Dates_OS,
                  title = "DCC and uGARCH")

test5 <- WLR.test(OS,H1 = H_bekk,H2 = H_g,Plot = T,Dates = Dates_OS,
                  title = "Bekk and uGARCH")

test6 <- WLR.test(OS,H1 = H_bekk,H2 = H_dcc,Plot = T,Dates = Dates_OS,
                  title = "Bekk and DCC")

source("Multiplot.R")
multiplot(test1[[4]],test2[[4]],test3[[4]],test4[[4]],test5[[4]],test6[[4]],
          cols = 3)

## Unweighted 2012

load("./DATA/Workspace2012.Rdata")
# bench_2012 = Benchmark(IS,OS)
# save(bench_2012,file = "./Forecasts_2012/Benchmark_forecasts_2012.Rdata")
load("./Forecasts_2012/Benchmark_forecasts_2012.Rdata")
load("./Forecasts_2012/DCC_forecasts_2012.Rdata")
load("./Forecasts_2012/uGARCH_forecasts_2012.Rdata")
load("./Forecasts_2012/Bekk_forecasts_2012_BFGS.Rdata")

dato <- Dates_OS %>% as.data.frame(); colnames(dato) <- c("Date")
cbind(dato,OS) %>% as.data.frame() %>%  gather(key = "Stock", value = "Value", -Date) %>% 
  mutate(Color = case_when(grepl(pattern = "Gold_OS", Stock) ~ "Gold",
                           grepl(pattern = "Oil_OS", Stock) ~ "Oil",
                           grepl(pattern = "SP500_OS", Stock) ~ "SP500")) %>% 
  ggplot(., aes(x = Date, y = Value, color = Color, group = Stock)) + 
  theme(legend.position="none") +
  geom_line() + scale_color_manual(values = c("#CCCC00", "#333333","#619CFF")) + 
  facet_wrap(~as_factor(Stock, levels = c("SP500", "Gold", "Oil", "SP500 Returns", "Gold Returns" , "Oil Returns")), ncol = 3, scales = "free_y")


test1 <- WLR.test(OS,H1 = H_g,bench_2012,Plot = T,Dates = Dates_OS, 
                  title = "uGARCH and Benchmark")
test2 <- WLR.test(OS,H1 = H_dcc,H2 = bench_2012,Plot = T,Dates = Dates_OS,
                  title = "DCC and Benchmark")
test3 <- WLR.test(OS,H1 = H_bekk,H2 = bench_2012,Plot = T,Dates = Dates_OS,
                  title = "Bekk and Benchmark")

test4 <- WLR.test(OS,H1 = H_dcc,H2 = H_g,Plot = T,Dates = Dates_OS,
                  title = "DCC and uGARCH")

test5 <- WLR.test(OS,H1 = H_bekk,H2 = H_g,Plot = T,Dates = Dates_OS,
                  title = "Bekk and uGARCH")

test6 <- WLR.test(OS,H1 = H_bekk,H2 = H_dcc,Plot = T,Dates = Dates_OS,
                  title = "Bekk and DCC")

source("Multiplot.R") 
multiplot(test1[[4]] + ylab("CLSD"),test2[[4]],test3[[4]],test4[[4]],test5[[4]],test6[[4]],
          cols = 3)



s1 <- unlist(H_dcc)[seq(4,9*756,by=9)]
s2 <- unlist(H_bekk)[seq(4,9*756,by=9)]

plot(s2,type = "l",col = "blue")
lines(s1,col = "red")



## CL 2006
load("./DATA/Workspace2006.Rdata")



test1 <- BENCH_uGARCH_2006_CL %>% corrected_statistic() 
test2 <- BENCH_DCC_2006_CL %>% corrected_statistic()
test3 <- BENCH_BEKK_2006_CL %>% corrected_statistic()
test4 <- DCC_uGARCH_2006_CL %>% corrected_statistic()
test5 <- BEKK_uGARCH_2006_CL %>% corrected_statistic()
BEKK_DCC_2006_CL %>% corrected_statistic() 




Data1 <- cbind(Dates_OS, BENCH_uGARCH_2006_CL %>% .$Diff %>% unlist() %>% cumsum()*(-1) ) %>% as.data.frame(); colnames(Data1) <- c("Date","CLCLD")

g1 = ggplot(Data1) + 
  geom_line(aes(x = Date %>% as.Date(origin = "1970-01-01"), y = CLCLD) ,size = 1) + 
  ylab("CCLCLD") + xlab(NULL) + facet_grid(.~"uGARCH and Benchmark");g1

Data2 <- cbind(Dates_OS, BENCH_DCC_2006_CL %>% .$Diff %>% unlist() %>% cumsum()*(-1) ) %>% as.data.frame(); colnames(Data2) <- c("Date","CLCLD")

g2 = ggplot(Data2) +
  geom_line(aes(x = Date %>% as.Date(origin = "1970-01-01"), y = CLCLD) ,size = 1) + 
  ylab("CLCSLD") + xlab(NULL) + facet_grid(.~"DCC and Benchmark");g2

Data3 <- cbind(Dates_OS, BENCH_BEKK_2006_CL %>% .$Diff %>% unlist() %>% cumsum()*(-1) ) %>% as.data.frame(); colnames(Data3) <- c("Date","CLCLD")

g3 = ggplot(Data3) +
  geom_line(aes(x = Date %>% as.Date(origin = "1970-01-01"), y = CLCLD) ,size = 1) + 
  ylab("CLCSLD") + xlab(NULL) + facet_grid(.~"BEKK and Benchmark");g3

Data4 <- cbind(Dates_OS, DCC_uGARCH_2006_CL %>% .$Diff %>% unlist() %>% cumsum() ) %>% as.data.frame(); colnames(Data4) <- c("Date","CLCLD")

g4 = ggplot(Data4) +
  geom_line(aes(x = Date %>% as.Date(origin = "1970-01-01"), y = CLCLD) ,size = 1) + 
  ylab("CLCSLD") + xlab(NULL) + facet_grid(.~"DCC and uGARCH");g4

Data5 <- cbind(Dates_OS, BEKK_uGARCH_2006_CL %>% .$Diff %>% unlist() %>% cumsum() ) %>% as.data.frame(); colnames(Data5) <- c("Date","CLCLD")

g5 = ggplot(Data5) +
  geom_line(aes(x = Date %>% as.Date(origin = "1970-01-01"), y = CLCLD) ,size = 1) + 
  ylab("CLCSLD") + xlab(NULL) + facet_grid(.~"BEKK and uGARCH");g5

Data6 <- cbind(Dates_OS, BEKK_DCC_2006_CL %>% .$Diff %>% unlist() %>% cumsum() ) %>% as.data.frame(); colnames(Data6) <- c("Date","CLCLD")

g6 = ggplot(Data6) +
  geom_line(aes(x = Date %>% as.Date(origin = "1970-01-01"), y = CLCLD) ,size = 1) + 
  ylab("CLCSLD") + xlab(NULL) + facet_grid(.~"BEKK and DCC");g6


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

multiplot(g1,g2,g3,g4,g5,g6 , cols = 3)

