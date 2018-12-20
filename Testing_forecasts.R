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



dato <- Dates_OS %>% as.data.frame(); colnames(dato) <- c("Date")
colnames(OS) <- c("SP500 Returns", "Gold Returns" , "Oil Returns")
cbind(dato,OS) %>% as.data.frame() %>%  gather(key = "Stock", value = "Value", -Date) %>% 
  mutate(Color = case_when(grepl(pattern = "Gold", Stock) ~ "Gold",
                           grepl(pattern = "Oil", Stock) ~ "Oil",
                           grepl(pattern = "SP500", Stock) ~ "SP500")) %>% 
  ggplot(., aes(x = Date, y = Value, color = Color, group = Stock)) + 
  theme(legend.position="none") +
   geom_line() + scale_color_manual(values = c("#CCCC00", "#333333","#619CFF")) + 
   facet_wrap(~as_factor(Stock, levels = c("SP500", "Gold", "Oil", "SP500 Returns", "Gold Returns" , "Oil Returns")), ncol = 3, scales = "free_y")



test1 <- WLR.test(OS,H1 = H_g,bench_2006,Plot = T,Dates = Dates_OS, 
                  title = "uGARCH and Sample Covariance")
test2 <- WLR.test(OS,H1 = H_dcc,H2 = bench_2006,Plot = T,Dates = Dates_OS,
                  title = "DCC and Sample Covariance")
test3 <- WLR.test(OS,H1 = H_bekk,H2 = bench_2006,Plot = T,Dates = Dates_OS,
                       title = "Bekk and Sample Covariance")

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
colnames(OS) <- c("SP500 Returns", "Gold Returns" , "Oil Returns")
cbind(dato,OS) %>% as.data.frame() %>%  gather(key = "Stock", value = "Value", -Date) %>% 
  mutate(Color = case_when(grepl(pattern = "Gold", Stock) ~ "Gold",
                           grepl(pattern = "Oil", Stock) ~ "Oil",
                           grepl(pattern = "SP500", Stock) ~ "SP500")) %>% 
  ggplot(., aes(x = Date, y = Value, color = Color, group = Stock)) + 
  theme(legend.position="none") +
  geom_line() + scale_color_manual(values = c("#CCCC00", "#333333","#619CFF")) + 
  facet_wrap(~as_factor(Stock, levels = c("SP500", "Gold", "Oil", "SP500 Returns", "Gold Returns" , "Oil Returns")), ncol = 3, scales = "free_y")


test1 <- WLR.test(OS,H1 = H_g,bench_2012,Plot = T,Dates = Dates_OS, 
                  title = "uGARCH and Sample Covariance")
test2 <- WLR.test(OS,H1 = H_dcc,H2 = bench_2012,Plot = T,Dates = Dates_OS,
                  title = "DCC and Sample Covariance")
test3 <- WLR.test(OS,H1 = H_bekk,H2 = bench_2012,Plot = T,Dates = Dates_OS,
                  title = "Bekk and Sample Covariance")

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
{
load("./DATA/Workspace2006.Rdata")
load("./Test Results WLR CL/2006/BEKK_DCC_2006_CL.Rdata")
load("./Test Results WLR CL/2006/BEKK_uGARCH_2006_CL.Rdata")
load("./Test Results WLR CL/2006/BENCH_BEKK_2006_CL.Rdata")
load("./Test Results WLR CL/2006/BENCH_DCC_2006_CL.Rdata")
load("./Test Results WLR CL/2006/BENCH_uGARCH_2006_CL.Rdata")
load("./Test Results WLR CL/2006/DCC_uGARCH_2006_CL.Rdata")

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

Data1 <- cbind(Dates_OS, BENCH_uGARCH_2006_CL %>% .$Diff %>% unlist() %>% cumsum()*(-1) ) %>% as.data.frame(); colnames(Data1) <- c("Date","CLCLD")

g1 = ggplot(Data1) + 
  geom_line(aes(x = Date %>% as.Date(origin = "1970-01-01"), y = CLCLD) ,size = 1) + 
  ylab("CLCLD") + xlab(NULL) + facet_grid(.~"uGARCH and Sample Covariance")

Data2 <- cbind(Dates_OS, BENCH_DCC_2006_CL %>% .$Diff %>% unlist() %>% cumsum()*(-1) ) %>% as.data.frame(); colnames(Data2) <- c("Date","CLCLD")

g2 = ggplot(Data2) +
  geom_line(aes(x = Date %>% as.Date(origin = "1970-01-01"), y = CLCLD) ,size = 1) + 
  ylab("CLCLD") + xlab(NULL) + facet_grid(.~"DCC and Sample Covariance")

Data3 <- cbind(Dates_OS, BENCH_BEKK_2006_CL %>% .$Diff %>% unlist() %>% cumsum()*(-1) ) %>% as.data.frame(); colnames(Data3) <- c("Date","CLCLD")

g3 = ggplot(Data3) +
  geom_line(aes(x = Date %>% as.Date(origin = "1970-01-01"), y = CLCLD) ,size = 1) + 
  ylab("CLCLD") + xlab(NULL) + facet_grid(.~"BEKK and Sample Covariance")

Data4 <- cbind(Dates_OS, DCC_uGARCH_2006_CL %>% .$Diff %>% unlist() %>% cumsum() ) %>% as.data.frame(); colnames(Data4) <- c("Date","CLCLD")

g4 = ggplot(Data4) +
  geom_line(aes(x = Date %>% as.Date(origin = "1970-01-01"), y = CLCLD) ,size = 1) + 
  ylab("CLCLD") + xlab(NULL) + facet_grid(.~"DCC and uGARCH")

Data5 <- cbind(Dates_OS, BEKK_uGARCH_2006_CL %>% .$Diff %>% unlist() %>% cumsum() ) %>% as.data.frame(); colnames(Data5) <- c("Date","CLCLD")

g5 = ggplot(Data5) +
  geom_line(aes(x = Date %>% as.Date(origin = "1970-01-01"), y = CLCLD) ,size = 1) + 
  ylab("CLCLD") + xlab(NULL) + facet_grid(.~"BEKK and uGARCH")

#Correction for NaN 
BEKK_DCC_2006_CL$Diff[[590]] = BEKK_uGARCH_2006_CL$Diff[[590]] - DCC_uGARCH_2006_CL$Diff[[590]]

Data6 <- cbind(Dates_OS, BEKK_DCC_2006_CL %>% .$Diff %>% unlist() %>% cumsum() ) %>% as.data.frame(); colnames(Data6) <- c("Date","CLCLD")

g6 = ggplot(Data6) +
  geom_line(aes(x = Date %>% as.Date(origin = "1970-01-01"), y = CLCLD) ,size = 1) + 
  ylab("CLCLD") + xlab(NULL) + facet_grid(.~"BEKK and DCC")
multiplot(g1,g2,g3,g4,g5,g6 , cols = 3)

}

## CL 2012
{
load("./DATA/Workspace2012.Rdata")
load("./Test Results WLR CL/2012/BEKK_DCC_2012_CL.Rdata")
load("./Test Results WLR CL/2012/BEKK_uGARCH_2012_CL.Rdata")
load("./Test Results WLR CL/2012/BENCH_BEKK_2012_CL.Rdata")
load("./Test Results WLR CL/2012/BENCH_DCC_2012_CL.Rdata")
load("./Test Results WLR CL/2012/BENCH_uGARCH_2012_CL.Rdata")
load("./Test Results WLR CL/2012/DCC_uGARCH_2012_CL.Rdata")


Data1 <- cbind(Dates_OS, BENCH_uGARCH_2012_CL %>% .$Diff %>% unlist() %>% cumsum()*(-1) ) %>% as.data.frame(); colnames(Data1) <- c("Date","CLCLD")

g1 = ggplot(Data1) + 
  geom_line(aes(x = Date %>% as.Date(origin = "1970-01-01"), y = CLCLD) ,size = 1) + 
  ylab("CLCLD") + xlab(NULL) + facet_grid(.~"uGARCH and Sample Covariance")

Data2 <- cbind(Dates_OS, BENCH_DCC_2012_CL %>% .$Diff %>% unlist() %>% cumsum()*(-1) ) %>% as.data.frame(); colnames(Data2) <- c("Date","CLCLD")

g2 = ggplot(Data2) +
  geom_line(aes(x = Date %>% as.Date(origin = "1970-01-01"), y = CLCLD) ,size = 1) + 
  ylab("CLCLD") + xlab(NULL) + facet_grid(.~"DCC and Sample Covariance")

Data3 <- cbind(Dates_OS, BENCH_BEKK_2012_CL %>% .$Diff %>% unlist() %>% cumsum()*(-1) ) %>% as.data.frame(); colnames(Data3) <- c("Date","CLCLD")

g3 = ggplot(Data3) +
  geom_line(aes(x = Date %>% as.Date(origin = "1970-01-01"), y = CLCLD) ,size = 1) + 
  ylab("CLCLD") + xlab(NULL) + facet_grid(.~"BEKK and Sample Covariance")

Data4 <- cbind(Dates_OS, DCC_uGARCH_2012_CL %>% .$Diff %>% unlist() %>% cumsum() ) %>% as.data.frame(); colnames(Data4) <- c("Date","CLCLD")

g4 = ggplot(Data4) +
  geom_line(aes(x = Date %>% as.Date(origin = "1970-01-01"), y = CLCLD) ,size = 1) + 
  ylab("CLCLD") + xlab(NULL) + facet_grid(.~"DCC and uGARCH")

Data5 <- cbind(Dates_OS, BEKK_uGARCH_2012_CL %>% .$Diff %>% unlist() %>% cumsum() ) %>% as.data.frame(); colnames(Data5) <- c("Date","CLCLD")

g5 = ggplot(Data5) +
  geom_line(aes(x = Date %>% as.Date(origin = "1970-01-01"), y = CLCLD) ,size = 1) + 
  ylab("CLCLD") + xlab(NULL) + facet_grid(.~"BEKK and uGARCH")

Data6 <- cbind(Dates_OS, BEKK_DCC_2012_CL %>% .$Diff %>% unlist() %>% cumsum() ) %>% as.data.frame(); colnames(Data6) <- c("Date","CLCLD")

g6 = ggplot(Data6) +
  geom_line(aes(x = Date %>% as.Date(origin = "1970-01-01"), y = CLCLD) ,size = 1) + 
  ylab("CLCLD") + xlab(NULL) + facet_grid(.~"BEKK and DCC")
multiplot(g1,g2,g3,g4,g5,g6 , cols = 3)

}



## CSL 2006
{
load("./DATA/Workspace2006.Rdata")
load("./Test_Results_WLR_CSL/2006/BEKK_DCC_2006.Rdata")
load("./Test_Results_WLR_CSL/2006/BEKK_uGARCH_2006.Rdata")
load("./Test_Results_WLR_CSL/2006/BENCH_BEKK_2006.Rdata")
load("./Test_Results_WLR_CSL/2006/BENCH_DCC_2006.Rdata")
load("./Test_Results_WLR_CSL/2006/BENCH_uGARCH_2006.Rdata")
load("./Test_Results_WLR_CSL/2006/DCC_uGARCH_2006.Rdata")


Data1 <- cbind(Dates_OS, BENCH_uGARCH_2006 %>% .$Diff %>% unlist() %>% cumsum()*(-1) ) %>% as.data.frame(); colnames(Data1) <- c("Date","CLCSLD")

g1 = ggplot(Data1) + 
  geom_line(aes(x = Date %>% as.Date(origin = "1970-01-01"), y = CLCSLD) ,size = 1) + 
  ylab("CLCSLD") + xlab(NULL) + facet_grid(.~"uGARCH and Sample Covariance")

Data2 <- cbind(Dates_OS, BENCH_DCC_2006 %>% .$Diff %>% unlist() %>% cumsum()*(-1) ) %>% as.data.frame(); colnames(Data2) <- c("Date","CLCSLD")

g2 = ggplot(Data2) +
  geom_line(aes(x = Date %>% as.Date(origin = "1970-01-01"), y = CLCSLD) ,size = 1) + 
  ylab("CLCSLD") + xlab(NULL) + facet_grid(.~"DCC and Sample Covariance")

Data3 <- cbind(Dates_OS, BENCH_BEKK_2006 %>% .$Diff %>% unlist() %>% cumsum()*(-1) ) %>% as.data.frame(); colnames(Data3) <- c("Date","CLCSLD")

g3 = ggplot(Data3) +
  geom_line(aes(x = Date %>% as.Date(origin = "1970-01-01"), y = CLCSLD) ,size = 1) + 
  ylab("CLCSLD") + xlab(NULL) + facet_grid(.~"BEKK and Sample Covariance")

Data4 <- cbind(Dates_OS, DCC_uGARCH_2006 %>% .$Diff %>% unlist() %>% cumsum() ) %>% as.data.frame(); colnames(Data4) <- c("Date","CLCSLD")

g4 = ggplot(Data4) +
  geom_line(aes(x = Date %>% as.Date(origin = "1970-01-01"), y = CLCSLD) ,size = 1) + 
  ylab("CLCSLD") + xlab(NULL) + facet_grid(.~"DCC and uGARCH")

Data5 <- cbind(Dates_OS, BEKK_uGARCH_2006 %>% .$Diff %>% unlist() %>% cumsum() ) %>% as.data.frame(); colnames(Data5) <- c("Date","CLCSLD")

g5 = ggplot(Data5) +
  geom_line(aes(x = Date %>% as.Date(origin = "1970-01-01"), y = CLCSLD) ,size = 1) + 
  ylab("CLCSLD") + xlab(NULL) + facet_grid(.~"BEKK and uGARCH")

Data6 <- cbind(Dates_OS, BEKK_DCC_2006 %>% .$Diff %>% unlist() %>% cumsum() ) %>% as.data.frame(); colnames(Data6) <- c("Date","CLCSLD")

g6 = ggplot(Data6) +
  geom_line(aes(x = Date %>% as.Date(origin = "1970-01-01"), y = CLCSLD) ,size = 1) + 
  ylab("CLCSLD") + xlab(NULL) + facet_grid(.~"BEKK and DCC")
multiplot(g1,g2,g3,g4,g5,g6 , cols = 3)

}


## CSL 2012
{
load("./DATA/Workspace2012.Rdata")
load("./Test_Results_WLR_CSL/2012/BEKK_DCC_2012.Rdata")
load("./Test_Results_WLR_CSL/2012/BEKK_uGARCH_2012.Rdata")
load("./Test_Results_WLR_CSL/2012/BENCH_BEKK_2012.Rdata")
load("./Test_Results_WLR_CSL/2012/BENCH_DCC_2012.Rdata")
load("./Test_Results_WLR_CSL/2012/BENCH_uGARCH_2012.Rdata")
load("./Test_Results_WLR_CSL/2012/DCC_uGARCH_2012.Rdata")


Data1 <- cbind(Dates_OS, BENCH_uGARCH_2012 %>% .$Diff %>% unlist() %>% cumsum()*(-1) ) %>% as.data.frame(); colnames(Data1) <- c("Date","CLCSLD")

g1 = ggplot(Data1) + 
  geom_line(aes(x = Date %>% as.Date(origin = "1970-01-01"), y = CLCSLD) ,size = 1) + 
  ylab("CLCSLD") + xlab(NULL) + facet_grid(.~"uGARCH and Sample Covariance")

Data2 <- cbind(Dates_OS, BENCH_DCC_2012 %>% .$Diff %>% unlist() %>% cumsum()*(-1) ) %>% as.data.frame(); colnames(Data2) <- c("Date","CLCSLD")

g2 = ggplot(Data2) +
  geom_line(aes(x = Date %>% as.Date(origin = "1970-01-01"), y = CLCSLD) ,size = 1) + 
  ylab("CLCSLD") + xlab(NULL) + facet_grid(.~"DCC and Sample Covariance")

Data3 <- cbind(Dates_OS, BENCH_BEKK_2012 %>% .$Diff %>% unlist() %>% cumsum()*(-1) ) %>% as.data.frame(); colnames(Data3) <- c("Date","CLCSLD")

g3 = ggplot(Data3) +
  geom_line(aes(x = Date %>% as.Date(origin = "1970-01-01"), y = CLCSLD) ,size = 1) + 
  ylab("CLCSLD") + xlab(NULL) + facet_grid(.~"BEKK and Sample Covariance")

Data4 <- cbind(Dates_OS, DCC_uGARCH_2012 %>% .$Diff %>% unlist() %>% cumsum() ) %>% as.data.frame(); colnames(Data4) <- c("Date","CLCSLD")

g4 = ggplot(Data4) +
  geom_line(aes(x = Date %>% as.Date(origin = "1970-01-01"), y = CLCSLD) ,size = 1) + 
  ylab("CLCSLD") + xlab(NULL) + facet_grid(.~"DCC and uGARCH")

Data5 <- cbind(Dates_OS, BEKK_uGARCH_2012 %>% .$Diff %>% unlist() %>% cumsum() ) %>% as.data.frame(); colnames(Data5) <- c("Date","CLCSLD")

g5 = ggplot(Data5) +
  geom_line(aes(x = Date %>% as.Date(origin = "1970-01-01"), y = CLCSLD) ,size = 1) + 
  ylab("CLCSLD") + xlab(NULL) + facet_grid(.~"BEKK and uGARCH")

Data6 <- cbind(Dates_OS, BEKK_DCC_2012 %>% .$Diff %>% unlist() %>% cumsum() ) %>% as.data.frame(); colnames(Data6) <- c("Date","CLCSLD")

g6 = ggplot(Data6) +
  geom_line(aes(x = Date %>% as.Date(origin = "1970-01-01"), y = CLCSLD) ,size = 1) + 
  ylab("CLCSLD") + xlab(NULL) + facet_grid(.~"BEKK and DCC")
multiplot(g1,g2,g3,g4,g5,g6 , cols = 3)
}