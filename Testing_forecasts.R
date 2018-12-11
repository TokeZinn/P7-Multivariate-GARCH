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



deck <- 30 ; draws <- 4 ; cards <- 2 ; prob <- 0 ; prob2 <- 0
for(i in 1:cards){
  prob <- prob + choose(cards,i)*choose(deck-cards,draws-i)/choose(deck,draws)
  prob2 <- prob2 + choose(cards,i)*choose(deck-draws-cards,draws-i)/choose(deck-draws,draws)
}
prob + (1-prob)*prob2 + (1-(prob + (1-prob)*prob2))*cards/(deck-draws)

deck <- 30 ; draws <- 3 ; cards <- 2 ; prob <- 0 ; prob2 <- 0
for(i in 1:cards){
  prob <- prob + choose(cards,i)*choose(deck-cards,draws-i)/choose(deck,draws)
  prob2 <- prob2 + choose(cards,i)*choose(deck-draws-cards,draws-i)/choose(deck-draws,draws)
}
prob + (1-prob)*prob2 + (1-(prob + (1-prob)*prob2))*cards/(deck-draws)




n <- 611 ; sum <- 0
for(i in 1:4){
  sum <- sum + floor(n/5^i)
}

