# Initialization ----------------------------------------------------------

pacman::p_load(tidyverse, GAS,xtable)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("VaR_Functions.R")


# Set confidence level and period ----------------------------------------------------

alpha_test = 0.1

period = "06" #06 or 12 as string.

# Backtesting VaR - Conditional Coverage ----------------------------------

source(paste0("VaR_Series",period,".R"))

VaRs = cbind(NonPara_EV$VaR , 
               Bench_EV$VaR,
               uGARCH_EV$VaR,
               BEKK_EV$VaR,
               DCC_EV$VaR) %>% as.data.frame() 

names(VaRs) = c("NP", "Bench", "uGARCH","BEKK","DCC")

LR_uc = matrix(0, nrow = ncol(VaRs) , ncol = 2)
LR_cc = matrix(0, nrow = ncol(VaRs) , ncol = 2)
LR_I  = matrix(0, nrow = ncol(VaRs) , ncol = 2)

for (i in 1:ncol(VaRs) ) {
  
  BackTest = BacktestVaR(data = PortReturnsOS , VaR = VaRs[,i] , alpha = alpha_test)
 
  LR_uc[i,] = BackTest$LRuc %>% as.numeric()
  LR_cc[i,] = BackTest$LRcc %>% as.numeric()
  LR_I[i,1] = LR_cc[i,1] - LR_uc[i,1]
  LR_I[i,2] = 1 - pchisq(LR_I[i,1], df=1)
  
  }

remove(BackTest,i)

cbind(LR_uc, LR_I , LR_cc) %>% round(digits = 4) %>% print()
