source("VaR_Functions.R")

# 2012 --------------------------------------------------------------------------------------------

load("../DATA/Workspace2012.Rdata")
load("../Forecasts_2012/BEKK_forecasts_2012_BFGS.Rdata")#; H_BEKK = mod; remove(mod)
load("../Forecasts_2012/DCC_forecasts_2012.Rdata")
load("../Forecasts_2012/uGARCH_forecasts_2012.Rdata")
load("../Forecasts_2012/Benchmark_forecasts_2012.Rdata")

#alpha_test = 0.05

NonPara_EV = NonParaEmpericalVarChecker(IS = IS, OS = OS, alpha = alpha_test);NonPara_EV[[2]]*100
Bench_EV = ListEmpericalVarChecker(IS = IS, OS = OS, List = bench_2012 , alpha = alpha_test);Bench_EV[[2]]*100
uGARCH_EV = ListEmpericalVarChecker(IS = IS, OS = OS, List = H_g, alpha = alpha_test);uGARCH_EV[[2]]*100
BEKK_EV = ListEmpericalVarChecker(IS = IS, OS = OS, List = H_bekk , alpha = alpha_test);BEKK_EV[[2]]*100
DCC_EV = ListEmpericalVarChecker(IS = IS, OS = OS, List = H_dcc, alpha = alpha_test);DCC_EV[[2]]*100

PortReturnsOS = MV_Port_Returns_OS(IS = IS, OS = OS) %>% .$Returns


# Plots -------------------------------------------------------------------

VaR_DF = data.frame(cbind(Return_DF_OS$Date, PortReturnsOS,NonPara_EV[[1]] , Bench_EV[[1]], uGARCH_EV[[1]], BEKK_EV[[1]], DCC_EV[[1]]))
names(VaR_DF) = c("Date","Returns", "NP", "Cov", "uGARCH","BEKK","DCC")

VaR_DF %>% select("Date","Returns","NP") %>%
  gather(key = "VaR", value = "Value", -Date) %>%
  ggplot(aes(x = as.Date(Date , origin = "1970-01-01"), y = Value, color = VaR, group = rev(VaR) )) +
  geom_line() + scale_color_manual(values = c("#f00000", "#000000")) +
  theme(legend.title=element_blank()) + xlab("Date") + ylab("Returns / VaR") -> p1;p1



source("../Multiplot.R")

multiplot(p4, cols = 1)
 