source("VaR_Functions.R")

#2006 ---------------------------------------------------------

load("../DATA/Workspace2006.Rdata")

load("../Forecasts_2006/BEKK_forecasts_2006_BFGS.Rdata")
load("../Forecasts_2006/DCC_forecasts_2006.Rdata")
load("../Forecasts_2006/uGARCH_forecasts_2006.Rdata")
load("../Forecasts_2006/Benchmark_forecasts_2006.Rdata")

#alpha_test = 0.05

NonPara_EV = NonParaEmpericalVarChecker(IS = IS, OS = OS , alpha = alpha_test);NonPara_EV[[2]]*100
Bench_EV = ListEmpericalVarChecker(IS = IS, OS = OS, List = bench_2006 , alpha = alpha_test);Bench_EV[[2]]*100
uGARCH_EV = ListEmpericalVarChecker(IS = IS, OS = OS , List = H_g, alpha = alpha_test);uGARCH_EV[[2]]*100
BEKK_EV = ListEmpericalVarChecker(IS = IS, OS = OS , List = H_bekk , alpha = alpha_test);BEKK_EV[[2]]*100
DCC_EV = ListEmpericalVarChecker(IS = IS, OS = OS , List = H_dcc, alpha = alpha_test);DCC_EV[[2]]*100

PortReturnsOS = MV_Port_Returns_OS(IS = IS, OS = OS)  %>% .$Returns

# Plots -------------------------------------------------------------------

# VaR_DF = data.frame(cbind(Return_DF_OS$Date, PortReturnsOS,NonPara_EV[[1]] , Bench_EV[[1]], uGARCH_EV[[1]], BEKK_EV[[1]], DCC_EV[[1]]))
# names(VaR_DF) = c("Date","Returns", "NP", "Bench", "uGARCH","BEKK","DCC")

# VaR_DF %>% select("Date","Returns","Bench") %>% 
#   gather(key = "VaR", value = "Value", -Date) %>% 
#   ggplot(aes(x = as.Date(Date , origin = "1970-01-01"), y = Value, color = VaR, group = rev(VaR) )) + 
#   geom_line() + scale_color_manual(values = c("#f00000", "#000000")) +
#   theme(legend.title=element_blank()) + xlab("Date") + ylab("Returns / VaR") -> p2;p2


# multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
#   library(grid)
#   
#   # Make a list from the ... arguments and plotlist
#   plots <- c(list(...), plotlist)
#   
#   numPlots = length(plots)
#   
#   # If layout is NULL, then use 'cols' to determine layout
#   if (is.null(layout)) {
#     # Make the panel
#     # ncol: Number of columns of plots
#     # nrow: Number of rows needed, calculated from # of cols
#     layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
#                      ncol = cols, nrow = ceiling(numPlots/cols))
#   }
#   
#   if (numPlots==1) {
#     print(plots[[1]])
#     
#   } else {
#     # Set up the page
#     grid.newpage()
#     pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
#     
#     # Make each plot, in the correct location
#     for (i in 1:numPlots) {
#       # Get the i,j matrix positions of the regions that contain this subplot
#       matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
#       
#       print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
#                                       layout.pos.col = matchidx$col))
#     }
#   }
# }
# 
# multiplot(p1,p2,p3,p4,p5 , cols = 1)
