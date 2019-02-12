setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)

load("./Test_Results_WLR_CSL/2012/BENCH_BEKK_2012.Rdata")
load("./Test_Results_WLR_CSL/2012/BENCH_DCC_2012.Rdata")
load("./Test_Results_WLR_CSL/2012/BENCH_uGARCH_2012.Rdata")
load("./Test_Results_WLR_CSL/2012/BEKK_DCC_2012.Rdata")
load("./Test_Results_WLR_CSL/2012/BEKK_uGARCH_2012.Rdata")
load("./Test_Results_WLR_CSL/2012/DCC_uGARCH_2012.Rdata")

BEKK_uGARCH_2012 %>% .$Diff %>% unlist() %>% cumsum()
BEKK_uGARCH_2012 %>%  .$'p-value'

#Making plots

load("./DATA/Workspace2012.Rdata")

Data1 <- cbind(Dates_OS, BENCH_uGARCH_2012 %>% .$Diff %>% unlist() %>% cumsum()*(-1) ) %>% as.data.frame(); colnames(Data1) <- c("Date","CLSD")

g1 = ggplot(Data1) + 
  geom_line(aes(x = Date %>% as.Date(origin = "1970-01-01"), y = CLSD) ,size = 1) + 
  ylab("CLCSLD") + xlab(NULL) + facet_grid(.~"uGARCH and Benchmark");g1

Data2 <- cbind(Dates_OS, BENCH_DCC_2012 %>% .$Diff %>% unlist() %>% cumsum()*(-1) ) %>% as.data.frame(); colnames(Data2) <- c("Date","CLSD")

g2 = ggplot(Data2) +
  geom_line(aes(x = Date %>% as.Date(origin = "1970-01-01"), y = CLSD) ,size = 1) + 
  ylab("CLCSLD") + xlab(NULL) + facet_grid(.~"DCC and Benchmark");g2

Data3 <- cbind(Dates_OS, BENCH_BEKK_2012 %>% .$Diff %>% unlist() %>% cumsum()*(-1) ) %>% as.data.frame(); colnames(Data3) <- c("Date","CLSD")

g3 = ggplot(Data3) +
  geom_line(aes(x = Date %>% as.Date(origin = "1970-01-01"), y = CLSD) ,size = 1) + 
  ylab("CLCSLD") + xlab(NULL) + facet_grid(.~"BEKK and Benchmark");g3

Data4 <- cbind(Dates_OS, DCC_uGARCH_2012 %>% .$Diff %>% unlist() %>% cumsum() ) %>% as.data.frame(); colnames(Data4) <- c("Date","CLSD")

g4 = ggplot(Data4) +
  geom_line(aes(x = Date %>% as.Date(origin = "1970-01-01"), y = CLSD) ,size = 1) + 
  ylab("CLCSLD") + xlab(NULL) + facet_grid(.~"DCC and uGARCH");g4

Data5 <- cbind(Dates_OS, BEKK_uGARCH_2012 %>% .$Diff %>% unlist() %>% cumsum() ) %>% as.data.frame(); colnames(Data5) <- c("Date","CLSD")

g5 = ggplot(Data5) +
  geom_line(aes(x = Date %>% as.Date(origin = "1970-01-01"), y = CLSD) ,size = 1) + 
  ylab("CLCSLD") + xlab(NULL) + facet_grid(.~"BEKK and uGARCH");g5

Data6 <- cbind(Dates_OS, BEKK_DCC_2012 %>% .$Diff %>% unlist() %>% cumsum() ) %>% as.data.frame(); colnames(Data6) <- c("Date","CLSD")

g6 = ggplot(Data6) +
  geom_line(aes(x = Date %>% as.Date(origin = "1970-01-01"), y = CLSD) ,size = 1) + 
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
