setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("./DATA/DataAndReturnFct_FullSample.R")
pacman::p_load(forecast)

Date = Gold$Date

Close = as.tibble(cbind(SP500$Close, Gold$Value, Oil$Value)) %>% mutate(Date = as.Date(Date)) 
Close = Close[,c(4,1,2,3)]; names(Close) = c("Date","SP500","Gold","Oil")

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

#Make tibble Include Returns
Close %>% returns(series = c("SP500","Gold","Oil")) %>% mutate(Date = as.Date(Date)) -> Close_Returns

names(Close_Returns) = c("Date","SP500", "Gold", "Oil", "SP500 Returns", "Gold Returns" , "Oil Returns")


ggCcf(Close_Returns[,5] , Close_Returns[,6]) + facet_grid(. ~ "SP500 vs. Gold") + ggtitle("") -> p1

ggCcf(Close_Returns[,5] , Close_Returns[,7]) + facet_grid(. ~ "SP500 vs. Oil") + ggtitle("") -> p2

ggCcf(Close_Returns[,6] , Close_Returns[,7]) + facet_grid(. ~ "Gold vs. Oil") + ggtitle("") -> p3

ggCcf(Close_Returns[,5] %>% as.matrix() %>% as.numeric() %>% .^2 , 
      Close_Returns[,6] %>% as.matrix() %>% as.numeric() %>% .^2) +
       facet_grid(. ~ "SP500 vs. Gold - Squared") + ggtitle("") -> p4

ggCcf(Close_Returns[,5] %>% as.matrix() %>% as.numeric() %>% .^2 , 
      Close_Returns[,7] %>% as.matrix() %>% as.numeric() %>% .^2) +
      facet_grid(. ~ "SP500 vs. Oil - Squared") + ggtitle("") -> p5

ggCcf(Close_Returns[,6] %>% as.matrix() %>% as.numeric() %>% .^2 , 
      Close_Returns[,7] %>% as.matrix() %>% as.numeric() %>% .^2) +
      facet_grid(. ~ "Gold vs. Oil - Squared ") + ggtitle("") -> p6

multiplot(p1,p4,p2,p5,p3,p6 , cols = 3)
