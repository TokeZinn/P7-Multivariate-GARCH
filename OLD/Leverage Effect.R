# Leverage effect

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("./Data_Load.R")
VIX = read_csv("./RAWDATA/^VIX.csv") %>% .[,-c(2:4,6,7)] 

Price = SP500[,-c(2:4,6,7)] 

Leverage = cbind.data.frame(Price, VIX$Close) %>% as.tibble(); names(Leverage)[c(2,3)] = c("SP500","VIX")

Leverage = Leverage %>% mutate(Date = as.Date(Date))

ggplot(data = Leverage , aes(x = Date)) + 
  geom_line(aes(y = SP500 , colour = "SP500")) +
  geom_line(aes(y = VIX*40 , colour = "VIX")) +
  scale_y_continuous(limits = c(250,3000), sec.axis = sec_axis(~./40, name = "VIX")) +
  scale_colour_manual(values = c("#619cff", "red")) + 
  facet_grid(~as.factor("Leverage Effect: SP500 and VIX"))  + 
  labs(colour = "") -> P1


Leverage_returns = returns(Leverage, series = c("SP500", "VIX")) %>% .[-c(2,3)]    

ggplot(data = Leverage_returns , aes(x = Returns_SP500 , y = Returns_VIX)) + 
  geom_point() + 
  geom_smooth(method = "lm" , se = F ) + 
  labs(x = "Returns of SP500" , y = "Change of VIX") + 
  facet_grid(~as.factor("Leverage Effect: SP500 Returns vs. Change of VIX")) -> P2

multiplot(P1,P2 , cols = 1)

