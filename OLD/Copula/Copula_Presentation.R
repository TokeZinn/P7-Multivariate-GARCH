library(copula);library(tidyverse)

#Copula Construction 
copula = normalCopula(param=c(0.8), dim=2, dispstr="un")
distribution = mvdc(copula, margins = c("norm","exp"), paramMargins = list(list(mean = 0, sd = 2),list(rate = 2)))
set.seed(711)
data = rMvdc(1000,distribution)

#Plots 
#Scatter
data %>% as_tibble() %>% ggplot(aes(x = V1, y = V2)) + geom_point() +
  scale_x_continuous(name = "X") + scale_y_continuous(name = "Y") 

#Histogram X
data %>% as_tibble() %>% ggplot(aes(x = V1)) + geom_histogram() + scale_x_continuous(name = "X")

#Histogram #Y
data %>% as_tibble() %>% ggplot(aes(x = V2)) + geom_histogram() + scale_x_continuous(name = "Y")


#Fit Normal and Probability Transform
mu = mean(data[,1])
sig = sd(data[,1])
U_1 = pnorm(data[,1],mean = mu, sd = sig)
hist(U_1)

#Fit Exponential and Probability Transform
lambda = 1/mean(data[,2])
U_2 = pexp(data[,2], rate = lambda)
hist(U_2,xlab = expression(U[2]),main = "Histogram")

U = cbind(U_1, U_2)

U %>% as_tibble() %>% ggplot(aes(x = U_1, y = U_2)) + geom_point() + 
  scale_x_continuous(name = expr(U[1])) + scale_y_continuous(name = expr(U[2]))

#Fit Copula - compare norm- and t-distribution
norm_fit = fitCopula(normalCopula(dim = 2),data = U); summary(norm_fit); AIC(norm_fit)
t_fit = fitCopula(tCopula(dim = 2),data = U); summary(t_fit); AIC(t_fit)
frank_fit = fitCopula(frankCopula(dim = 2), data = U); summary(frank_fit); AIC(frank_fit)
joe_fit = fitCopula(joeCopula(dim = 2), data = U); summary(joe_fit); AIC(joe_fit)
gumbel_fit = fitCopula(gumbelCopula(dim = 2), data = U); summary(gumbel_fit); AIC(gumbel_fit)
