setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(tidyverse,emdbook,mvtnorm,plotly)



Sigma <- as.matrix(cbind(c(9,2),c(2,9))) ; e <- c(1,1)

sim <- rmvnorm(10^4,sigma = Sigma)
w <- (solve(Sigma)%*%e) / as.numeric(t(e)%*%solve(Sigma)%*%e)
x <- seq(-10,10,by = 0.05)
y <- seq(-10,10,by = 0.05)


f <- function(x,y){
  X = matrix(0, ncol = 2, nrow = length(x))
  X[,1] = x
  X[,2] = y
  return(dmvnorm(X,sigma = Sigma))
}

f_star <- function(x,y){
  X = matrix(0, ncol = 2, nrow = length(x))
  X[,1] = x
  X[,2] = y
  I = apply(X,1,FUN = function(x){x %*% w >= qnorm(0.05)})
  return(I*dmvnorm(X, sigma = Sigma))
}

func <- function(x,y){
  cbind(x,y)%*%w
}

zf <- outer(x,y,FUN = f)
zf_star <- outer(x,y, FUN = f_star)
zf_vec <- outer(x,y,FUN = function(x,y){rep(0.0005,length(x))})

p <- plot_ly(x = ~x, y = ~y, z = ~zf_vec, type = 'scatter3d', mode = 'lines',
             opacity = 1, line = list(width = 6, color = "red", reverscale = FALSE)) %>% 
  add_surface(z = ~zf, opacity = 0.6) %>% 
  add_surface(z = ~zf_star)

p <- plot_ly(x = x,y=y,z = ~zf_star,type = "surface") %>% add_surface(z = ~zf, opacity = 0.6) %>% 
  add_trace(x = x, y = x, z = ~rep(0,length(x)), mode = "lines", type = "scatter3d", 
                                          line = list(width = 20, color = "red")) %>% 
  layout(scene = list(
    xaxis = list(title = "x"),
    yaxis = list(title = "y"),
    zaxis = list(title = "Density")
    ))
         






