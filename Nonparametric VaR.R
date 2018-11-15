setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(tidyverse)

source("./DATA/DataAndReturnFct.R")


NonParaVaR = function(series, alpha){
  n = length(series)
  
  K = round(n*alpha)
  
  series = sort(series , decreasing = F)
  
  VaR = series[K]
  
  return(VaR)
}
