#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(Quandl, tidyverse)

# Returns func ----

#Function for Calculating Returns
returns = function(data, series = NULL, logreturns = T, Date = T,demean = T){
  if(is.vector(data)){
    if(logreturns){
      return(100*c(NA,log(data[2:length(data)]) - log(data[2:length(data)-1]))) 
    }else{
      return(100*c(NA,(data[2:length(data)]-data[2:length(data)-1])/data[2:length(data)-1]))
    }
  }
  
  if(!is.tibble(data)|!is.data.frame(data)){
    data = as.tibble(data)
  }
  name_data = names(data)
  if(is.null(series)){
    idx = 2
    name_vector = name_data[idx]
  }else{
    idx = series 
    if(is.numeric(series)){
      name_vector = name_data[idx]
    }else{
      name_vector = series
    }
  }
  
  placeholder = as.matrix(data[,idx])
  n = dim(placeholder)[1]
  
  name_returns = c()
  for(name in name_vector){
    name_returns = c(name_returns, paste("Returns_",name, sep = ""))
  }
  #browser()
  
  if(logreturns){
    return_matrix = 100*(log(placeholder[2:n,]) - log(placeholder[2:n - 1,]))
    if(demean){
      for(i in 1:length(return_matrix[1,])){
        return_matrix[,i] <- return_matrix[,i] - mean(return_matrix[,i])
      }
    }
    result = as.tibble(cbind(as.matrix(data[2:n,]),return_matrix))
    names(result) = c(name_data,name_returns)
  }else{
    return_matrix = 100*((placeholder[2:n,] - placeholder[2:n - 1,])/placeholder[2:n - 1,])
    result = as.tibble(cbind(as.matrix(data[2:n,]),return_matrix))
    names(result) = c(name_data,name_returns)
  }
  result[,-1] = lapply(result[,-1],as.numeric)
  return(result)
}

# Date in sample ----

Quandl.api_key("CtCjYTvXs7FS5robdqFv")

Oil = Quandl('OPEC/ORB' , start_date= "2005-12-31", end_date = "2016-12-31")
Gold = Quandl('WGC/GOLD_DAILY_USD', start_date= "2005-12-31" , end_date = "2016-12-31")
SP500 = read.csv("./DATA/^GSPC_FullSample.csv", stringsAsFactors=FALSE) %>% .[,c(1,5)] 

SP500$Date = SP500$Date %>% as.Date()
Gold$Date = Gold$Date %>% as.Date()
Oil$Date = Oil$Date %>% as.Date()

#SP500 have fewer observations due to holidays. These are removed for the two data sets.

Index_Gold = which(!(Gold$Date %in% SP500$Date ))

Gold = Gold[-Index_Gold,] %>% arrange(Date)

Index_Oil = which(!(Oil$Date %in% SP500$Date ))

Oil = Oil[-Index_Oil,]  %>% arrange(Date)

#Oil has two less observations. These are removed from the two other data sets. 

Index_Oil_Missing = which(!(SP500$Date %in% Oil$Date ))
Index_Oil_Missing1 = which(!(Gold$Date %in% Oil$Date ))

Gold = Gold[-Index_Oil_Missing1,]
SP500 = SP500[-Index_Oil_Missing,]

#Gold has one extra observation which we remove from the two other.



remove(Index_Gold,Index_Oil,Index_Oil_Missing, Index_Oil_Missing1)

#Computing demeaned returns for each series
Gold_returns = returns(Gold$Value)[-1] - mean(returns(Gold$Value)[-1])
Oil_returns = returns(Oil$Value)[-1] - mean(returns(Oil$Value)[-1])
SP500_returns = returns(SP500$Close)[-1] - mean(returns(SP500$Close)[-1])

#Making a data frame
Return_DF = cbind.data.frame(SP500, Gold$Value , Oil$Value ); names(Return_DF) = c("Date", "SP500", "Gold", "Oil")
Return_DF = Return_DF %>% returns(series = c("SP500","Gold","Oil") , demean = F) %>% mutate(Date = as.Date(Date))


