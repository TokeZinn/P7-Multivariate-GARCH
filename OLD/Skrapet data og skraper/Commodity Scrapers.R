pacman::p_load(rvest,dplyr)


Commodity_price_scraper = function(max_count, commodity = "oil"){

for (i in 0:(max_count/30)) {
  
  if(commodity == "oil"){
  HTML= paste0("http://www.livecharts.co.uk/futures_commodities/oil_prices_historical.php?type_symbol=futures_cl&start=",i*30)
  }else if(commodity == "gold"){
    HTML= paste0("http://www.livecharts.co.uk/futures_commodities/nyse_gold_prices_historical.php?type_symbol=futures_cl&start=",i*30)
  }
  
  if(i == 0){
    
   Data = HTML %>% read_html() %>% html_table() %>% .[[1]]
   Names = Data[1,]
   names(Data) = Names
   Data = Data[-1,]
   
  }else{
    
    Sys.sleep(1.5)
    
    Data_new = HTML %>% read_html() %>% html_table() %>% .[[1]] %>% .[-1,]
    names(Data_new) = Names
    
    Data = rbind(Data,Data_new)
    
  }
  

}
  return(Data) 
}

Oil_data = Commodity_price_scraper(max_count = 2640 , commodity = "oil")
Gold_data = Commodity_price_scraper(max_count = 2640 , commodity = "gold")

write.csv(Oil_data, "Oil_data.csv")
write.csv(Gold_data, "Gold_data.csv")
