#Sys.setenv(HTTP_PROXY = 'http://10.0.128.104:9090/')
#Sys.setenv(HTTPS_PROXY = 'http://10.0.128.104:9090/')
library(dplyr)
library(tidyquant)


# kann man deutlich optimieren, lädt viel zu lange auf Laptop
boerse_fra_symbols <- read.csv2('https://www.xetra.com/resource/blob/2289108/3fc299b11f7d9900e4049a310b479af1/data/t7-xfra-BF-allTradableInstruments.csv',skip = 2) %>%
 filter(Product.Assignment.Group == 'PAG_EQU')%>%
  filter(!is.na(Mnemonic)&Mnemonic!='')%>%
      select(Instrument,Mnemonic)%>%
    mutate(Mnemonic=paste0(Mnemonic,'.F'))
  
system.time(
myfradata <- tq_get(boerse_fra_symbols$Mnemonic,
                    from = Sys.Date() - (365*1))


)
mycompletefradata <- left_join(myfradata,boerse_fra_symbols,by=c('symbol' = 'Mnemonic'))

wd.cur <- getwd()

save(myfradata,file=paste0(wd.cur,'/image.rdata'))
save(mycompletefradata,file=paste0(wd.cur,'/imagenew.rdata'))