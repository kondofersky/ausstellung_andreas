library(dplyr)
library(tidyquant)
wd.cur <- getwd()

# fetch all stocks from 's&p600'
sp600 <- tq_index('SP600')%>%mutate(index='SP600')

# fetch data for last 5 years for s&p600-indices
mysp600data <- tq_get(sp600$symbol,
                      from = Sys.Date() - (365*10))%>%
  right_join(sp600,.)
  
# fetch all stocks from 'dowglobal'
dowglobal <- tq_index('DOWGLOBAL')%>%mutate(index='DOWGlobal')

# fetch data for last 5 years for dowglobal-indices

mydowglobaldata <- tq_get(dowglobal$symbol,
                      from = Sys.Date() - (365*10))%>%
  right_join(dowglobal,.)

#combine data
mydata <- rbind(mysp600data,mydowglobaldata)

save(mydata, file = paste0(getwd(),'/sp600dowglobal.rdata'))
