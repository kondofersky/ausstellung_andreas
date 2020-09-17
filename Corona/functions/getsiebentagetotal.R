getsiebentagetotal<-function(max=maxDate,tbl){max.Date<-as.Date(max)
sum(tbl%>%filter(referencedate>=max.Date-6&referencedate<=max.Date)%>%select(total))}  