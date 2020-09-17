getsiebentageinzidenz<-function(max=maxDate,mytable){max.Date<-as.Date(max)
sum(mytable%>%filter(referencedate>=max.Date-6&referencedate<=max.Date)%>%select(total))/einwohnerMunich*100000}  
