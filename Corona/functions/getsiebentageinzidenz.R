getsiebentageinzidenz<-function(max=maxDate,mytable){max.Date<-as.Date(max)
sum(mytable%>%filter(referencedate>=max.Date-6&referencedate<=max.Date)%>%select(total))/(mytable%>%filter(referencedate==max.Date)%>%select(Population))*100000}  
