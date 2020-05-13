# display the area surroundings of string in file
getSurroundings<-function(string='iasviewability',file,area=200,myignorecase = T){
  Code<-read_file(file)
  result.count<-str_count(Code,regex(string,ignore_case = myignorecase))
  result<-vector(mode="character", length=result.count)
  result2<-vector(mode="character", length=result.count)
  
  stringchars<-nchar(string)
  allregexs<-gregexpr(pattern = string,text = Code,ignore.case = myignorecase)[[1]]
  position <- 0
  #return(result)
  for(i in 1:result.count){
    
    #find position of ith appearance
    result2[i] <- substr(x = Code,
                         start = allregexs[i]-area,
                         stop = allregexs[i]+area+stringchars
    )
  }
  
  return(result2)
}
