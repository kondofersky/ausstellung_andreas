# function that searches string in file and returns how often it appeared
findFiles<-function(file,string=string,myignorecase = T){
  Code<-read_file(paste0(file))
  #result.grepl<-grepl(pattern = string,x = Code,ignore.case = myignorecase)
  result.count<-str_count(Code,regex(string,ignore_case = myignorecase))
  return(data.frame(resultcount=result.count))
}
