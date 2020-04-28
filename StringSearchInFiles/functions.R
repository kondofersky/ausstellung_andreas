# function that searches string in file and returns how often it appeared
findFiles<-function(file,string=string,myignorecase = T){
  Code<-read_file(paste0(file))
  #result.grepl<-grepl(pattern = string,x = Code,ignore.case = myignorecase)
  result.count<-str_count(Code,regex(string,ignore_case = myignorecase))
  return(data.frame(resultcount=result.count))
}

# display the area surroundings of string in file
getSurroundings<-function(string='iasviewability',file,area=200,myignorecase = T){
  Code<-read_file(file)
  result.count<-str_count(Code,regex(string,ignore_case = myignorecase))
  result<-vector(mode="character", length=result.count)
  stringchars<-nchar(string)
  position <- 0
  #return(result)
  for(i in 1:result.count){
    #find position of ith appearance
    loopcode<-
    substring(Code,
              position+((i-1)<0)*stringchars,
              nchar(Code)
    )
    positionupdate<-regexpr(pattern=string,
                      text = loopcode,
                      
                      ignore.case = myignorecase)
    position<-position+positionupdate
    result[i]<-substring(text = loopcode,
                         first = max(positionupdate-area,1),
                         last = min(positionupdate+area+stringchars,
                                    nchar(loopcode)
                                    )
                         )
  }
  
  
  return(result)
}

stringSearch<-
  function(
    # folder you want to search
    folder = 'C:/nanoathenareports',
    # what you are searching for inside the code in Regex
    string = 'iasviewability',
    # should your search be case insensivite?
    myignorecase = T,
    # add a 'date modified' filter, if you want to. Only result younger than this will be shown
    datefilter = NULL,
    # pattern (Default: R, python and SQL -files)
    pattern = '\\.R$|\\.py$|\\.r$|\\.sql|\\.SQL',
    # surrounding area
    surroundingarea = 20
  ){
    # list of ALL files in folder
    # recursive: Search also sub folders
    filelist<-list.files(folder,
                         pattern = pattern,
                         recursive=T,
                         full.names = T)
    # filter out files, that match criteria
    # datefilter:
    if(!is.null(datefilter)){
      filelist<-filelist[difftime(datefilter,file.info(filelist)$mtime)<0]}
    
    # sort by 'last modified' date
    filelist<-filelist[order(file.info(filelist)$mtime,decreasing = T)]
    # Does file-content match string?
    fileSearch<-lapply(X = filelist,FUN = findFiles,string=string)
    fileSearch<-do.call(what = rbind,args = fileSearch)
    
    #
    fulllist<-data.frame(fileslist=filelist,
                     appearancecount=fileSearch$resultcount,
                     stringsAsFactors = F)
    
    
    # Filter only files, that contain string
    results.tmp<-fulllist[fulllist$appearancecount>0,]
    
    # show surroundings of that string
    a<-lapply(X =  results.tmp$fileslist,
              FUN = getSurroundings,
              string = string,
              area = surroundingarea)
    names(a)<-results.tmp$fileslist
    returnlist <- list()
    returnlist[[1]] <- data.frame(results.tmp,
                                  lastchange=format(file.info(results.tmp$fileslist)$mtime),
                                  stringsAsFactors = F
    )
    returnlist[[2]] <- a
    names(returnlist) <- c('files','surroundings')
    return(returnlist)
    
  }

