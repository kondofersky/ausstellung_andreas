
stringSearch<-
  function(
    # folder you want to search
    folder = paste0(getwd(),'/StringSearchInFiles'),
    # what you are searching for inside the code in Regex
    string = 'stringyouwillfind',
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

