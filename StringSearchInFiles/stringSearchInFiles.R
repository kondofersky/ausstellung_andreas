library(readr)
library(stringr)
#enter folder you want to search
functionfiles<-list.files(paste0(getwd(),'/StringSearchInFiles/functions'),full.names = T)
for(myfunctionfile in functionfiles){
  source(myfunctionfile)
  }

result<-stringSearch()

# Files with that pattern. Contains:
# - Actual File name 
# - Count of appearances of this pattern inside that file.
# - Date when that file was last modified
View(result$files)

# show surrounding areas of appearances
for (myfile in 1:length(result$surroundings)) {
  cat(paste0('\r\n\r\n','File: ',names(result$surroundings[myfile]),'\r\n\r\n'))
  for(i in 1:length(result$surroundings[[myfile]])){
    cat(paste0('\r\n\r\n # of appearance: ',i,'\r\n\r\n',result$surroundings[[myfile]][i]))
  }
}
