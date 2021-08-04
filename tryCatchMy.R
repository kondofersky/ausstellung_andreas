tryCatchMy <- function(expr){
  tryCatch(expr = expr,error=function(e){
    print(paste0('insufficient data'))
    return(NA)
  })
}