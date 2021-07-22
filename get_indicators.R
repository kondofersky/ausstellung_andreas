get_indicators <- function(stock, period,myresponse = 0,forforecast =FALSE){
  # Price change, by using lag function (lag (.,-period ) --> go period in future 

  # relative price change (price in x days - price today) / price today
  price_change <- (Ad(stats::lag(stock,-period)) - Ad(stock)) / Ad(stock)

  # creating response variable.
  if(is.numeric(myresponse)){    
    # creating binary response.
    response <- ifelse(price_change > myresponse, "UP", "DOWN")
  }else{
    #creating response variable. Predicting relative stock change
    response <- price_change
  }
  #Calculating RSI
  rsi <- tryCatch(expr = {RSI(Ad(stock), n=14) },error=function(e){
    print(paste0('insufficient data'))
    return(NA)
  })
  
  #High, Low, and adjusted close xts object
  hlac <- as.xts(data.frame(x=tryCatchMy(Hi(stock)), y=tryCatchMy(Lo(stock)), z=tryCatchMy(Ad(stock))))
  
  #Stochastic Oscillator
  sto <- tryCatch(expr = {stoch(hlac, nFastK = 14) *100    },error=function(e){
    print(paste0('insufficient sto data'))
    return(data.frame(rep(NA,nrow(stock)),dummy=NA,dumy2=NA))
  }) #Price Rate of Change
  #Williams %R
  wpr <-tryCatchMy(WPR(hlac, n=14) * (-100))
  
  
  #MACD
  macd <- tryCatch(expr = {MACD(Ad(stock), nFast=12, nSlow=26, nSig=9)    },error=function(e){
	print(paste0('insufficient data'))
	return(data.frame(rep(NA,nrow(stock)),dummy=NA))
})

      
  #Price Rate of Change
  roc <- tryCatchMy(ROC(Ad(stock), n=14) *100)
  #On Balance Volume
  obv <- tryCatchMy(OBV(Ad(stock), Vo(stock)))
  
  #create data set with all indicators and labeled columns 
  indicators <- data.frame(rsi, sto, wpr, macd, roc, obv, response,price_change)
  colnames(indicators) <- c("RSI", "StoFASTK","StoFASTD","StoSLOWD", 
                            "WilliamPR", "MACD","MACDSignal", "PriceRateOfChange", 
                            "OnBalanceVolume",
                            "Response",
							'price_change')
  #removing na values from calculations and keeping sizes of columns same
  indicators <- indicators[-1:-35,]
  
  #removing na values due to lag
  if(forforecast){indicators <- tail(indicators%>%select(-c("Response",
							'price_change')),1)}else{
  indicators <- head(indicators,-period)
  }
  return(indicators)
}
