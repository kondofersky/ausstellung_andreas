# TODO: Historisation of transactions
# TODO: Visualization per run
# TODO: faster! -> stage 1 simple things done, stage 2 profile
# TODO: bank account automatic take, i.e. 3% of every transaction or every TP

library(dplyr)
library(magrittr)
library(ranger)
library(lubridate)
library(tidyr)
#install.packages('ranger')
load('sp600dowglobal.rdata')
#load('#p600dowglobalandcustom.rdata')
# mydata %<>% filter(index != 'Steffen')
# mydata %<>% filter(index != 'Dieter')
topsyms <- mydata %>% group_by(symbol) %>% summarise(max_vol = max(volume, na.rm = T)) %>% 
  arrange(max_vol %>% desc) %>% slice(1:500) %>% pull(symbol)

mydata %>% mutate(nas = rowSums(is.na(mydata %>% select(symbol, adjusted, date)))) %>% 
  #filter(date == '2016-12-05 ') %>% View %>% 
  group_by(date) %>% summarise(nasum = sum(nas)) %>% arrange(nasum %>% desc)

rawdata <-
  mydata %>%
  arrange(date) %>% 
  filter(symbol %in% topsyms) %>% 
  filter(date>='2011-01-01') %>%
  select(c(symbol,adjusted,date)) %>% 
  pivot_wider(id_cols = date,
              values_from = adjusted,
              names_from = symbol) %>% 
  as_tibble

rawdata %<>% select(-CSR)

score1 <- matrix(NA, nc = ncol(rawdata) - 1, nr = nrow(rawdata))
for(i in 1:nrow(rawdata)){
  cat(i)
  score1[i, ] <- rawdata %>% slice(1:i) %>% 
    filter(date >= rawdata$date[i] - 6*30) %>% 
    summarise_at(-1, raster::cv, aszero = T) %>% unlist
}
colnames(score1) <- colnames(rawdata)[-1]
score1 %<>% cbind(rawdata$date, .) %>% as_tibble()
colnames(score1)[1] <- 'date'
score1 %>% slice(nrow(score1)) %>% select(-date) %>% sort(decreasing = T)


plot(rawdata$DDD, t = 'l') # hoch zum schluss
plot(rawdata$GME, t = 'l') # hoch zum schluss
plot(rawdata$ADC, t = 'l') # niedrig zum schluss
plot(rawdata$UFPI, t = 'l') # niedrig zum schluss

# rawdata
# 
# budget_original <- 1000
# tp <- .2
# sl <- .1
# single_invest <- 100
# ban_time <- 10 # not implemented, probably not needed with smart strategy
# burn_in_period <- 30
# charge <- 0.02
# max_time_hold <- 100 # not implemented

buy_func <- function(invest, symbol, dat, charge = 0.02){
  units_to_buy <- invest/dat[, symbol]*(1-charge)
  return(units_to_buy)
}

invest_strategy <- function(strategy = 'random', nr_days = 1000, verbose = F, tp, sl, single_invest,
                            burn_in_period = 30, budget_original = 1000, charge = .02, rawdata = rawdata){
  portfolio <- vector('numeric')
  nr_transactions <- nr_invests <- nr_tps <- nr_sls <- 0
    # initial investment
    burn_in_data <- slice(rawdata, 1:burn_in_period)
    invest_data <- slice(rawdata, -(1:burn_in_period))
    nr_invest <- budget_original %/% single_invest
    if(strategy == 'random') stock_invest_symbols <- colnames(invest_data)[sample((invest_data %>% slice(1) %>% is.na %>% `!` %>% which)[-1], size = nr_invest)]
    if(strategy == 'highvar') stock_invest_symbols <- score1 %>% filter(date == invest_data$date[1]) %>% select(-1) %>% unlist %>% sort(decreasing = T) %>% head(nr_invest) %>% names
    for(i in 1:length(stock_invest_symbols)){
      portfolio <- c(portfolio, buy_func(single_invest, stock_invest_symbols[i], invest_data %>% slice(1), charge = charge))
    }
    portfolio %<>% unlist
    budget <- budget_original - single_invest*nr_invest
    nr_invests <- nr_invests + nr_invest
    nr_transactions <- nr_transactions + nr_invest
    portfolio_money <- (invest_data %>% slice(1) %>% select(names(portfolio)))*portfolio
    budget_total <- budget + sum(portfolio_money)
    invest_data_back <- invest_data
    # Loop for given number of days
    for(d in 1:nr_days){  
      # update new stocks
      invest_data <- invest_data_back %>% slice(d)
      #invest_data %<>% slice(-(1:100))
      portfolio_money <- (invest_data %>% slice(1) %>% select(names(portfolio)))*portfolio
      if(length(portfolio_money) > 0) budget_total <- budget + sum(portfolio_money)

      # check if tp is reached
      tp_check <- portfolio_money/single_invest - 1 > tp#-.005
      while(any(tp_check, na.rm = T) & length(tp_check) > 0){
        if(verbose) cat('Day ', d, '. TP reached for ', (colnames(tp_check)[which(tp_check)])[1], '. Selling.\t')
        # sell stock
        budget <- budget + (invest_data %>% slice(1) %>% pull((colnames(tp_check)[which(tp_check)])[1]))*
          unname(portfolio[(colnames(tp_check)[which(tp_check)])[1]])
        # realised margin
        checkcriteria <- ((invest_data %>% slice(1) %>% pull((colnames(tp_check)[which(tp_check)])[1]))*
  unname(portfolio[(colnames(tp_check)[which(tp_check)])[1]])) / single_invest - 1
# realised margin significantly higher than tp
        if(checkcriteria>(tp+.2)){
          cat('odd profits, better check \n margin: ',checkcriteria,'\n sellprice: ',
                       (invest_data %>% slice(1) %>% pull((colnames(tp_check)[which(tp_check)])[1]))*
  unname(portfolio[(colnames(tp_check)[which(tp_check)])[1]]),'\n')
  print(invest_data_back%>%select(all_of(colnames(tp_check)[which(tp_check)][1]))%>%slice(d:(d+4)-2))
        }
                portfolio <- portfolio[-which(tp_check)[1]]
        portfolio_money <- (invest_data %>% slice(1) %>% select(names(portfolio)))*portfolio
        if(length(portfolio_money) > 0) {budget_total <- budget + sum(portfolio_money)
        }
        if(verbose) cat('New budget:', budget_total, '.\n')
        nr_transactions <- nr_transactions + 1
        nr_tps <- nr_tps + 1
        tp_check <- portfolio_money/single_invest - 1 > tp
      }

      # check if sl is reached
      sl_check <- 1 - portfolio_money/single_invest > sl
      while(any(sl_check, na.rm = T) & length(sl_check) > 0){
        if(verbose) cat('Day ', d, '. SL reached for ', (colnames(sl_check)[which(sl_check)])[1], '. Selling.\t')
        # sell stock
        budget <- budget + (invest_data %>% slice(1) %>% pull((colnames(sl_check)[which(sl_check)])[1]))*
          unname(portfolio[(colnames(sl_check)[which(sl_check)])[1]])
        portfolio <- portfolio[-which(sl_check)[1]]
        portfolio_money <- (invest_data %>% slice(1) %>% select(names(portfolio)))*portfolio
        if(length(portfolio_money)) budget_total <- budget + sum(portfolio_money)
        if(verbose) cat('New budget:', budget_total, '.\n')
        nr_transactions <- nr_transactions + 1
        nr_sls <- nr_sls + 1
        sl_check <- 1 - portfolio_money/single_invest > sl
      }

      # check if there is enough money to buy new stock (TODO: currently one transaction per day)
      if(as.logical(budget %/% single_invest %>% as.numeric)){
        if(strategy == 'random') stock_invest_symbols <- colnames(invest_data)[sample((invest_data %>% slice(1) %>% is.na %>% `!` %>% which)[-1], size = 1)]
        if(strategy == 'highvar') stock_invest_symbols <- score1 %>% filter(date == invest_data$date[1]) %>% select(-one_of(names(portfolio))) %>% select(-1) %>% unlist %>% sort(decreasing = T) %>% head(1) %>% names
        if(verbose) cat('Day ', d, '. Investing in ', stock_invest_symbols, '.\t')
        portfolio <- c(portfolio, buy_func(single_invest, stock_invest_symbols, invest_data %>% slice(1), charge = charge))
        portfolio %<>% unlist
        budget <- budget - single_invest
        portfolio_money <- (invest_data %>% slice(1) %>% select(names(portfolio)))*portfolio
        if(length(portfolio_money) > 0) budget_total <- budget + sum(portfolio_money)
        if(verbose) cat('New budget:', budget_total, '.\n')
        nr_transactions <- nr_transactions + 1
        nr_invests <- nr_invests + 1
      }
      #cat('----', budget %>% as.numeric(), '-----')
      # return
      #if(length(portfolio) == 0) 
      if(length(portfolio_money) > 0) budget_total <- budget + sum(portfolio_money)
      else budget_total <- budget
    }
    # print(list(budget_total = budget_total,
    #            budget = budget,
    #            portfolio_money = portfolio_money,
    #            final_day = d,
    #            roi_per_day_ct = (budget_total-budget_original)/d*100,
    #            yearly_gain_pct = (budget_total-budget_original)/(d/365)/budget_original*100,
    #            nr_stocks_remaining = length(portfolio_money),
    #            nr_transactions = nr_transactions,
    #            nr_invests = nr_invests,
    #            nr_tps = nr_tps,
    #            nr_sls = nr_sls
    # ) %>% unlist)
    return(list(budget_total = budget_total, 
                budget = budget, 
                portfolio_money = portfolio_money,
                final_day = d, 
                roi_per_day_ct = (budget_total-budget_original)/d*100,
                yearly_gain_pct = (budget_total-budget_original)/(d/365)/budget_original*100,
                nr_stocks_remaining = length(portfolio_money),
                nr_transactions = nr_transactions,
                nr_invests = nr_invests,
                nr_tps = nr_tps,
                nr_sls = nr_sls
    ))
}
system.time(result <- invest_strategy(burn_in_period = 6*30, tp = .1, sl = .5, single_invest = 100, rawdata = rawdata, verbose = T, charge = .02, nr_days = 1000))
result$yearly_gain_pct # 14%
system.time(result2 <- invest_strategy(burn_in_period = 6*30, tp = .25, sl = .3, single_invest = 100, rawdata = rawdata, verbose = T, charge = .02, nr_days = 500, strategy = 'highvar'))
result2$yearly_gain_pct # 43%
system.time(result3 <- invest_strategy(burn_in_period = 5.7*12*30, tp = .25, sl = .3, single_invest = 100, rawdata = rawdata, verbose = T, charge = .02, nr_days = 365, strategy = 'highvar'))
result3$yearly_gain_pct # 43%
system.time(result4 <- invest_strategy(burn_in_period = 5.7*12*30, tp = .25, sl = .5, single_invest = 100, rawdata = rawdata, verbose = T, charge = .02, nr_days = 365, strategy = 'highvar'))
result4$yearly_gain_pct # 43%
result4
system.time(result5 <- invest_strategy(burn_in_period = 7*30, tp = .25, sl = .95, single_invest = 600, rawdata = rawdata, verbose = T, charge = .02, nr_days = 1000, strategy = 'highvar',budget_original = 4000))
result5$yearly_gain_pct # 43%
result5


# rawdata %>%
#   select(date, CSR) %>%
#   slice(30+69:78) %>% View
