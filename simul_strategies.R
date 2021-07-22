# Simulate following parameters on grid:
sim_options <- expand.grid(
  nr_days_simulate = c(180, 360, 540, 720, 1000),
  tp = c(.05, .1, .15, .2, .25),
  sl = c(.2, .3, .5),
  single_invest = c(100, 200)
)

library(dplyr)
library(magrittr)
library(ranger)
library(lubridate)
library(tidyr)
#install.packages('ranger')
#load('stocks/data/sp600dowglobalandcustom.rdata')

mydata %<>% filter(index != 'Steffen')
mydata %<>% filter(index != 'Dieter')
topsyms <- mydata %>% group_by(symbol) %>% summarise(max_vol = max(volume, na.rm = T)) %>% 
  arrange(max_vol %>% desc) %>% slice(1:500) %>% pull(symbol)

mydata %>% mutate(nas = rowSums(is.na(mydata %>% select(symbol, adjusted, date)))) %>% 
  #filter(date == '2016-12-05 ') %>% View %>% 
  group_by(date) %>% summarise(nasum = sum(nas)) %>% arrange(nasum %>% desc)

rawdata <-
  mydata %>%
  arrange(date) %>% 
  filter(symbol %in% topsyms) %>% 
  #filter(date>='2012-01-01') %>%
  select(c(symbol,adjusted,date)) %>% 
  pivot_wider(id_cols = date,
              values_from = adjusted,
              names_from = symbol) %>% 
  as_tibble()

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

daterange <- rawdata %>% pull(date) %>% range

B = 50 # nr runs per row

simulate <- function(nr_days_simulate, tp, sl, single_invest){
  RES <- numeric(0)
  DAT <- integer(0); class(DAT) <- "Date"
  for(b in 1:B){
    cat(b)
    sampdat <- sample(seq(daterange[1], daterange[2] - nr_days_simulate - 1 - 30, 1), size = 1) # 30: burn-in
    tried <- try(res <- invest_strategy(strategy = 'highvar', rawdata = rawdata %>% filter(date >= sampdat), 
                           nr_days = nr_days_simulate, tp = tp, sl = sl, single_invest = single_invest)$yearly_gain_pct) # extract more info here
    if(class(tried) != 'try-error'){
      RES <- c(RES, res)
      DAT <- c(DAT, sampdat)
    } 
  }
  return(list(sampdat = DAT, mr = mean(RES), wc = min(RES), bc = max(RES), RESULTS = RES))
}

sim_result <- data.frame(sim_options, yearly_gain_pct_mean = NA, yearly_gain_pct_min = NA, yearly_gain_pct_max = NA) %>% as_tibble()
for(j in 1:150){
  cat(j, '\n')
  act_sim <- simulate(nr_days_simulate = sim_options[j, 1], tp = sim_options[j, 2], sl = sim_options[j, 3], single_invest = sim_options[j, 4])
  sim_result[j, 5] <- act_sim$mr
  sim_result[j, 6] <- act_sim$wc
  sim_result[j, 7] <- act_sim$bc
}
save(sim_result, file = paste0('stocks/sims/sim_result_', make.names(Sys.time()), '.Rdata'))
sim_result %>% group_by(tp, sl) %>% summarise(avg = mean(yearly_gain_pct_mean, na.rm = T))
sim_result %>% group_by(tp, sl, single_invest) %>% summarise(avg = mean(yearly_gain_pct_mean, na.rm = T)) %>% filter(avg > 10)
sim_result %>% View


load('stocks/sims/sim_result_X2021.05.26.11.53.34.Rdata') # random
sim_result %>% group_by(tp, sl) %>% summarise(avg = mean(yearly_gain_pct_mean, na.rm = T), wc = min(yearly_gain_pct_mean))
load('stocks/sims/sim_result_X2021.07.05.14.54.59.Rdata') # highvar
sim_result %>% group_by(tp, sl) %>% summarise(avg = mean(yearly_gain_pct_mean, na.rm = T), wc = min(yearly_gain_pct_mean))
