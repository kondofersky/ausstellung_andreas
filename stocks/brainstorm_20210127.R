library(dplyr)
library(magrittr)
library(ranger)
library(lubridate)
#install.packages('ranger')
#load('stocks/data/sp600dowglobalandcustom.rdata')

rawdata <-
  mydata %>%
  #filter(symbol %in% c("AAPL","NFLX", "MSFT", "AMZN")) %>%  
  filter(date>='2015-01-01' & date <='2016-12-31') %>%
  select(c(symbol,adjusted,date)) %>% 
  mutate(adjusted = c(0, diff(adjusted)))


mydata %>% filter(symbol == 'AAPL') %>% select(date, adjusted) %>% plot
mydata %>% filter(symbol == 'AAPL') %>% filter(year(date) >= 2015 & year(date) <= 2017) %>% select(date, adjusted) %>% plot
# rawdata %>% group_by(date) %>% summarise(mean(adjusted)) %>% plot

get_model_var <- function(sym, dat){
  output <- data.frame(x1 = dat %>% filter(symbol == sym) %>% pull(adjusted) %>% lag(0),
                       x2 = dat %>% filter(symbol == sym) %>% pull(adjusted) %>% lag(1),
                       x3 = dat %>% filter(symbol == sym) %>% pull(adjusted) %>% lag(2),
                       x4 = dat %>% filter(symbol == sym) %>% pull(adjusted) %>% lag(3),
                       x5 = dat %>% filter(symbol == sym) %>% pull(adjusted) %>% lag(5),
                       x6 = dat %>% filter(symbol == sym) %>% pull(adjusted) %>% lag(7),
                       x7 = dat %>% filter(symbol == sym) %>% pull(adjusted) %>% lag(10),
                       x8 = dat %>% filter(symbol == sym) %>% pull(adjusted) %>% lag(20))
  colnames(output) <- paste0(sym, c('0', '1', '2', '3', '5', '7', '1X', '2X'))
  output %<>% slice(-(1:20))
  return(output)
}

#modeldata <- do.call(cbind, lapply(c("AAPL","NFLX", "MSFT", "AMZN"), FUN = get_model_var, dat = rawdata)) %>% as_tibble
ns <- rawdata %>% group_by(symbol) %>% summarise(n = n())
mysymbols <- rawdata %>% select(symbol) %>% distinct() %>% inner_join(ns %>% filter(n == 504) %>% select(symbol)) %>% pull(symbol)
modeldata <- do.call(cbind, lapply(mysymbols, FUN = get_model_var, dat = rawdata)) %>% as_tibble

# caret Beispiel
# trainctrl <- trainControl(method = "repeatedcv", 
#                           repeats = 1,
#                           number = 5,
#                           sampling = 'down',
#                           summaryFunction=prSummary,
#                           classProbs = TRUE,
#                           allowParallel = T,
#                           verboseIter = T)
# caretGrid <- expand.grid(mtry=c(16, 32, 64, 128), splitrule = c('gini', 'extratrees'), min.node.size = c(1))
# train$HMGIN <- factor(train$HMGIN, labels = c('c0', 'c1'))
# ran.caret <- train(
#   x = train %>% select(-(1:2)) %>% select(-HMGIN),
#   y = train %>% pull(HMGIN),
#   method = "ranger",
#   trControl = trainctrl,
#   tuneGrid=caretGrid,
#   importance = 'impurity',
#   metric = 'F' # AUC (auc_pr), F
# )
# ran.caret
# ran.caret %>% varImp()

X <- modeldata %>% select(-contains('0'))
X %<>% cbind(modeldata %>% select(AAPL0))
mod <- ranger(AAPL0 ~ ., data = X, importance = 'impurity', num.trees = 10000)
importance(mod) %>% sort(decreasing = T) %>% head(20)

mod2 <- ranger(AAPL0 ~ ., data = X %>% select(starts_with('AAPL')), importance = 'impurity', num.trees = 10000)
importance(mod2) %>% sort(decreasing = T) %>% head(20)


# prediction
predrawdata <-
  mydata %>%
  #filter(symbol %in% c("AAPL","NFLX", "MSFT", "AMZN")) %>% 
  filter(date>='2016-12-01' & date <='2017-12-31') %>%
  select(c(symbol,adjusted,date)) %>% 
  mutate(adjusted = c(0, diff(adjusted)))
predrawdata %>% group_by(date) %>% summarise(AVG = mean(adjusted)) %>% filter(AVG < 200) %>% plot
predrawdata %>% group_by(date) %>% summarise(AVG = mean(adjusted)) %>% lm(AVG ~ date, data = .) %>% abline
abline(h = 0, lty = 2)
preddata <- do.call(cbind, lapply(mysymbols, FUN = get_model_var, dat = predrawdata)) %>% as_tibble


pred <- predict(mod, data = preddata, type = 'response')$predictions
plot(preddata$AAPL0, ylim = range(c(pred, preddata$AAPL0)))
points(pred, col = 'red')

pred_train <- predict(mod, data = modeldata, type = 'response')$predictions
plot(modeldata$AAPL0, ylim = range(c(pred, modeldata$AAPL0)))
points(pred_train, col = 'red')
table(pred_train > 0, modeldata$AAPL0 > 0)


plot(c(modeldata$AAPL0, preddata$AAPL0), col = c(rep(1, nrow(modeldata)), rep(2, nrow(preddata))))

rmse <- sqrt(mean((preddata$AAPL0 - pred)^2))
rmse
# 1.499 bei 1 J und lags 0, 1, 3, 5
# 1.474 bei 2 J und lags 0, 1, 3, 5
# 1.396 bei 2 J und lags 0, bis 20

preddata %>% select(starts_with('AAPL')) %>% View
pred <- predict(mod2, data = preddata %>% select(starts_with('AAPL')), type = 'response')$predictions
plot(preddata$AAPL0, ylim = range(c(pred, preddata$AAPL0)))
points(pred, col = 'blue', pch = 19)

sum(preddata$AAPL0[pred > 0])
sum(preddata$AAPL0)
mean(preddata$AAPL0[pred > 0.075])
mean(preddata$AAPL0[pred > 0])
mean(preddata$AAPL0)

table(pred > 0, preddata$AAPL0 > 0)


plot(c(modeldata$AAPL0, preddata$AAPL0))
