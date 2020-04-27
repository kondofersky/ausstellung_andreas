# packages ----------------------------------------------------------------
library(parallel)
library(dplyr)
library(skimr)
library(ggplot2)
library(plotly)

# Standardfrage -----------------------------------------------------------
# Wie viele Duploriegel muss man kaufen um alle 32 Sammelbilder zu bekommen?
B <- 1000
N <- 32
pool <- 1:N
gezogen <- NULL
ziehen <- function(b = 1){
  while(length(unique(gezogen)) != N){
    gezogen <- c(gezogen, sample(pool, size = 1))
  }
  return(length(gezogen))
}
#system.time(ziehen(1))
system.time(result <- sapply(mclapply(1:B, ziehen, mc.cores = 1), c))
result %>% as.data.frame %>% ggplot(aes(x = .)) + 
  geom_histogram() + 
  theme_minimal() +
  ylab('Anzahl') + 
  xlab('Gegessene Duploriegel')+
  ggtitle('Ulli\'s scheiﬂ Histogram')

mean(result) # 131
median(result) # 123
skim(result)



# Christian ---------------------------------------------------------------
# Christian kauft sich 130 Duploriegel. Wie hoch ist Wkeit, dass alle 32 Bilder drin sind?
B <- 100000
N <- 32
Christian <- 130
pool <- 1:N
ziehen <- function(b = 1){
  ret <- sample(pool, size = Christian, replace = T)
  return(length(unique(ret)))
}
#system.time(ziehen(1))
system.time(result <- sapply(mclapply(1:B, ziehen, mc.cores = 1), c))
result %>% hist(col = 'blue')
mean(result == 32) # 58.25 %
skim(result)
table(result)

# Ivan --------------------------------------------------------------------
B <- 10000
N <- 32
fehlen <- 8
pool <- 1:N
gezogen <- NULL
ziehen <- function(b = 1){
  while(length(unique(gezogen[gezogen <= fehlen])) != fehlen){
    gezogen <- c(gezogen, sample(pool, size = 1))
  }
  return(length(gezogen))
}
#system.time(ziehen(1))
system.time(result <- sapply(mclapply(1:B, ziehen, mc.cores = 1), c))
result %>% hist(col = 'blue')
g <- result %>% as.data.frame %>% ggplot(aes(x = .)) + 
  geom_histogram(color = 'midnightblue', aes(y = stat(count) / sum(count)), bins = 400) + 
  geom_density(color = 'red') +
  theme_minimal() +
  ylab('Anzahl') + 
  xlab('Gegessene Duploriegel')+
  ggtitle('Ulli\'s scheiﬂ Histogram')
g
ggplotly(g)

mean(result) # Ulli sch‰tzt 52, tats‰chlich sind es 87
median(result) # 
summary(result)
