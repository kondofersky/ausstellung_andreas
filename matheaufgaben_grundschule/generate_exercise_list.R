# Skript Mathe Aufgaben zu generieren und übersichtlich aufzulisten
# Hintergrund: Grundschulkinder müssen viel Mathe üben, am besten mehr als in der Schule selbst
# Beispiele: im Internet zahlreiche Zusatzaufgabenblätter, z.B. https://www.grundschulkoenig.de/fileadmin/user_upload/mathe/klasse1/zr20/gemischteaufgaben/gemischte_aufgaben_zr20_4.pdf
# Problem: zähe Suche im Internet; Schwierigkeitsstufe oft nicht optimal; Design kann verbessert werden
# Lösung soll in pdfs, oder in einer shiny App (pro Edition :) ) je nach Auswahl verfügbar sein 

# packages ------------------------------------------------------------------------------------
library(dplyr)


# Aufgabenliste generieren --------------------------------------------------------------------
generate_exercise_list <- function(Nexercise = 3, difficulty = 1, type = 1){
  # params:
  # type (only types 1 and 2 implemented yet)
  #   1: a + b = _
  #   2: a - b = _
  #   3: a + _ = b
  #   4: a - _ = b
  #   5: _ + a = b
  #   6: _ - a = b
  #   7: mix of 1 and 2
  #   8: mix of 3, 4, 5, and 6
  #   9: mix of 7 and 8
  # difficulty
  #   1 = numbers 0-5
  #   2 = numbers 0-10
  #   3 = numbers 0-20
  #   4 = numbers 0-20
  # Nexercise - number of exercises to generate

  # decide number pool based on difficulty
  if(difficulty == 1) pool <- 0:5
  if(difficulty == 2) pool <- 0:10
  if(difficulty == 3) pool <- 0:20
  if(difficulty == 4) pool <- 5:20
  if(!difficulty %in% 1:4) stop('only diff 1 to 4 yet implemented')
  
  # generate a and b list of numbers
  a <- sample(pool, replace = T, size = Nexercise)
  b <- sample(pool, replace = T, size = Nexercise)
  # TODO: could be too many duplicated, make sure that only e.g. up to 5% are duplicated
  
  
  result <- vector(mode = 'character', length = Nexercise)
  if(type == 1){
    for(i in 1:length(a))
      result[i] <- paste(a[i], '+', b[i], '= _')
  }
  if(type == 2){
    for(i in 1:length(a))
      result[i] <- ifelse(a[i] > b[i], paste(a[i], '-', b[i], '= _'), paste(b[i], '-', a[i], '= _'))
  }
  if(!type %in% 1:2) stop('only types 1 to 2 yet implemented')
  
  class(result) <- 'matheaufgabe'
  return(result)
}


