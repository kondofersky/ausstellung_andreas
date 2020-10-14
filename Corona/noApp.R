# save with Encoding Windows-1252 (when done with Rscript in terminal)
# when done via Runapp in seperate R file: doch in UTF 8
print(sessionInfo())
library(jsonlite)
library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(shinydashboard)
library(DT)
library(httr)
library(openxlsx)
# Stats from 2019-12-31
population <-
  read.xlsx(
    'https://www.destatis.de/DE/Themen/Laender-Regionen/Regionales/Gemeindeverzeichnis/Administrativ/04-kreise.xlsx;jsessionid=5D16A0CE74B2CE61F0DC52ED94E6C8A5.internet8711?__blob=publicationFile',
    sheet = 2,
    startRow = 3
  )
landkreisedim <-
  population %>% select(IdLandkreis = 'Schlüssel-nummer',
                        Population = `Bevölkerung2)`,
                        Landkreis = `Kreisfreie.Stadt`) %>% filter(nchar(IdLandkreis) == 5) %>% mutate(id =
                                                                                                         c(substring(IdLandkreis, 1, 2)))
# Since the official Munich page takes Munich numbers from 2018-12-31, I use this
landkreisedim[landkreisedim$IdLandkreis == '09162',]$Population <-
  1471508

bundeslaender <-
  population %>% select(id = `Schlüssel-nummer`, Bundesland = Regionale.Bezeichnung) %>%
  filter(nchar(id) == 2)

landkreisedim <-
  left_join(landkreisedim, bundeslaender, by = c('id' = 'id'))
landkreisedim$id = NULL

populationByBundesland <- landkreisedim%>%group_by(Bundesland)%>%summarise(Population=sum(as.double(Population)),.groups='drop')
populationByLandkreis <- landkreisedim%>%group_by(IdLandkreis,Landkreis)%>%summarise(Population=sum(as.double(Population)),.groups='drop')
# functions
mywd <- getwd()
mywd <- gsub(pattern = '/Corona',replacement = '',x = mywd)
print(mywd)
wd.function <- paste0(mywd, '/Corona/functions')
print(wd.function)
files.sources <- list.files(
  wd.function,
  recursive = T,
  full.names = T,
  pattern = '*.R'
)
lapply(files.sources, source, verbose = FALSE)
# source of Infections: https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0/data

# https://www.muenchen.de/rathaus/Stadtinfos/Coronavirus-Fallzahlen.html
# Die Inzidenzzahl des Landesamtes für Gesundheit und Lebensmittelsicherheit bezieht sich auf die offizielle Einwohnerzahl der Stadt München (ohne Landkreis) zum 31.12.2018: 1.471.508 Einwohner.
# Um lokale Ausbruchsereignisse rechtzeitig eindämmen zu können, wurde für die 7-Tage-Inzidenz ein Schwellenwert von 50 sowie als „Frühwarnsystem“ ein Signalwert von 35 festgelegt.

  mydata <-
          fromJSON(
            'https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson'
          )
        mydata <- mydata[[3]]$properties
        mydata$referencedate <-
          as.Date(substring(mydata$Meldedatum, 1, 10))
        # Since population only knows whole of Berlin, I update this in the numbers from RKI (11001-11012 --> 11000)
        mydata[grepl(pattern = '110(0[1-9]|1[0-2])', x = mydata$IdLandkreis),]$IdLandkreis <-
          '11000'
        
        #bayerndata <- mydata %>% filter(Bundesland == 'Bayern')
        #munichdata <-
        #  bayerndata %>% filter(Landkreis == 'SK München')
        #return(munichdata)
        #mydata <- mydata%>%filter(IdLandkreis %in% myid())
  maxDate <- 
    max(mydata$referencedate)
  
  minDate <- 
    min(mydata$referencedate)

  tmppopulation <-

      sum(as.double(landkreisedim$Population))
  # calculating Schwellen- and Signalwert
  grenzmean <- tmppopulation / 100000 * 50 / 7
  grenzmeanyellow <-tmppopulation / 100000 * 35 / 7
  
  #  observe(print(tmppopulation()))
  #  observe(tmppopulation())
  
  perday <-
      group_by(mydata, referencedate) %>% summarize(
        total = sum(AnzahlFall),
        deaths = sum(AnzahlTodesfall),
        .groups = 'drop'
      )

  perdaynew <-
    perday %>% group_by(referencedate) %>%
      mutate(
        siebentageinzidenz = round(
          x = getsiebentageinzidenzMunich(referencedate, perday, mypopulation =
                                            tmppopulation),
          digits =
            1
        ),
        siebentagetotal = getsiebentagetotal(referencedate, perday),
        state = case_when(
          siebentageinzidenz > 50 ~ 'RED',
          siebentageinzidenz > 35 ~ 'YELLOW',
          TRUE ~ 'GREEN'
        ),
        missingtoRed = round(grenzmean * 7 - siebentagetotal),
        missingtoYellow = round(grenzmeanyellow * 7 - siebentagetotal)
      ) %>%
      ungroup() %>%
      mutate(
        removed = lag(x = total, n = 7),
        removednext = lag(x = total, n = 6),
        tomorrowmaxtoRed = missingtoRed + removednext,
        tomorrowmaxtoYellow = missingtoYellow + removednext
        
      ) %>%
      filter(referencedate >= (minDate + 7)) %>%
      mutate(
        trend = total - removed,
        trendgoodBad = case_when(trend > 0 ~ 'BAD',
                                 TRUE ~ 'GOOD'),
        statereferencedate = case_when(
          total > grenzmean ~ 'RED',
          total > grenzmeanyellow ~ 'YELLOW',
          TRUE ~ 'GREEN'
        )
      ) %>%
      select(-c(trendgoodBad))

perday
mydata

casesperstate <-
  mydata %>% filter(referencedate>=maxDate-6)%>%group_by(Bundesland) %>%summarize(
    total = sum(AnzahlFall),
#    deaths = sum(AnzahlTodesfall),
    .groups = 'drop'
  )

byBundesland<-left_join(populationByBundesland,casesperstate)%>%mutate(Inzidenz=total/Population)%>%
  select(-c(Population,total))

casesperlandkreis <-
  mydata %>% filter(referencedate>=maxDate-6)%>%group_by(IdLandkreis) %>%summarize(
    total = sum(AnzahlFall),
#    deaths = sum(AnzahlTodesfall),
    .groups = 'drop'
  )
bylandkreis <- left_join(populationByLandkreis,casesperlandkreis)%>%mutate(Inzidenz=total/Population)%>%select(-c(IdLandkreis))#,Population,total))
options(scipen=999)
ggplot(data = bylandkreis,
       aes(x = Population,
           y = Inzidenz)) + 
  geom_line() +
  geom_smooth()
