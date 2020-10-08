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
header <- dashboardHeader(title = 'Corona Numbers Germany')

sidebar <- dashboardSidebar(disable = T)

body <-
  dashboardBody(
    fluidPage(
      fluidRow(
        column(
          3,
          selectizeInput(
            inputId = 'bundesland',
            label = 'Bundesland',
            choices = landkreisedim$Bundesland,
            selected = '',
            multiple = T,
            options = list(onInitialize = I('function() { this.setValue(""); }'))
#            options = list(onInitialize = I('function() { this.setValue("Bayern"); }'))
          )
        ),
        column(3, uiOutput('landkreis')),
        column(
          3,
          dateRangeInput(
            inputId = 'dates',
            label = 'Date Range',
            start = NULL,
            end = NULL,
            min = NULL,
            weekstart	= 1
          )
        )
      ),
      fluidRow(
        box(
          title = 'First Look',
          status = 'primary',
          width = 12,
          collapsible = TRUE,
          DT::dataTableOutput(outputId = 'tableout1')
        )
      ),
      fluidRow(column(
        12,
        plotlyOutput('inzidenz', width = '100%')
      )),
      fluidRow(column(6,
                      plotlyOutput('total', width = '100%')),
               column(6,
                      plotlyOutput('over60', width = '100%'))),
      fluidRow(
        box(
          title = 'Grouped per day',
          status = 'primary',
          width = 12,
          collapsible = TRUE,
          DT::dataTableOutput(outputId = 'tableout4')
        )
      ),
      fluidRow(column(
        6,
        plotlyOutput('deaths', width = '100%')
        ),column(6,
                 plotlyOutput('deathratio', width = '100%'))),
      fluidRow(column(12,
                      plotlyOutput('byAge', width = '100%'))),
      #    fluidRow(plotlyOutput('byGender', width = '100%')),
      fluidRow(column(
        12,
        plotlyOutput('deathsbyAge', width = '100%')
      )),
      fluidRow(column(
        12,
        plotlyOutput('totalsbyBundesland', width = '100%')
      )),
      fluidRow(
        box(
          title = 'Raw data',
          status = 'primary',
          width = 12,
          collapsible = TRUE,
          DT::dataTableOutput(outputId = 'tableout3')
        )
      )
    )
  )

ui <- fluidPage(withMathJax(),
                dashboardPage(header, sidebar, body))

server <- function(input, output, session) {
  output$landkreis <- renderUI({
    mylandkreise <-
      landkreisedim %>% filter(Bundesland %in% selected_choices_bundesland())
    selectizeInput(
      inputId = 'landkreis',
      label = 'Landkreis',
      choices = mylandkreise$Landkreis,
      multiple = T,
      options = list(
        placeholder = '',
        onInitialize = I(
#          'function() { this.setValue("München, Landeshauptstadt"); }'
          'function() { this.setValue(""); }'
        )
      )
    )
  })
  selected_choices_landkreis <-
    reactive(if (identical(input$landkreis, NULL)) {
      landkreisedim$Landkreis
    } else{
      input$landkreis
    })
  selected_choices_bundesland <-
    reactive(if (identical(input$bundesland, NULL)) {
      landkreisedim$Bundesland
    } else{
      input$bundesland
    })
  
  myid <-
    reactive({
      tmppop <-
        landkreisedim %>% filter(
          Landkreis %in% selected_choices_landkreis() &
            Bundesland %in% selected_choices_bundesland()
        )
      return(tmppop$IdLandkreis)
    })
  
  mydata <-
    reactivePoll(
      intervalMillis = 5 * 60 * 1000,
      session = session,
      checkFunc = function() {
        mywebcrawl <-
          GET(
            'https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson'
          )
        return(mywebcrawl$headers$`last-modified`)
      },
      valueFunc = function() {
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
        return(mydata)
      }
    )
  currentdata <-
    reactive({
      mydata() %>% filter(IdLandkreis %in% myid())
    })
  maxDate <- reactive({
    max(mydata()$referencedate)
  })
  minDate <- reactive({
    min(mydata()$referencedate)
  })
  observe({
    updateDateRangeInput(
      session = session,
      inputId = 'dates',
      start = minDate(),
      end = maxDate(),
      min = minDate(),
      max = maxDate()
    )
  })
  
  tmppopulation <-
    reactive({
      returnvalue <- landkreisedim %>% filter(IdLandkreis %in% myid())
      return(sum(as.double(returnvalue$Population)))
    })
  # calculating Schwellen- and Signalwert
  grenzmean <- reactive(tmppopulation() / 100000 * 50 / 7)
  grenzmeanyellow <- reactive(tmppopulation() / 100000 * 35 / 7)
  
  #  observe(print(tmppopulation()))
  #  observe(tmppopulation())
  
  perday <-
    reactive({
      group_by(currentdata(), referencedate) %>% summarize(
        total = sum(AnzahlFall),
        deaths = sum(AnzahlTodesfall),
        .groups = 'drop'
      )
    })
  perdaynew <- reactive({
    perday() %>% group_by(referencedate) %>%
      mutate(
        siebentageinzidenz = round(
          x = getsiebentageinzidenzMunich(referencedate, perday(), mypopulation =
                                            tmppopulation()),
          digits =
            1
        ),
        siebentagetotal = getsiebentagetotal(referencedate, perday()),
        state = case_when(
          siebentageinzidenz > 50 ~ 'RED',
          siebentageinzidenz > 35 ~ 'YELLOW',
          TRUE ~ 'GREEN'
        ),
        missingtoRed = round(grenzmean() * 7 - siebentagetotal),
        missingtoYellow = round(grenzmeanyellow() * 7 - siebentagetotal)
      ) %>%
      ungroup() %>%
      mutate(
        removed = lag(x = total, n = 7),
        removednext = lag(x = total, n = 6),
        tomorrowmaxtoRed = missingtoRed + removednext,
        tomorrowmaxtoYellow = missingtoYellow + removednext
        
      ) %>%
      filter(referencedate >= (minDate() + 7)) %>%
      mutate(
        trend = total - removed,
        trendgoodBad = case_when(trend > 0 ~ 'BAD',
                                 TRUE ~ 'GOOD'),
        statereferencedate = case_when(
          total > grenzmean() ~ 'RED',
          total > grenzmeanyellow() ~ 'YELLOW',
          TRUE ~ 'GREEN'
        )
      ) %>%
      select(-c(trendgoodBad))
  })
  
  #  observe(print(perdaynew()))
  #  observe(print(typeof(perdaynew())))
  perdaynewdatesfiltered <- reactive(
    perdaynew() %>% filter(referencedate >= input$dates[1] &
                             referencedate <= input$dates[2]) %>% mutate(deathratio = round(deaths / total, digits = 4))
  )
  
  output$inzidenz <-  renderPlotly({
    plot <-
      ggplot(data = perdaynewdatesfiltered()
             ,
             aes(x = referencedate,
                 y = siebentageinzidenz)) +
      xlim(c(
        min(perdaynewdatesfiltered()$referencedate),
        as.Date(max(
          perdaynewdatesfiltered()$referencedate
        ) + 100)
      )) +
      coord_cartesian(ylim = c(0, 1.2*max(
        perdaynewdatesfiltered()$siebentageinzidenz
      ) +
        50)) +
      stat_smooth(
        fullrange = TRUE,
        method = 'gam',
        formula = y ~ s(x, bs = "cs")
      ) +
      geom_line() + xlab('Time') + ylab('Siebentageinzidenz') + theme_bw() +
      geom_hline(yintercept = 50, color = "red") +
      geom_hline(yintercept = 35, color = "yellow") +
      theme(
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 12)
      ) + ggtitle(paste0('Siebentageinzidenz'))# +
    
    #scale_colour_brewer(type = 'qual')
    #renderPlotly(
    ggplotly(plot)
  })
  
  #plot<-ggplot(data=perdaynew, aes(x=referencedate, y=siebentageinzidenz))+
  #  geom_line()+ xlab('Time') + ylab('Siebentageinzidenz') + theme_bw() +
  #  geom_hline(yintercept=50,color = "red") +
  #  geom_hline(yintercept=35, color = "yellow")+
  #  scale_colour_brewer(type = 'qual')+
  #  facet_wrap(~ state,scales='free_y')
  #ggplotly(plot)
  
  output$total <- renderPlotly({
    plot <-
      ggplot(data = perdaynewdatesfiltered(),
             aes(x = referencedate,
                 y = total)) +
      xlim(c(
        min(perdaynewdatesfiltered()$referencedate),
        as.Date(max(
          perdaynewdatesfiltered()$referencedate
        ) + 100)
      )) +
      coord_cartesian(ylim = c(
        0,
        1.2*max(perdaynewdatesfiltered()$total)
      )) +
      stat_smooth(
        fullrange = TRUE,
        method = 'gam',
        formula = y ~ s(x, bs = "cs")
      ) +
      geom_col() + xlab('Time') + ylab('Cases') + theme_bw() +
      geom_hline(yintercept = grenzmean(), color = "red") +
      geom_hline(yintercept = grenzmeanyellow(), color = "yellow") +
      theme(
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 12)
      ) + ggtitle(paste0('Cases'))# +
    
    #scale_colour_brewer(type = 'qual')
    #renderPlotly(
    ggplotly(plot)
  })

  output$over60 <- renderPlotly({
    thisdata <- currentdata() %>% filter(Altersgruppe %in% c('A60-A79','A80+')&referencedate >= input$dates[1] &
                                                                                            referencedate <= input$dates[2])%>%
                                  group_by(referencedate) %>% summarize(total =
                                                                            sum(AnzahlFall), .groups =
                                                                            'drop')
    
    plot <-
      ggplot(data = thisdata,
             aes(x = referencedate,
                 y = total)) +
      xlim(c(
        min(thisdata$referencedate),
        as.Date(max(
          thisdata$referencedate
        ) + 100)
      )) +
      coord_cartesian(ylim = c(
        0,
        1.2*max(thisdata$total)
      )) +
      stat_smooth(
        fullrange = TRUE,
        method = 'gam',
        formula = y ~ s(x, bs = "cs")
      ) +
      geom_col() + xlab('Time') + ylab('Cases') + theme_bw() +
#      geom_hline(yintercept = grenzmean(), color = "red") +
#      geom_hline(yintercept = grenzmeanyellow(), color = "yellow") +
      theme(
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 12)
      ) + ggtitle(paste0('Cases over 60'))# +
    
    #scale_colour_brewer(type = 'qual')
    #renderPlotly(
    ggplotly(plot)
  })
  
  output$deaths <- renderPlotly({
    plot <-
      ggplot(data = perdaynewdatesfiltered(),
             aes(x = referencedate,
                 y = deaths)) +
      xlim(c(
        min(perdaynewdatesfiltered()$referencedate),
        as.Date(max(
          perdaynewdatesfiltered()$referencedate
        ) + 100)
      )) +
      coord_cartesian(ylim = c(0, 1.2*max(perdaynewdatesfiltered()$deaths))) +
      stat_smooth(
        fullrange = TRUE,
        method = 'gam',
        formula = y ~ s(x, bs = "cs")
      ) +
      geom_col()  + xlab('Time') + ylab('Deaths') + theme_bw() +
      theme(
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 12)
      ) + ggtitle(paste0('Deaths'))# +
    
    #scale_colour_brewer(type = 'qual')
    #renderPlotly(
    ggplotly(plot)
  })
  output$deathratio <- renderPlotly({
    plot <-
      ggplot(data = perdaynewdatesfiltered(),
             aes(x = referencedate,
                 y = deathratio)) +
      xlim(c(
        min(perdaynewdatesfiltered()$referencedate),
        as.Date(max(
          perdaynewdatesfiltered()$referencedate
        ) + 100)
      )) +
      coord_cartesian(ylim = c(0, 1.2*max(
        perdaynewdatesfiltered()$deathratio
      ))) +
      stat_smooth(
        fullrange = TRUE,
        method = 'gam',
        formula = y ~ s(x, bs = "cs")
      ) +
      geom_line()  + xlab('Time') + ylab('Lethality') + theme_bw() +
      theme(
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 12)
      ) + ggtitle(paste0('Lethality')) + scale_y_continuous(labels = scales::percent)# +
    
    #scale_colour_brewer(type = 'qual')
    #renderPlotly(
    ggplotly(plot)
  })
  output$byAge <- renderPlotly({
    plot <-
      ggplot(
        data = currentdata() %>% filter(referencedate >= input$dates[1] &
                                          referencedate <= input$dates[2])%>%
          group_by(referencedate, Altersgruppe) %>% summarize(total =
                                                                                       sum(AnzahlFall), .groups =
                                                                                       'drop') %>%
          group_by(Altersgruppe) %>% mutate(totalsperAge = sum(total)) %>%
          filter(totalsperAge != 0),
        aes(x = referencedate,
            y = total)
      ) +
      geom_col() + ylab('Cases') + xlab('Time')  + theme_bw() +
      scale_colour_brewer(type = 'qual') +
      facet_wrap( ~ Altersgruppe, scales = 'free_y') +
      ggtitle(paste0('Cases by Age'))
    ggplotly(plot)
  })
  output$totalsbyBundesland <- renderPlotly({
    plot <-
      ggplot(
        data = currentdata() %>% filter(referencedate >= input$dates[1] &
                                              referencedate <= input$dates[2])%>%
          group_by(referencedate, Bundesland) %>% summarize(total =
                                                                                     sum(AnzahlFall), .groups =
                                                                                     'drop'),
        aes(x = referencedate,
            y = total)
      ) +
      geom_col() + ylab('Cases') + xlab('Time')  + theme_bw() +
      scale_colour_brewer(type = 'qual') +
      facet_wrap( ~ Bundesland, scales = 'free_y') +
      ggtitle(paste0('Cases by States'))
    ggplotly(plot)
  })
  
  output$deathsbyAge <- renderPlotly({
    plot <-
      ggplot(
        data = currentdata()%>% filter(referencedate >= input$dates[1] &
                                         referencedate <= input$dates[2])%>%
          group_by(referencedate, Altersgruppe) %>% summarize(deaths =
                                                                                       sum(AnzahlTodesfall), .groups =
                                                                                       'drop') %>%
          group_by(Altersgruppe) %>% mutate(totalsperAge = sum(deaths)) %>%
          filter(totalsperAge != 0),
        aes(x = referencedate,
            y = deaths)
      ) +
      geom_col() + ylab('Deaths') + xlab('Time')  + theme_bw() +
      scale_colour_brewer(type = 'qual') +
      facet_wrap( ~ Altersgruppe, scales = 'free_y') +
      ggtitle(paste0('Deaths by Age'))
    ggplotly(plot)
  })
  
  # output$byGender <- renderPlotly({
  # plot <-
  #   ggplot(
  #     data = munichdata() %>% group_by(referencedate, Geschlecht) %>% summarize(total =
  #                                                                               sum(AnzahlFall)),
  #     aes(x = referencedate,
  #         y = total)
  #   ) +
  #   geom_line() + xlab('Time') + ylab('Fälle nach Geschlecht') + theme_bw() +
  #   scale_colour_brewer(type = 'qual') +
  #   facet_wrap( ~ Geschlecht, scales = 'free_y')
  # ggplotly(plot)
  # })
  
  
  output$tableout4 <- DT::renderDataTable({
    DT::datatable(
      perdaynewdatesfiltered() %>% arrange(desc(referencedate)),
      extensions = 'Buttons',
      options = list(
        lengthMenu = c(7, 20, 1000),
        pageLength = 7,
        scrollX = TRUE,
        dom = 'Bflrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        columnDefs = list(list(
          targets = c(5, 13), visible = FALSE
        ))
      ),
      rownames = F,
      height = 10000,
      class = 'cell-border stripe',
      callback = JS(
        paste0(
          "
var tips = ['referencedate', 'Cases on that day; Meaning of the color: State of Siebentageinzidenz, if every day would have the same amount of cases as referencedate; Green: <= ",
          floor(grenzmeanyellow()),
          ", Red: > ",
          floor(grenzmean()),
          ", Yellow: Else',
            'Deaths on that day',
            '(cases for the last 7 days) / (population) * 100,000; Red: > 50, Green: <=35, Yellow: Else',
            'Sum of cases for the last 7 days',
            'will be removed',
            'How many more cases would have meant a siebentageinzidenz over 50',
            'How many more cases would have meant a siebentageinzidenz over 35',
            'Amount of cases from 8 days before referencedate (= number of cases that were removed from the siebentageinzidenz calculation on referencedate)',
            'Amount of cases from 7 days before referencedate (= number of cases that will be removed on the next day from the siebentageinzidenz-calculation)',
            '((missing to red) + removednext) ( = how many cases on the next day would mean a siebentageinzidenz over 50',
            '((missing to yellow) + removednext) ( = how many cases on the next day would mean a siebentageinzidenz over 35',

            'total - removed'],
    header = table.columns().header();
for (var i = 0; i < tips.length; i++) {
  $(header[i]).attr('title', tips[i]);
}
")
      )
      
      
    ) %>%
      formatStyle('siebentageinzidenz',
                  'state',
                  backgroundColor = styleEqual(c('GREEN', 'YELLOW'),
                                               c('lightgreen', 'yellow'),
                                               default = 'red')) %>%
      formatStyle('total',
                  'statereferencedate',
                  backgroundColor = styleEqual(c('GREEN', 'YELLOW'),
                                               c('lightgreen', 'yellow'),
                                               default = 'red')) %>%
      formatPercentage('deathratio', 2)
  })
  
  output$tableout3 <- DT::renderDataTable({
    DT::datatable(
      currentdata(),
      extensions = 'Buttons',
      options = list(
        lengthMenu = c(5, 100, 100000),
        pageLength = 100,
        scrollX = TRUE,
        dom = 'Bflrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ),
      rownames = F,
      height = 10000,
      class = 'cell-border stripe'
    )
  })
  
  output$tableout1 <- DT::renderDataTable({
    DT::datatable(
      cbind(
        perdaynewdatesfiltered() %>%
          arrange(desc(referencedate)) %>%
          filter(row_number()  ==  1) %>%
          select(
            siebentageinzidenz,
            tomorrowmaxtoRed,# = format(tomorrowmaxtoRed, big.mark = ","),
            tomorrowmaxtoYellow# = format(tomorrowmaxtoYellow, big.mark = ",")
          )%>%mutate(tomorrowmaxtoRed = format(tomorrowmaxtoRed, big.mark = ","),
                     tomorrowmaxtoYellow = format(tomorrowmaxtoYellow, big.mark = ",")),
        population = format(tmppopulation()
                            , big.mark = ",")
        ,
        
        perdaynewdatesfiltered()  %>%  summarize(
          total_infected  =  format(sum(total), big.mark = ","),
          total_deaths  =  format(sum(deaths), big.mark = ","),
          .groups  =  'drop'
        )
      )
      ,
      options = list(dom = 't'),
      rownames = F,
      height = 1,
      class = 'cell-border stripe'
    ) %>%
      formatStyle('siebentageinzidenz',
                  backgroundColor = styleInterval(c(35, 50),
                                                  c('lightgreen', 'yellow', 'red')#,
  #                                               default = 'red'
                                                  ))
  })
  
}
shinyApp(ui = ui, server = server)
