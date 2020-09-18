library(jsonlite)
library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(shinydashboard)
library(data.table)
library(httr)

# functions
wd.function <- paste0(getwd(), '/Corona/functions')
files.sources <- list.files(
  wd.function,
  recursive = T,
  full.names = T,
  pattern = '*.R'
)
lapply(files.sources, source, verbose = FALSE)
# Calculating Munich Einwohner from Numbers
#einwohnerMunich<-612/41.6*100000
# source of Infections: https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0/data

einwohnerMunich <- 1471508
# calculating Schwellen- and Signalwert
grenzmean <- einwohnerMunich / 100000 * 50 / 7
grenzmeanyellow <- einwohnerMunich / 100000 * 35 / 7

# https://www.muenchen.de/rathaus/Stadtinfos/Coronavirus-Fallzahlen.html
# Die Inzidenzzahl des Landesamtes für Gesundheit und Lebensmittelsicherheit bezieht sich auf die offizielle Einwohnerzahl der Stadt München (ohne Landkreis) zum 31.12.2018: 1.471.508 Einwohner.
# Um lokale Ausbruchsereignisse rechtzeitig eindämmen zu können, wurde für die 7-Tage-Inzidenz ein Schwellenwert von 50 sowie als „Frühwarnsystem“ ein Signalwert von 35 festgelegt.
header <- dashboardHeader(title = 'Corona Numbers Munich')

sidebar <- dashboardSidebar(disable = T)

body <-
  dashboardBody(fluidPage(
    fluidRow(column(
      3,
      dateRangeInput(
        inputId = 'dates',
        label = 'Date Range',
        start = NULL,
        end = NULL,
        min = NULL,
        weekstart	= 1
      )
    )),
    fluidRow(column(
      6,
      plotlyOutput('inzidenz', width = '100%')
    ),
    column(6,
           plotlyOutput('total', width = '100%'))),
    fluidRow(column(12,
           plotlyOutput('byAge', width = '100%'))),
#    fluidRow(plotlyOutput('byGender', width = '100%')),
    fluidRow(
      box(
        title = 'Grouped per day',
        status = 'primary',
        width = 12,
        collapsible = TRUE,
        DT::dataTableOutput(outputId = 'tableout4')
      )
    ),
    fluidRow(
      box(
        title = 'Raw data',
        status = 'primary',
        width = 12,
        collapsible = TRUE,
        DT::dataTableOutput(outputId = 'tableout3')
      )
    )
  ))
#withMathJax($$\\IN$$)
ui <- fluidPage(withMathJax(),
                dashboardPage(header, sidebar, body))

server <- function(input, output, session) {
  munichdata <-
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
        bayerndata <- mydata %>% filter(Bundesland == 'Bayern')
        munichdata <- bayerndata %>% filter(Landkreis == 'SK München')
        return(munichdata)
      }
    )
  
  maxDate <- reactive({
    max(munichdata()$referencedate)
  })
  minDate <- reactive({
    min(munichdata()$referencedate)
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
  
  perday <-
    reactive({
      group_by(munichdata(), referencedate) %>% summarize(total = sum(AnzahlFall))
    })
  perdaynew <- reactive({
    perday() %>% group_by(referencedate) %>%
      mutate(
        siebentageinzidenz = round(x = getsiebentageinzidenz(referencedate, perday()), digits =
                                     1),
        siebentagetotal = getsiebentagetotal(referencedate, perday()),
        missingtoRed = round(grenzmean * 7 - siebentagetotal)
      ) %>%
      ungroup() %>%
      mutate(
        state = case_when(
          siebentageinzidenz > 50 ~ 'RED',
          siebentageinzidenz > 35 ~ 'YELLOW',
          TRUE ~ 'GREEN'
        ),
        removed = lag(x = total, n = 7),
        removednext = lag(x = total, n = 6),
        tomorrowmaxtoRed = missingtoRed + removednext
      ) %>%
      filter(referencedate >= (minDate() + 7)) %>%
      mutate(
        trend = total - removed,
        trendgoodBad = case_when(trend > 0 ~ 'BAD',
                                 TRUE ~ 'GOOD'),
        statetoday = case_when(
          total > grenzmean ~ 'RED',
          total > grenzmeanyellow ~ 'YELLOW',
          TRUE ~ 'GREEN'
        )
      ) %>%
      select(-c(trendgoodBad))
  })
  output$inzidenz <-  renderPlotly({
    plot <-
      ggplot(
        data = perdaynew() %>% filter(
          referencedate >= input$dates[1] & referencedate <= input$dates[2]
        )
        ,
        aes(x = referencedate,
            y = siebentageinzidenz)
      ) +
      geom_line() + xlab('Time') + ylab('Siebentageinzidenz') + theme_bw() +
      geom_hline(yintercept = 50, color = "red") +
      geom_hline(yintercept = 35, color = "yellow") +
      theme(
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 12)
      ) + ggtitle(paste0('Siebentageinzidenz München'))# +
    
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
      ggplot(
        data = perdaynew() %>% filter(
          referencedate >= input$dates[1] & referencedate <= input$dates[2]
        ),
        aes(x = referencedate,
            y = total)
      ) +
      coord_cartesian(ylim=c(min(perdaynew()$total)-5,max(perdaynew()$total)+5)) +
      geom_smooth(span = .3) +
      geom_line() + xlab('Time') + ylab('Fälle') + theme_bw() +
      geom_hline(yintercept = grenzmean, color = "red") +
      geom_hline(yintercept = grenzmeanyellow, color = "yellow") +
      theme(
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 12)
      ) + ggtitle(paste0('Fälle München'))# +
    
    #scale_colour_brewer(type = 'qual')
    #renderPlotly(
    ggplotly(plot)
  })
  
  output$byAge <- renderPlotly({  
  plot <-
    ggplot(
      data = munichdata() %>% group_by(referencedate, Altersgruppe) %>% summarize(total =
                                                                                  sum(AnzahlFall)),
      aes(x = referencedate,
          y = total)
    ) +
    geom_line() + ylab('Fälle') + xlab('Time')  + theme_bw() +
    scale_colour_brewer(type = 'qual') +
    facet_wrap( ~ Altersgruppe, scales = 'free_y') +
    ggtitle(paste0('Fälle nach Altersgruppe'))
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
      perdaynew() %>% filter(
        referencedate >= input$dates[1] &
          referencedate <= input$dates[2]
      ) %>% arrange(desc(referencedate)),
      extensions = 'Buttons',
      options = list(
        lengthMenu = c(5, 10, 30),
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bflrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ),
      rownames = F,
      height = 10000,
      class = 'cell-border stripe'
    )
  })
  output$tableout3 <- DT::renderDataTable({
    DT::datatable(
      munichdata() %>% filter(
        referencedate >= input$dates[1] & referencedate <= input$dates[2]
      ),
      extensions = 'Buttons',
      options = list(
        lengthMenu = c(5, 10, 30),
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bflrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ),
      rownames = F,
      height = 10000,
      class = 'cell-border stripe'
    )
  })
  
}
shinyApp(ui = ui, server = server)
