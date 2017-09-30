library(shiny)
library(plotly)
library(shinyjs)

source('osie.R')
source('pom.R')

# JScode =
#   '$(function() {
#     skala = [0,100,200,500,1000,2000,5000,10000,20000,50000,100000,200000,500000,1000000,2000000];
#     setTimeout(function(){
#       $('#ludnosc').data('ionRangeSlider').update({'values':skala})
#     }, 5)})'

# stan = reactiveValues(
#   stan$ktory_wykres = 'liniowy'
#   )
#
#   observeEvent(input$liniowe, {
#     stan$rodzaj_wykresu <<- 'liniowy'
#   })
#
#   observeEvent(input$slupkowe, {
#     stan$rodzaj_wykresu <<-'slupkowy'
#     })

# Define UI for application that draws a histogram

shinyUI(fluidPage(
  useShinyjs(),
  tags$head(tags$script(src="disable.js")),
  extendShinyjs(script="www/shinycalls.js"),

  includeCSS('styles.css'),
#   tags$head(tags$script(HTML(JScode))),

  # Application title
#   titlePanel('Hello Shiny!'),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    conditionalPanel('output.parametry',
      sidebarPanel(
        fluidRow(
          column(2, actionButton('ukryjParametry', 'Ukryj opcje')),
          column(6,
            fluidRow(
              tags$div(tags$b(textOutput('pamiec')), align='center'),
              tags$div(tags$b(textOutput('czas')), align='center')
            )
          ),
          column(4, passwordInput('haslo', 'HASŁO:'))
        ),
        fluidRow(
          h4('Typ wykresu:'),
          actionButton('liniowy', 'Liniowy (XY)'),
          actionButton('slupkowy', 'Słupkowy (Y)')
        ),
        # actionButton('filtryWspolne', 'Dodaj wspolne filtry', width='100%'),
        fluidRow(
          column(6, os_UI('Y', list(), 'os.wynik.gm')),
          column(6, os_UI('X', list('os.licznosc'), 'os.wynik.s'))
        ),
        # sliderInput('ludnosc',
        #             'Wielkosc miejscowosci:',
        #             min = 0,
        #             max = 2000000,
        #             value = c(0, 2000000)),
#         textOutput('text1'),
        fluidRow(
          column(12, actionButton('przelicz', tags$b('Przelicz'), width='100%'))
        ),
        h4('WARUNKI WSPÓLNE'),
        uiOutput('wiersz0'),
        h4('WARUNKI SERII DANYCH'),
        lapply(1:WIERSZE, function(i) {
          uiOutput(paste0('wiersz', i))
        }),
        uiOutput('uiPokazDodaj')
        # textOutput('text2')

   #     ,
   #    actionButton("dodajWiersz", "Dodaj")
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      # stan = reactiveValues(
      #   stan$ktory_wykres = 'liniowy'
      # )

      fluidRow(
        'Źródło danych:',
        a('ZPD', href='http://zpd.ibe.edu.pl/doku.php', target="_blank"),
        '(przystosowane)'
      ),
      uiOutput('uiPokazParametry'),
#       checkboxInput("parametry", "Ukryj parametry", FALSE),
      plotlyOutput('plot1')
      # plotlyOutput('plot2')
    )
  )
))
