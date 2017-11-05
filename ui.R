library(shiny)
library(plotly)
library(shinyjs)
library(shinyBS)

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
  tags$head(
    tags$script(src="r-js.js"),
    tags$script(src="disable.js")
  ),
  extendShinyjs(
    script="www/shinycalls.js",
    functions = c(
      "otwartoModalWiersza",
      "zmienionoHaslo"
    )),

  includeCSS('styles.css'),
#   tags$head(tags$script(HTML(JScode))),

  # Application title
#   titlePanel('Hello Shiny!'),

  # Sidebar with a slider input for the number of bins

  bsModal("modalOsi", "Edytuj warunki osi", "BUTnew", size = "large",
  fluidRow(
    h4('Typ wykresu:'),
      fluidRow(
        span("aaa", style="color:white"),
        actionButton('liniowy', 'Liniowy (XY)', width='45%'),
        actionButton('slupkowy', 'Słupkowy (Y)', width='45%')
      ),
      # span("a", style="color:white"),
    column(6, os_UI('Y', list(), 'os.wynik.gm')),
    column(6, os_UI('X', list('os.licznosc'), 'os.wynik.s'))
    )
  ),

  sidebarLayout(
    conditionalPanel('output.parametry',
      sidebarPanel(
        fluidRow(
          column(2, actionButton('ukryjParametry', 'Ukryj opcje')),
          column(6,
            fluidRow(
              tags$div(tags$b(htmlOutput('pamiec')), align='center'),
              tags$div(tags$b(textOutput('czas')), align='center')
            )
          ),
          column(4, passwordInput('haslo', 'HASŁO:'))
        ),
        fluidRow(
          column(6,
            h4('Wybór populacji'),
            radioButtons('f.gl.rok',
              label = '',
              choiceNames = list(
                # 'wszystkie',
                selectInput('p.rok.g', 'koniec gim:',
                  c(
                    '2013' = 2013,
                    '2014' = 2014,
                    '2015' = 2015
                  ),
                  width="100%"
                ),
                selectInput('p.rok.m', 'koniec lic:',
                  c(
                    '2015' = 2015,
                    '2016' = 2016
                  ),
                  width="100%"
                )
              ),
              choiceValues = list(
                # 'ogol',
                '.rok.g',
                '.rok.m'
              ),
              selected = '.rok.g'
            )
          ),
           column(6,
          #   h4('Typ wykresu:'),
          #   fluidRow(
          #     actionButton('liniowy', 'Liniowy (XY)', width='49%'),
          #     actionButton('slupkowy', 'Słupkowy (Y)', width='49%')
          #   ),
          #   span("a", style="color:white"),
            fluidRow(
              column(3, tags$b("Oś Y:")),
              column(9, textOutput("opis.osi.y"))
            ),
            fluidRow(
              column(3, tags$b("Oś X:")),
              column(9, textOutput("opis.osi.x"))
            ),
            fluidRow(
              actionButton('osie', tags$b('Edytuj osie'), width='100%')
            )
          )
        ),
        # actionButton('filtryWspolne', 'Dodaj wspolne filtry', width='100%'),
        # fluidRow(
        #   column(6, os_UI('Y', list(), 'os.wynik.gm')),
        #   column(6, os_UI('X', list('os.licznosc'), 'os.wynik.s'))
        # ),
        # sliderInput('ludnosc',
        #             'Wielkosc miejscowosci:',
        #             min = 0,
        #             max = 2000000,
        #             value = c(0, 2000000)),
#         textOutput('text1'),
        fluidRow(
          column(10, actionButton('przelicz', tags$b('Przelicz'), width='100%')),
          column(2, downloadButton('pobierz', tags$b('CSV'), width='100%'))
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
