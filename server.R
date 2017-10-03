library(data.table)
library(dplyr)
library(plotly)
library(pryr)
library(shiny)
library(shinyBS)
library(shinyjs)
library(tidyr)

source('pom.R')
source('wiersze.R')
source('filtry.R')
source('warunki.R')
source('panel.R')
source('kolory.R')
source('osie.R')
source('system.R')
source('r_js.R')

#   skala = c(0,100,200,500,1000,2000,5000,10000,20000,50000,100000,200000,500000,1000000,2000000)

# **** WCZYTANIE DANYCH ****
dane = data.frame()
load('../gimsp7.RData') # ---> dane

shinyServer(function(input, output, session) {
  session$allowReconnect(TRUE)

  stan = reactiveValues(
    rodzaj_wykresu = 'liniowy',
    wykresy = plot_ly(),
    pam_uzyta = pam_uzyta(),
    pam_cala = pam_cala(),
    ost_czas = 0
  )

  zarzadzajPanelem(input, output)
  rzeczyWPanelu(input, output, stan)

  odbierajZdarzenie(input, session, 'przelicz', function() {
    przelicz()
  })

  observeEvent(input$przelicz, {
    przelicz()
  })

  observeEvent(input$osie, {
    osie()
    js$otwartoModalWiersza('', 'modalOsi')
  })

  res = rysujWiersze(input, output)
  czyWiersze = res$czyWiersze
  wiersze = res$wiersze
  ktoreProbkowac = res$ktoreProbkowac
  wierszWspolny = res$wierszWspolny

  dane_glob = function() {
    dane %>%
      dodaj_os('X', input) %>%
      dodaj_os('Y', input) %>%
      drop_na(os_Y)
  }

#   output$text1 <- renderText({
#     paste('Wielkosc miejscowosci: od', input$ludnosc[1], 'do', input$ludnosc[2])
#   })

  osie = function(){
    print("Osie")
    toggleModal(session, "modalOsi", toggle = "open")
  }

  przelicz = function() {
    print("Przelicz")
    czas = proc.time()[['elapsed']]
    stan$wykresy = wykresy()
    stan$pam_uzyta = pam_uzyta()
    stan$pam_cala = pam_cala()
    stan$ost_czas = proc.time()[['elapsed']] - czas
  }

  wyznacz_warunki = function(nr_wiersza) {
    lacz_warunki(wiersze[[nr_wiersza]], wierszWspolny, input)
  }

  dodaj_serie = function(wykres, nr_wiersza, ktory_wykres) {
    warunki = wyznacz_warunki(nr_wiersza)

    dane_serii = dane_glob() %>%
      dane_dla_wiersza(warunki, ktory_wykres)
    dane_agr = dane_serii %>%
      os_Y_agreguj(input$os.wartosc.Y)

    opis = opis.dla.warunkow(warunki, nrow(dane_serii))

    if (ktory_wykres == 'liniowy') {
      wykres %>%
        add_trace(
          x = dane_agr$os_X,
          y = dane_agr$os_Y,
          line = list(color = kolorWykres(nr_wiersza)),
          name = opis
        )
    } else {
      wykres %>%
        add_trace(
          x = opis,
          y = dane_agr$os_Y,
          marker = list(color = kolorWykres(nr_wiersza)),
          name = opis
        )
    }
  }

  wykres_gl = function() {
    if (stan$rodzaj_wykresu == 'liniowy') {
      wykres = plot_ly(type='scatter', mode='lines', height=750)
    } else {
      wykres = plot_ly(type='bar', height = 750)
    }

    for (i in 1:WIERSZE) {
      if (czyWiersze[[i]]$czy) {
        wykres = wykres %>% dodaj_serie(i, stan$rodzaj_wykresu)
      }
    }
    wykres
  }

  # gdzie_kursor = function() {
  #   # Read in hover data
  #   eventdata <- event_data("plotly_hover", source = "source")
  #
  #   # Get point number
  #   list(
  #     point = as.numeric(eventdata$pointNumber)[1],
  #     curve = as.numeric(eventdata$curveNumber)[1]
  #   )
  # }

  wykres_dolny = function() {
    # kursor = gdzie_kursor()
    nr_wiersza = ktoreProbkowac$ktore
    warunki = wyznacz_warunki(nr_wiersza)

    wielkosci_probek = dane_glob() %>%
      dane_dla_wiersza(warunki, 'liniowy') %>%
      summarize(liczba_uczniow = n())

    wykres = plot_ly(wielkosci_probek, type='bar') %>%
      add_trace(
        x = ~os_X,
        y = ~liczba_uczniow,
        name = opis.dla.warunkow(warunki, NULL),
        marker = list(color = kolorWykres(nr_wiersza)),
        showlegend = FALSE)
    wykres
  }

  wykresy = function() {
    if (
      (input$haslo == 'okelomza') |
      (session$clientData$url_hostname == '127.0.0.1')
    ) {
      lista_wyk = list(wykres_gl())
      if (stan$rodzaj_wykresu == 'liniowy')
        lista_wyk[[2]] = wykres_dolny()
      subplot(lista_wyk, nrows=2, heights=c(0.8, 0.2))
    } else {
      plot_ly()
    }
  }

  output$plot1 = renderPlotly({
    stan$wykresy
  })

  # output$plot2 = renderPlotly({
  #   wykres_dolny()
  # })

  # output$text2 = renderText({
  #
  #   # Get window length
  #   window <- as.numeric(input$window)
  #
  #   # Show correlation heatmap
  #   paste('C', curve, 'P', point)
  # })
})
