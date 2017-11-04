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
source('pobierz.R')
source('przelicz.R')
source('wykresy.R')

shinyServer(function(input, output, session) {
  session$allowReconnect(TRUE)

  stan = reactiveValues(
    rodzaj_wykresu = 'liniowy',
    wykresy = plot_ly(),
    serie = list(),
    pam_uzyta = pam_uzyta(),
    pam_cala = pam_cala(),
    ost_czas = 0
  )

  zarzadzajPanelem(input, output)
  rzeczyWPanelu(input, output, stan)

  odbierajZdarzenie(input, session, 'przelicz', function(nazwaModalu) {
    przelicz(nazwaModalu == 'modalOsi')
  })

  observeEvent(input$przelicz, {
    ustaw_tab_diag_zmian(TRUE)
    przelicz(TRUE)
  })

  observeEvent(input$osie, {
    ustaw_tab_diag_zmian(TRUE)
    pokazOsie()
    js$otwartoModalWiersza('modalOsi', '')
  })

  observeEvent(input$haslo, {
    if (input$haslo == 'okelomza') {
      js$zmienionoHaslo()
    }
    odswiez_wykresy()
  })

  pierwszaZmianaGrupy = TRUE
  observeEvent(
    c(input$f.gl.rok, input$p.rok.g, input$p.rok.m),
    {
      ustaw_tab_diag_zmian(TRUE)
      przelicz(pierwszaZmianaGrupy)
      pierwszaZmianaGrupy <<- FALSE
    })

  res = rysujWiersze(input, output, odswiez_wykresy)
  czyWiersze = res$czyWiersze
  wiersze = res$wiersze
  ktoreProbkowac = res$ktoreProbkowac
  wierszWspolny = res$wierszWspolny

  pokazOsie = function(){
    loguj('Osie')
    toggleModal(session, 'modalOsi', toggle = 'open')
  }

  wyznacz_warunki = function(nr_wiersza) {
    lacz_warunki(wiersze[[nr_wiersza]], wierszWspolny, input)
  }

  wyznacz_warunki_wsp = function() {
    lacz_warunki(list(), wierszWspolny, input)
  }

  oblicz_wejscie = function() {
    Wejscie(
      warunki = lapply(1:WIERSZE, wyznacz_warunki),
      warunki_wsp = wyznacz_warunki_wsp(),
      input = input,
      rodzaj_wykresu = stan$rodzaj_wykresu,
      ktore_probkowac = ktoreProbkowac$ktore,
      czy_wiersze = lapply(1:WIERSZE, function(i) { czyWiersze[[i]]$czy })
    )
  }

  przelicz = function(czy_zmiana_osi = FALSE) {
    loguj('Przelicz')
    czas = proc.time()[['elapsed']]

    wejscie = oblicz_wejscie()
    loguj_wejscie(wejscie)
    stan$serie <<- wylicz_serie(wejscie, stan$serie, tab_diag_zmian, czy_zmiana_osi)
    loguj('Przeliczono')
    loguj_serie(wejscie, stan$serie)
    odswiez_wykresy()

    ustaw_tab_diag_zmian(FALSE)

    stan$pam_uzyta = pam_uzyta()
    stan$pam_cala = pam_cala()
    loguj('pamiec_uzyta', stan$pam_uzyta)
    loguj('pamiec_cala', stan$pam_cala)
    stan$ost_czas = proc.time()[['elapsed']] - czas
  }

  mozna_rysowac = function() {
    (input$haslo == 'okelomza') ||
      (session$clientData$url_hostname == '127.0.0.1')
  }

  odswiez_wykresy = function() {
    if (mozna_rysowac() && length(stan$serie) > 0) {
      stan$wykresy = wykresy(oblicz_wejscie(), stan$serie)
    } else {
      stan$wykresy = plot_ly()
    }
  }

  output$plot1 = renderPlotly({
    stan$wykresy
  })

  output$opis.osi.y = renderText({
    opis.osi('Y', input)
  })

  output$opis.osi.x = renderText({
    if (stan$rodzaj_wykresu == 'liniowy') {
      opis.osi('X', input)
    } else {
      '(brak)'
    }
  })

  output$pobierz = downloadHandler(
    filename = domyslna.nazwa.csv,
    content = function(file) {
      zapisz.csv(file, oblicz_wejscie(), stan$serie)
    }
  )
})
