library(data.table)
library(dplyr)
library(parallel)
library(plotly)
library(pryr)
library(shiny)
library(shinyBS)
library(shinyjs)
library(tidyr)

source('filtry.R')
source('kolory.R')
source('osie.R')
source('niania.R')
source('panel.R')
source('pobierz.R')
source('pom.R')
source('postep.R')
source('r_js.R')
source('system/rpc.R')
source('system/system.R')
source('warunki.R')
source('wiersze.R')
source('wykresy.R')

shinyServer(function(input, output, session) {
  session$allowReconnect(TRUE)

  stan = reactiveValues(
    rodzaj_wykresu = 'liniowy',
    wykresy = plot_ly(),
    serie = list(),
    pam_uzyta = pam_uzyta(),
    pam_dziecka = 0,
    pam_cala = pam_cala(),
    duze_dziecko = FALSE,
    ost_start = 0,
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
      if (pierwszaZmianaGrupy) {
        poczatek()
      } else {
        ustaw_tab_diag_zmian(TRUE)
        przelicz(FALSE)
      }
      pierwszaZmianaGrupy <<- FALSE
    })

  res = rysujWiersze(input, output, odswiez_wykresy)
  czyWiersze = res$czyWiersze
  wiersze = res$wiersze
  ktoreProbkowac = res$ktoreProbkowac
  wierszWspolny = res$wierszWspolny

  poczatek = function() {
    ustaw_tab_diag_zmian(TRUE)
    odswiez_dziecko()
    przelicz(TRUE)
  }

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

  oblicz_wejscie = function(czy_zmiana_osi=FALSE) {
    list(
      type = 'Wejscie',
      warunki = lapply(1:WIERSZE, wyznacz_warunki),
      warunki_wsp = wyznacz_warunki_wsp(),
      input = list(
        os.wartosc.X = input$os.wartosc.X,
        os.jednostka.X = input$os.jednostka.X,
        os.wartosc.Y = input$os.wartosc.Y,
        os.jednostka.Y = input$os.jednostka.Y
      ),
      rodzaj_wykresu = stan$rodzaj_wykresu,
      ktore_probkowac = ktoreProbkowac$ktore,
      czy_wiersze = lapply(1:WIERSZE, function(i) { czyWiersze[[i]]$czy }),
      tab_diag_zmian = tab_diag_zmian,
      czy_zmiana_osi = czy_zmiana_osi
    )
  }

  obsluz_blad = function(wyjscie) {
    if (wyjscie$typ == 'BladDziecka') {
      loguj('Blad dziecka:', wyjscie$opis)
      lapply(wyjscie$stos, function(x) { loguj('Stos:', x) })
      TRUE
    } else if (wyjscie$typ == 'LogDziecka') {
      loguj('Dziecko:', wyjscie$tekst)
      TRUE
    } else {
      FALSE
    }
  }

  obsluz_postep = function(wyjscie) {
    if (wyjscie$typ == 'Postep') {
      wyswietl_postep(wyjscie, stan)
      TRUE
    } else {
      FALSE
    }
  }

  odswiez_dziecko = function() {
    nianiu_odswiez(oblicz_wejscie())
    nianiu_odbieraj(function(wyjscie, koniec) {
      if (!obsluz_blad(wyjscie) && !obsluz_postep(wyjscie)) {
        koniec()
      }
    })
  }

  przelicz = function(czy_zmiana_osi = FALSE) {
    loguj('Przelicz')
    stan$ost_start = proc.time()[['elapsed']]

    wejscie = oblicz_wejscie(czy_zmiana_osi)
    loguj_wejscie(wejscie)

    nianiu_wyslij(wejscie)
    nianiu_odbieraj(function(wyjscie, koniec) {
      if (!obsluz_blad(wyjscie) && !obsluz_postep(wyjscie)) {
        stan$serie <<- wyjscie$wynik
        koniec()
      }
    })

    loguj('Przeliczono')
    # loguj_serie(wejscie, stan$serie)
    odswiez_wykresy()

    ustaw_tab_diag_zmian(FALSE)

    loguj('pamiec_uzyta', stan$pam_uzyta)
    loguj('pamiec_cala', stan$pam_cala)
    loguj('czas', stan$ost_czas)
  }

  czy_odswiezyc = FALSE
  observe({
    stan$duze_dziecko <<- (stan$pam_dziecka > 2)
    if (stan$duze_dziecko) {
      if (czy_odswiezyc) {
        czy_odswiezyc <<- FALSE
        isolate(odswiez_dziecko())
      }
      else {
        czy_odswiezyc <<- TRUE
        invalidateLater(5000, session)
      }
    }
  })

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
