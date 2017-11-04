
source('pom.R')
source('wiersz.R')
source('filtry.R')

tab_diag_zmian = list()

ustaw_tab_diag_zmian = function(na_co) {
  lapply(1:WIERSZE, function(i) {
    tab_diag_zmian[[i]] <<- na_co
  })
}

nowa_tab_wyszarzen = function() {
  # tab_wyszarzen = list()
  # lapply(LISTA_FILTROW, function(nazwa){ tab_wyszarzen[[nazwa]] <<- FALSE })
  # tab_wyszarzen
  list()
}

tab_wyszarzen_wspolnych = nowa_tab_wyszarzen()
tab_wyszarzen = nowa_tab_wyszarzen()

rysujWiersze = function(input, output, odswiez_wykresy) {
  czyWiersze = lapply(1:WIERSZE, function(i) {
    reactiveValues(czy = (i == 1))
  })

  edytujWiersz = lapply(1:WIERSZE, function(i) {
    reactiveValues(edytuj = 0)
  })
  edytujWierszWspolny = reactiveValues(edytuj = 0)

  ktoreProbkowac = reactiveValues(
    ktore = 1
  )

  stan = reactiveValues(
    liczbaWierszy = 1,
    dodawalnosc = TRUE
  )

  js$otwartoModalWiersza('wiersz0', '-modalnew')
  js$otwartoModalWiersza('wiersz1', '-modalnew')

  wiersze = lapply(1:WIERSZE, function(i) {
    klucz = paste0('wiersz', i)
    callModule(wierszModul, klucz, id = klucz, numer = i, function(nazwa) {
      czy = tab_wyszarzen[[nazwa]]
      !is.null(czy) & czy
    }, edytujWiersz[[i]])
  })

  wierszWspolny = callModule(
    wierszModul, 'wiersz0', id = 'wiersz0', numer = 0, function(nazwa) {
      czy = tab_wyszarzen_wspolnych[[nazwa]]
      !is.null(czy) & czy
    }, edytujWierszWspolny)

  lapply(1:WIERSZE, function(i) {
    observeEvent(wiersze[[i]]$pokazWielkoscProbek, {
      ktoreProbkowac$ktore <<- i
      odswiez_wykresy()
    })
  })

  wierszeUI = lapply(1:WIERSZE, function(i) {
    klucz = paste0('wiersz', i)
    wierszUI(klucz)
  })

  # zastapienie tego "lapply" przez "for" nie dziala!!!
  # (wszystkie klucze okazuja sie byc rowne "wiersz10" - !?!?)
  lapply(1:WIERSZE, function(i) {
    klucz = paste0('wiersz', i)
    output[[klucz]] <<- renderUI({
      res = tagList()
      if (czyWiersze[[i]]$czy) {
        res = wierszeUI[[i]]
      }
      res
    })
  })

  output$wiersz0 = renderUI(wierszUI('wiersz0'))

  dodajWiersz = function() {
    id_wiersza = 0
    for (i in 1:WIERSZE) {
      if (!czyWiersze[[i]]$czy) {
        break
      }
    }
    if (i > WIERSZE) {
      break
    }
    czyWiersze[[i]]$czy <<- TRUE
    stan$liczbaWierszy <<- stan$liczbaWierszy + 1
    klucz = paste0('wiersz', i)
    js$otwartoModalWiersza(klucz, '-modalnew')
    edytuj_wiersz(i)
    ktoreProbkowac$ktore <<- i
  }

  czy_wyszarzyc_wspolne = function(nazwa) {
    czy = FALSE
    lapply(1:WIERSZE, function(i) {
      if (czyWiersze[[i]]$czy) {
        if ((wiersze[[i]])[[nazwa]] != 'ogol') {
          czy <<- TRUE
        }
      }
    })
    czy
  }

  czy_wyszarzyc = function(nazwa) {
    wierszWspolny[[nazwa]] != 'ogol'
  }

  ustaw_tab_wyszarzen_wspolnych = function() {
    lapply(lista.filtrow(wiersze[[1]]), function(nazwa) {
      tab_wyszarzen_wspolnych[[nazwa]] <<- czy_wyszarzyc_wspolne(nazwa)
    })
  }

  ustaw_tab_wyszarzen = function() {
    lapply(lista.filtrow(wiersze[[1]]), function(nazwa) {
      tab_wyszarzen[[nazwa]] <<- czy_wyszarzyc(nazwa)
    })
  }

  edytuj_wiersz = function(nr_wiersza) {
    ustaw_tab_wyszarzen()
    tab_diag_zmian[[nr_wiersza]] <<- TRUE
    edytujWiersz[[nr_wiersza]]$edytuj <<- edytujWiersz[[nr_wiersza]]$edytuj + 1
  }

  observeEvent(wierszWspolny$edytujWiersz, {
    ustaw_tab_wyszarzen_wspolnych()
    ustaw_tab_diag_zmian(TRUE)
    edytujWierszWspolny$edytuj <<- edytujWierszWspolny$edytuj + 1
  })

  lapply(1:WIERSZE, function(i) {
    observeEvent(wiersze[[i]]$edytujWiersz, {
      edytuj_wiersz(i)
    })
  })

  usunWiersz = function(i) {
    czyWiersze[[i]]$czy <<- FALSE
    stan$liczbaWierszy <<- stan$liczbaWierszy - 1
    odswiez_wykresy()
  }

  observeEvent(input$dodajWiersz, {
    dodajWiersz()
  })

  lapply(1:WIERSZE, function(i) {
    observeEvent(wiersze[[i]]$usunWiersz, {
      usunWiersz(i)
    })
  })

  output$uiPokazDodaj <- renderUI({
    if (stan$liczbaWierszy < WIERSZE) {
      actionButton('dodajWiersz', 'Dodaj', width='100%')
    }
  })

  list(
    czyWiersze = czyWiersze,
    wiersze = wiersze,
    ktoreProbkowac = ktoreProbkowac,
    wierszWspolny = wierszWspolny
  )
}
