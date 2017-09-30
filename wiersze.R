
source('pom.R')
source('wiersz.R')
source('filtry.R')

nowa_tab_wyszarzen = function() {
  # tab_wyszarzen = list()
  # lapply(LISTA_FILTROW, function(nazwa){ tab_wyszarzen[[nazwa]] <<- FALSE })
  # tab_wyszarzen
  list()
}

tab_wyszarzen_wspolnych = nowa_tab_wyszarzen()
tab_wyszarzen = nowa_tab_wyszarzen()

rysujWiersze = function(input, output) {
  czyWiersze = lapply(1:WIERSZE, function(i) {
    reactiveValues(czy = (i == 1))
  })

  ktoreProbkowac = reactiveValues(
    ktore = 1
  )

  stan = reactiveValues(
    liczbaWierszy = 1,
    dodawalnosc = TRUE
  )

  wiersze = lapply(1:WIERSZE, function(i) {
    klucz = paste0('wiersz', i)
    callModule(wierszModul, klucz, id = klucz, numer = i, function(nazwa) {
      czy = tab_wyszarzen[[nazwa]]
      !is.null(czy) & czy
    })
  })

  wierszWspolny = callModule(
    wierszModul, 'wiersz0', id = 'wiersz0', numer = 0, function(nazwa) {
      czy = tab_wyszarzen_wspolnych[[nazwa]]
      !is.null(czy) & czy
    })

  lapply(1:WIERSZE, function(i) {
    observeEvent(wiersze[[i]]$pokazWielkoscProbek, {
      ktoreProbkowac$ktore <<- i
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
    ktoreProbkowac$ktore <<- i
  }


#  wierszModul1 = {}

  #function(input, output, session, id, numer) {
  #  observeEvent(input$edytujWiersz, {
  #    toggleModal(session, "modalnew", toggle = "open")
  #  }}

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

  observeEvent(wierszWspolny$edytujWiersz, {
    js$reactToClicksUnderRadios()
    ustaw_tab_wyszarzen_wspolnych()
  }, priority = 2)

  lapply(1:WIERSZE, function(i) {
    klucz = paste0('wiersz', i)
    observeEvent(wiersze[[i]]$edytujWiersz, {
      js$reactToClicksUnderRadios()
      ustaw_tab_wyszarzen()
    }, priority = 2)
  })

#   filtryWspolne = function() {
#   #  bsModal("modalnew", "Change name", "BUTnew", size = "large",
#   #    fluidRow(
#   #      column(2)
#   #    )
#   #  )
#
#     callModule(wierszModul1, 'wiersz1', id = 'wiersz1', numer = 1)
# #    toggleModal(session, "modalnew", toggle = "open")
#   }

  usunWiersz = function(i) {

    czyWiersze[[i]]$czy <<- FALSE
    stan$liczbaWierszy <<- stan$liczbaWierszy - 1
  }

  observeEvent(input$dodajWiersz, {
    dodajWiersz()
  })

  # observeEvent(input$filtryWspolne, {
  #   filtryWspolne()
  # })

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
