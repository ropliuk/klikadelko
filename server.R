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
source('postep.R')
source('pobierz.R')

#   skala = c(0,100,200,500,1000,2000,5000,10000,20000,50000,100000,200000,500000,1000000,2000000)

# **** WCZYTANIE DANYCH ****
dane = data.frame()
load('../sciezki4.RData') # ---> dane

shinyServer(function(input, output, session) {
  session$allowReconnect(TRUE)

  tab_diag = list()
  tab_opisow = list()
  tab_dane_dla_serii = list()

  stan = reactiveValues(
    rodzaj_wykresu = 'liniowy',
    wykres_gl = plot_ly(),
    wykresy_dolne = list(),
    pam_uzyta = pam_uzyta(),
    pam_cala = pam_cala(),
    ost_czas = 0
  )

  ustaw_tab_diag = function() {
    lapply(1:WIERSZE, function(i) {
      tab_diag[[i]] <<- FALSE
      tab_opisow[[i]] <<- FALSE
      tab_dane_dla_serii[[i]] <<- FALSE
    })
  }

  ustaw_tab_diag()
  ustaw_tab_diag_zmian()

  zarzadzajPanelem(input, output)
  rzeczyWPanelu(input, output, stan)

  odbierajZdarzenie(input, session, 'przelicz', function() {
    przelicz()
  })

  observeEvent(input$przelicz, {
    ustaw_tab_diag_zmian()
    przelicz()
  })

  observeEvent(input$osie, {
    ustaw_tab_diag_zmian()
    pokazOsie()
    js$otwartoModalWiersza('', 'modalOsi')
  })

  observe({
    input$f.gl.rok
    input$p.rok.g
    input$p.rok.m
    ustaw_tab_diag_zmian()
    isolate(przelicz())
  })

  res = rysujWiersze(input, output)
  czyWiersze = res$czyWiersze
  wiersze = res$wiersze
  ktoreProbkowac = res$ktoreProbkowac
  wierszWspolny = res$wierszWspolny

  wylicz_dane_glob = function() {
    dane %>%
      postep_krok(postep.gl, msg='Dodaję oś X') %>%
      dodaj_os('X', input) %>%
      postep_krok(postep.gl, msg='Dodaję oś Y') %>%
      dodaj_os('Y', input)
  }

#   output$text1 <- renderText({
#     paste('Wielkosc miejscowosci: od', input$ludnosc[1], 'do', input$ludnosc[2])
#   })

  pokazOsie = function(){
    loguj('Osie')
    toggleModal(session, 'modalOsi', toggle = 'open')
  }

  przelicz = function() {
    loguj('Przelicz')
    loguj('grupa', input$f.gl.rok, 'gim', input$p.rok.g, 'mat', input$p.rok.m)
    loguj('rodzaj_wykresu', stan$rodzaj_wykresu)
    loguj('os_X', input$os.wartosc.X, input$os.jednostka.X)
    loguj('os_Y', input$os.wartosc.Y, input$os.jednostka.Y)
    czas = proc.time()[['elapsed']]

    postep.gl <<- postep_start(ile_krokow())

    dane_glob <<- wylicz_dane_glob()

    stan$wykres_gl = wykres_gl()
    stan$wykresy_dolne = wykresy_dolne()
    stan$wykresy = wykresy()
    stan$pam_uzyta = pam_uzyta()
    stan$pam_cala = pam_cala()

    postep_koniec(postep.gl)

    loguj('pamiec', 'uzyta', stan$pam_uzyta, 'cala', stan$pam_cala)
    stan$ost_czas = proc.time()[['elapsed']] - czas
  }

  wyznacz_warunki = function(nr_wiersza) {
    lacz_warunki(wiersze[[nr_wiersza]], wierszWspolny, input)
  }

  wyznacz_warunki_wsp = function(nr_wiersza) {
    lacz_warunki(list(), wierszWspolny, input)
  }

  wylicz_serie = function(nr_wiersza, ktory_wykres) {
    loguj('wylicz_serie', nr_wiersza)
    warunki = wyznacz_warunki(nr_wiersza)

    tab_dane_dla_serii[[nr_wiersza]] <<- dane_glob %>%
      postep_krok(postep.gl, msg='Filtruję wg osi Y') %>%
      dane_dla_wiersza(warunki, ktory_wykres)

    dane_serii =
      # dane_glob %>%
      # postep_krok(postep.gl, msg='Filtruję wg osi Y') %>%
      # drop_na(os_Y) %>%
      tab_dane_dla_serii[[nr_wiersza]] %>%
      postep_krok(postep.gl, msg='Filtruję wg osi Y') %>%
      # dane_dla_wiersza(warunki, ktory_wykres) %>%
      drop_na(os_Y)
    dane_agr = dane_serii %>%
      postep_krok(postep.gl, msg='Kumuluję wg osi Y') %>%
      os_Y_agreguj(input$os.wartosc.Y)

    opis = opis.dla.warunkow(warunki, wyznacz_warunki_wsp(nr_wiersza), licznosc=nrow(dane_serii))
    tab_diag[[nr_wiersza]] <<- dane_agr
    tab_opisow[[nr_wiersza]] <<- opis

    tab_diag_zmian[[nr_wiersza]] <<- FALSE
  }

  dodaj_serie = function(wykres, ktory_wykres, dane_agr, opis, nr_wiersza) {
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

  ile_krokow = function() {
    # wylicz_dane_glob
    ile_krokow = 2
    for (i in 1:WIERSZE) {
      if (czyWiersze[[i]]$czy) {
        if (tab_diag_zmian[[i]]) {
          ile_krokow = ile_krokow + 4
          warunki = wyznacz_warunki(i)
          ile_krokow = ile_krokow + liczba.filtrow.dla.warunkow(warunki)
        }
        ile_krokow = ile_krokow + 2
      }
    }
    nr_wiersza = ktoreProbkowac$ktore
    warunki = wyznacz_warunki(nr_wiersza)

    # wykres dolny
    ile_krokow = ile_krokow + 2 + liczba.filtrow.dla.warunkow(warunki)

    # wykres dolny 2
    if (czy_wykres_dolny2()) {
      ile_krokow = ile_krokow + 2 + liczba.filtrow.dla.warunkow(warunki)
    }

    # laczenie wykresow
    ile_krokow = ile_krokow + 1
    ile_krokow
  }

  wykres_gl = function() {
    loguj('wykres_gl:wierszWspolny', opis.dla.warunkow(list(), wierszWspolny))
    postep.gl$set(message='Wykres główny')

    if (stan$rodzaj_wykresu == 'liniowy') {
      wykres = plot_ly(type='scatter', mode='lines', height=750)
    } else {
      wykres = plot_ly(type='bar', height = 750)
    }

    for (i in 1:WIERSZE) {
      if (czyWiersze[[i]]$czy) {
        loguj(sprintf('wykres_gl:wiersze[[%d]]', i),
          opis.dla.warunkow(wiersze[[i]])
        )
        # wykres = wykres %>% dodaj_serie(i, stan$rodzaj_wykresu)
        if (tab_diag_zmian[[i]]) {
          wylicz_serie(i, stan$rodzaj_wykresu)
        }
        wykres = wykres %>%
          postep_krok(postep.gl, msg=sprintf('Generuję wykres dla serii %d', i)) %>%
          dodaj_serie(stan$rodzaj_wykresu, tab_diag[[i]], tab_opisow[[i]], i) %>%
          postep_krok(postep.gl, msg=sprintf('Wykres dla serii %d gotowy', i))
      }
    }

    wykres
  }

  wykres_dolny = function() {
    postep.gl$set(message='Wykres liczności')

    # kursor = gdzie_kursor()
    nr_wiersza = ktoreProbkowac$ktore
    warunki = wyznacz_warunki(nr_wiersza)

    wielkosci_probek =
      # dane_glob %>%
      # drop_na(os_Y) %>%
      # dane_dla_wiersza(warunki, 'liniowy') %>%
      tab_dane_dla_serii[[nr_wiersza]] %>%
      drop_na(os_Y) %>%
      summarize(liczba_uczniow = n())

    wykres = plot_ly(wielkosci_probek, type='bar') %>%
      add_trace(
        x = ~os_X,
        y = ~liczba_uczniow,
        name = opis.dla.warunkow(warunki),
        marker = list(color = kolorWykres(nr_wiersza)),
        showlegend = FALSE)
    wykres
  }

  czy_wykres_dolny2 = function() {
    nr_wiersza = ktoreProbkowac$ktore
    warunki = wyznacz_warunki(nr_wiersza)

    !os.dot.matury(input$os.wartosc.X) &
      os.dot.matury(input$os.wartosc.Y) &
      !warunki.dot.matury(warunki)
  }

  wykres_dolny2 = function() {
    nr_wiersza = ktoreProbkowac$ktore
    warunki = wyznacz_warunki(nr_wiersza)

    if (!czy_wykres_dolny2()) {
      NULL
    } else {
      postep.gl$set(message='Wykdane_globres maturzystów')

      procenty_bez_matury =
        print('ciap3')
        # dane_glob %>%
        # dane_dla_wiersza(warunki, 'liniowy') %>%
        tab_dane_dla_serii[[nr_wiersza]] %>%
        summarize(liczba_uczniow = mean(is.na(wynik_mma)))

      wykres = plot_ly(procenty_bez_matury, type='bar') %>%
        add_trace(
          x = ~os_X,
          y = ~liczba_uczniow,
          name = opis.dla.warunkow(warunki),
          marker = list(color = kolorWykres(nr_wiersza)),
          showlegend = FALSE)
      wykres
    }
  }

  wykresy_dolne = function() {
    list(wykres_dolny(), wykres_dolny2())
  }

  observe({
    ktoreProbkowac$ktore
    stan$wykresy_dolne = isolate(wykresy_dolne())
    stan$wykresy = isolate(wykresy())
  })

  observe({
    if (input$haslo == 'okelomza') {
      js$zmienionoHaslo()
      stan$wykresy = isolate(wykresy())
    }
  })

  wykresy = function() {
    postep.gl$set(message='Łączę wykresy', detail='')
    if (
      (input$haslo == 'okelomza') |
      (session$clientData$url_hostname == '127.0.0.1')
    ) {
      lista_wyk = list(stan$wykres_gl)
      if (stan$rodzaj_wykresu == 'liniowy') {
        lista_wyk[[2]] = stan$wykresy_dolne[[1]]
        lista_wyk[[3]] = stan$wykresy_dolne[[2]]
      }
      subplot(lista_wyk, nrows=3, heights=c(0.7, 0.15, 0.15))
    } else {
      plot_ly()
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
      zapisz.csv(file, czyWiersze, tab_diag, tab_opisow, stan$rodzaj_wykresu,
        opis.dla.warunkow(wyznacz_warunki_wsp(nr_wiersza)), input)
    }
  )

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
