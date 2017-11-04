source('pom.R')
source('seria.R')

dodaj_serie_gl = function(wykres, rodzaj_wykresu, seria, nr_wiersza) {
  if (rodzaj_wykresu == 'liniowy') {
    wykres %>%
      add_trace(
        x = seria$gl$os_X,
        y = seria$gl$os_Y,
        line = list(color = kolorWykres(nr_wiersza)),
        name = seria$opis
      )
  } else {
    wykres %>%
      add_trace(
        x = seria$opis,
        y = seria$gl$os_Y,
        marker = list(color = kolorWykres(nr_wiersza)),
        name = seria$opis
      )
  }
}

wykres_gl = function(wejscie, serie) {
  if (wejscie$rodzaj_wykresu == 'liniowy') {
    wykres = plot_ly(type='scatter', mode='lines', height=750)
  } else {
    wykres = plot_ly(type='bar', height = 750)
  }

  for (i in 1:WIERSZE) {
    if (wejscie$czy_wiersze[[i]]) {
      wykres = wykres %>%
        dodaj_serie_gl(wejscie$rodzaj_wykresu, serie[[i]], i)
    }
  }

  wykres
}

wykres_dolny = function(dane, wejscie) {
  nr_wiersza = wejscie$ktore_probkowac

  dane %>%
  plot_ly(type='bar') %>%
  add_trace(
    x = ~os_X,
    y = ~liczba_uczniow,
    name = opis.dla.warunkow(wejscie$warunki[[nr_wiersza]]),
    marker = list(color = kolorWykres(nr_wiersza)),
    showlegend = FALSE)
}

wykres_licznosci = function(wejscie, serie) {
  nr_wiersza = wejscie$ktore_probkowac

  if (!wejscie$czy_wiersze[[nr_wiersza]]) {
    NULL
  } else {
    serie[[nr_wiersza]]$licznosci %>%
      wykres_dolny(wejscie)
  }
}

wykres_mat = function(wejscie, serie) {
  nr_wiersza = wejscie$ktore_probkowac

  if (!wejscie$czy_wiersze[[nr_wiersza]]) {
    NULL
  } else if (nrow(serie[[nr_wiersza]]$mat) == 0) {
    NULL
  } else {
    serie[[nr_wiersza]]$mat %>%
      wykres_dolny(wejscie)
  }
}

wykresy = function(wejscie, serie) {
  lista_wyk = list(wykres_gl(wejscie, serie))
  if (wejscie$rodzaj_wykresu == 'liniowy') {
    lista_wyk[[2]] = wykres_licznosci(wejscie, serie)
    lista_wyk[[3]] = wykres_mat(wejscie, serie)
  }
  subplot(lista_wyk, nrows=3, heights=c(0.7, 0.15, 0.15))
}
