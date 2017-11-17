source('pom.R')
source('system/system.R')
source('warunki.R')

# setOldClass('data.frame')
# setOldClass('pairlist')
# setOldClass('plotly')
# setOldClass('reactivevalues')

# Wejscie = setRefClass('Wejscie',
#   fields = list(
#     warunki = 'list',
#     warunki_wsp = 'list',
#     input = 'reactivevalues',
#     rodzaj_wykresu = 'character', # string
#     ktore_probkowac = 'numeric',
#     czy_wiersze = 'list', # lista booleanow
#     tab_diag_zmian = 'list', # lista booleanow
#     czy_zmiana_osi = 'logical'
#   )
# )

# Seria = setRefClass('Seria', fields = list(
#   opis = 'character', # string
#   licznosc = 'numeric',
#   gl = 'data.frame',
#   licznosci = 'data.frame',
#   mat = 'data.frame'
# ))

# BladDziecka = setRefClass('BladDziecka', fields = list(
#   opis = 'character', # string
#   stos = 'pairlist'
# ))

loguj_wejscie = function(wejscie) {
  loguj(
    'grupa',
    wejscie$input$f.gl.rok,
    ifelse(wejscie$input$f.gl.rok == '.rok.g',
      wejscie$input$p.rok.g,
      wejscie$input$p.rok.m
    ))
  loguj('rodzaj_wykresu', wejscie$rodzaj_wykresu)
  loguj('os_X', wejscie$input$os.wartosc.X, wejscie$input$os.jednostka.X)
  loguj('os_Y', wejscie$input$os.wartosc.Y, wejscie$input$os.jednostka.Y)
  loguj('wiersz_wspolny', opis.dla.warunkow(wejscie$warunki_wsp))
  for (i in 1:WIERSZE) {
    if (wejscie$czy_wiersze[[i]]) {
      loguj(sprintf('wiersz%d', i), opis.dla.warunkow(wejscie$warunki[[i]]))
    }
  }
}

loguj_serie = function(wejscie, serie) {
  for (i in 1:WIERSZE) {
    if (wejscie$czy_wiersze[[i]]) {
      loguj(sprintf('seria%d_licznosc', i), serie[[i]]$licznosc)
    }
  }
}
