library(dplyr)
library(pryr)
library(tidyr)

source('kolory.R')
source('osie.R')
source('postep.R')
source('pom.R')
source('system/rpc.R')
source('seria.R')
source('warunki.R')

kod_dziecka = function(wejscie) {
  laduj_dane = function() {
    postep_krok(faza='Ładuję dane')
    load('../sciezki4.RData') # ---> dane
    dane
  }

  dodaj_osie = function(dane, wejscie) {
    postep_faza('Ładuję dane')
    dane %>%
      postep_krok('Dodaję oś X') %>%
      dodaj_os('X', wejscie$input) %>%
      postep_krok('Dodaję oś Y') %>%
      dodaj_os('Y', wejscie$input)
  }

  dla_wiersza = function(dane, wejscie, nr_wiersza) {
    dane = dane[filtr.dla.warunkow(dane, wejscie$warunki[[nr_wiersza]]),]
    if (wejscie$rodzaj_wykresu == 'liniowy') {
      dane %>%
        postep_krok('Grupuję wg osi X') %>%
        group_by(os_X) %>%
        postep_krok('Filtruję wg osi X') %>%
        drop_na(os_X)
    } else {
      dane %>%
        postep_krok(i=2)
    }
  }

  oblicz_gl = function(dane, wejscie, nr_wiersza) {
    postep_faza(sprintf('Wykres główny (%d)', nr_wiersza))
    dane %>%
      postep_krok('Filtruję wg osi Y') %>%
      drop_na(os_Y) %>%
      postep_krok('Agreguję wg osi X') %>%
      os_Y_agreguj(wejscie$input$os.wartosc.Y)
  }

  oblicz_licznosci = function(dane, wejscie, nr_wiersza) {
    postep_faza(sprintf('Wykres liczności (%d)', nr_wiersza))
    if (wejscie$rodzaj_wykresu != 'liniowy') {
      data.frame()
    } else {
      dane %>%
        postep_krok('Filtruję wg osi Y') %>%
        drop_na(os_Y) %>%
        postep_krok('Obliczam liczności') %>%
        summarize(liczba_uczniow = n())
    }
  }

  czy_wykres_mat = function(wejscie, nr_wiersza) {
    !os.dot.matury(wejscie$input$os.wartosc.X) &
      os.dot.matury(wejscie$input$os.wartosc.Y) &
      !warunki.dot.matury(wejscie$warunki[[nr_wiersza]])
  }

  oblicz_mat = function(dane, wejscie, nr_wiersza) {
    postep_faza(sprintf('Wykres maturzystów (%d)', nr_wiersza))
    if (wejscie$rodzaj_wykresu != 'liniowy') {
      data.frame()
    } else if (!czy_wykres_mat(wejscie, nr_wiersza)) {
      data.frame()
    } else {
      dane %>%
        postep_krok('Obliczam prawdopodobieństwa') %>%
        summarize(liczba_uczniow = mean(is.na(wynik_mma)))
    }
  }

  wylicz_serie = function(wejscie, stare_serie) {
    postep.gl <<- postep_start(20)

    if (wejscie$czy_zmiana_osi) {
      dane <<- dane_surowe %>% dodaj_osie(wejscie)
    }

    wynik = list()

    for (nr_wiersza in 1:WIERSZE) {
      if (wejscie$czy_wiersze[[nr_wiersza]]) {
        postep_faza(sprintf('Seria danych %d', nr_wiersza))
        if (!is.null(stare_serie) && !wejscie$tab_diag_zmian[[nr_wiersza]]) {
          postep_krok(tekst='Przepisuję poprzedni wynik')
          wynik[[nr_wiersza]] = stare_serie[[nr_wiersza]]
        } else {
          dane_wiersza = dane %>% dla_wiersza(wejscie, nr_wiersza)
          licznosc = nrow(dane_wiersza)

          wynik[[nr_wiersza]] = list(
            typ = 'Seria',
            gl = dane_wiersza %>% oblicz_gl(wejscie, nr_wiersza),
            licznosci = dane_wiersza %>% oblicz_licznosci(wejscie, nr_wiersza),
            mat = dane_wiersza %>% oblicz_mat(wejscie, nr_wiersza),
            opis = opis.dla.warunkow(
              wejscie$warunki[[nr_wiersza]],
              wejscie$warunki_wsp,
              licznosc=licznosc
            ),
            licznosc=licznosc
          )
        }
      }
    }

    postep_koniec(postep.gl)

    if (!is.null(stare_serie)) {
      wyslij_do_rodzica(list(typ = 'Wynik', wynik = wynik))
    }
    wynik
  }

  # Probny blad dziecka
  # a = list()
  # print(a[[1]])

  # # Komunikat startowy - konczymy nasluch od razu
  # odbieraj_od_rodzica(function(wejscie, koniec) {
  #   serie <<- wejscie
  #   koniec()
  # })

  postep.gl <<- postep_start(4)
  dane_surowe = laduj_dane()
  dane = dane_surowe %>% dodaj_osie(wejscie)
  serie = wylicz_serie(wejscie, NULL)
  postep_koniec(postep.gl)
  wyslij_do_rodzica(list(typ = 'Koniec'))

  # Czekamy na zwykle polecenia lub polecenie konca
  odbieraj_od_rodzica(function(wejscie, koniec) {
    if (wejscie$typ == 'Wejscie') {
      serie <<- wylicz_serie(wejscie, serie)
    } else if (wejscie$typ == 'Koniec') {
      koniec()
    }
  })
}

proces_dziecka = function(wejscie) {
  withCallingHandlers(captureStackTraces(kod_dziecka(wejscie)),
    error = function(e) {
      wyslij_do_rodzica(list(
        typ = 'BladDziecka',
        opis = conditionMessage(e),
        stos = conditionStackTrace(e)
      ))
    }
  )
}
