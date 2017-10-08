source('filtry.R')

is.string = function(input) {
  is.character(input) & length(input) == 1
}

czy.filtr = function(x) {
  is.string(x) & x %in% names(filtry)
}

filtr.dla.warunkow = function(dane, warunki) {
  filtr = TRUE
  for (gr in names(warunki)) {
    if (czy.filtr(warunki[[gr]])) {
      filtr = filtr & filtry[[ warunki[[gr]] ]](dane, warunki)
    }
  }
  filtr
}

opis.dla.warunkow = function(warunki, licznosc) {
  res = 'ogol'
  for (gr in names(warunki)) {
    if (czy.filtr(warunki[[gr]])) {
      n = warunki[[gr]]
      if (n != 'ogol') {
        if (gr %in% names(SLOWNIK_PARAMETROW)) {
          n = sprintf('%s(%s)', n, paste(
            lapply(SLOWNIK_PARAMETROW[[gr]], function(par) { warunki[[par]] }),
            collapse=','))
        }
        if (res == 'ogol') {
          res = n
        } else {
          res = sprintf('%s+%s', res, n)
        }
      }
    }
  }
  if (is.null(licznosc)) {
    res
  } else {
    sprintf('%s [%d uczniów]', res, licznosc)
  }
}

warunki.dot.matury = function(warunki) {
  for (gr in names(warunki)) {
    if (czy.filtr(warunki[[gr]])) {
      if (filtr.dot.matury(warunki[[gr]])) {
        return(TRUE)
      }
    }
  }
  FALSE
}

przepisz_warunki_jesli = function(dokad, skad, warunek_na_filtr) {
  lapply(lista.filtrow(skad), function(nazwa) {
    if (warunek_na_filtr(nazwa)) {
      dokad[[nazwa]] <<- skad[[nazwa]]
      if (!is.null(SLOWNIK_PARAMETROW[[nazwa]])) {
        lapply(SLOWNIK_PARAMETROW[[nazwa]], function(p) {
          dokad[[p]] <<- skad[[p]]
        })
      }
    }
  })
  dokad
}

przepisz_warunki = function(dokad, skad) {
  dokad %>% przepisz_warunki_jesli(skad, function(nazwa) { TRUE })
}

lacz_warunki = function(wiersz, wierszWspolny, input) {
  list() %>%
    przepisz_warunki(wiersz) %>%
    przepisz_warunki(input) %>%
    przepisz_warunki_jesli(wierszWspolny,
      function(nazwa) { wierszWspolny[[nazwa]] != 'ogol' })
}

dane_dla_wiersza = function(dane, warunki, ktory_wykres) {
  filtr = filtr.dla.warunkow(dane, warunki)
  if (ktory_wykres == 'liniowy') {
    dane[filtr,] %>%
      group_by(os_X) %>%
      drop_na(os_X)
  } else {
    dane[filtr,]
  }
}
