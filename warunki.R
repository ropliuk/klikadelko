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

lacz_warunki = function(wiersz, wierszWspolny) {
  warunki = list()
  lapply(
    c(
      lista.filtrow(wiersz),
      lista.parametrow(wiersz)
    ),
    function(nazwa) {
      warunki[[nazwa]] <<- wiersz[[nazwa]]
    }
  )
  lapply(lista.filtrow(wierszWspolny), function(nazwa) {
    if (wierszWspolny[[nazwa]] != 'ogol') {
      warunki[[nazwa]] <<- wierszWspolny[[nazwa]]
      if (!is.null(SLOWNIK_PARAMETROW[[nazwa]])) {
        lapply(SLOWNIK_PARAMETROW[[nazwa]], function(p) {
          warunki[[p]] <<- wierszWspolny[[p]]
        })
      }
    }
  })
  warunki
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
