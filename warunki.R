source('filtry.R')
source('postep_rpc.R')

is.string = function(input) {
  is.character(input) & length(input) == 1
}

czy.filtr = function(x) {
  is.string(x) & x %in% names(filtry)
}

liczba.filtrow.dla.warunkow = function(warunki) {
  wynik = 0
  for (gr in names(warunki)) {
    if (czy.filtr(warunki[[gr]])) {
      if (warunki[[gr]] != 'ogol') {
        wynik = wynik + 1
      }
    }
  }
  wynik
}

filtr.dla.warunkow = function(dane, warunki) {
  filtr = TRUE
  for (gr in names(warunki)) {
    if (czy.filtr(warunki[[gr]])) {
      if (warunki[[gr]] != 'ogol') {
        postep_krok(tekst=sprintf('Nakladam filtr: %s', warunki[[gr]]))
      }
      filtr = filtr & filtry[[ warunki[[gr]] ]](dane, warunki)
    }
  }
  filtr
}

opis.dla.warunku = function(warunki, gr) {
  res = NULL
  if (czy.filtr(warunki[[gr]])) {
    n = warunki[[gr]]
    if (n != 'ogol') {
      if (n %in% names(SLOWNIK_PARAMETROW)) {
        n = sprintf('%s(%s)', n, paste(
          lapply(SLOWNIK_PARAMETROW[[n]], function(par) {
            if (n %in% names(SLOWNIK_OPISOW)) {
              SLOWNIK_OPISOW[[n]](warunki[[par]])
            } else {
              warunki[[par]]
            }
          }),
          collapse=','))
      }
      res = n
    }
  }
  res
}

opis.dla.warunkow = function(warunki, warunki.wsp=NULL, licznosc=NULL) {
  lacz.opisy = function(opisy) {
    opisy.nietryw = opisy[lapply(opisy, function(o) { !is.null(o) }) > 0]
    if (length(opisy.nietryw) == 0) {
      'ogol'
    } else {
      do.call(paste, c(opisy.nietryw, sep='+'))
    }
  }

  if (!is.null(warunki.wsp)) {
    opisy.wsp = lapply(names(warunki.wsp), function(gr) { opis.dla.warunku(warunki.wsp, gr) })
    opisy.reszta = lapply(names(warunki), function(gr) {
      if (gr %in% names(warunki.wsp)) {
        NULL
      } else {
        opis.dla.warunku(warunki, gr)
      }
    })
    res = sprintf('[%s]+%s', lacz.opisy(opisy.wsp), lacz.opisy(opisy.reszta))
  } else {
    opisy = lapply(names(warunki), function(gr) { opis.dla.warunku(warunki, gr) })
    res = lacz.opisy(opisy)
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
      filtr = skad[[nazwa]]
      dokad[[nazwa]] <<- filtr
      if (!is.null(SLOWNIK_PARAMETROW[[filtr]])) {
        lapply(SLOWNIK_PARAMETROW[[filtr]], function(p) {
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
