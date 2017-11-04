source('dane.R')
source('pom.R')
source('osie.R')
source('warunki.R')

zapisz.csv = function(file, wejscie, serie) {
  tab = NULL
  for (i in 1:WIERSZE) {
    if (wejscie$czy_wiersze[[i]]) {
      tab_dla_wiersza = serie[[i]]$gl %>%
        sp_rename('os_Y', serie[[i]]$opis)
      if (is.null(tab)) {
        tab = tab_dla_wiersza
      } else {
        tab = tab %>% full_join(tab_dla_wiersza)
      }
    }
  }

  opis_osi_X = opis.osi.krotki('X', wejscie$input)
  opis_wykresu = opis.wykresu(
    wejscie$rodzaj_wykresu,
    wejscie$input,
    opis.dla.warunkow(wejscie$warunki_wsp))

  zawartosc.gl = do.call(
    paste,
    c(as.list(
      capture.output(
        tab %>%
          arrange(os_X) %>%
          sp_rename('os_X', opis_osi_X) %>%
          write.table(stdout(), row.names=FALSE, na='', sep=';', dec=','))),
      sep = '\n'
    ))

  zawartosc = paste(
    opis_wykresu,
    '',
    zawartosc.gl,
    sep = '\n'
  )

  write(zawartosc, file)
}

domyslna.nazwa.csv = function() {
  format(Sys.time(), "dane_%Y-%m-%d_%H.%M.%S.csv")
}
