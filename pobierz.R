source('dane.R')
source('pom.R')
source('osie.R')

zapisz.csv = function(file, czyWiersze, tab_diag, tab_opisow, rodzaj_wykresu, opis_wykresu, input) {
  tab = NULL
  for (i in 1:WIERSZE) {
    if (czyWiersze[[i]]$czy) {
      tab_dla_wiersza = tab_diag[[i]] %>%
        sp_rename('os_Y', tab_opisow[[i]])
      if (is.null(tab)) {
        tab = tab_dla_wiersza
      } else {
        tab = tab %>% full_join(tab_dla_wiersza)
      }
    }
  }

  opis_osi_X = opis.osi.krotki('X', input)
  opis_wykresu = opis.wykresu(rodzaj_wykresu, input, opis_wykresu)

  zawartosc.gl = do.call(
    paste,
    c(as.list(
      capture.output(
        tab %>%
          arrange(os_X) %>%
          sp_rename('os_X', opis_osi_X) %>%
          write.csv(stdout(), row.names=FALSE, na=''))),
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
