
source('dane.R')

wybory.os.wartosc = list(
  'Wynik s' = 'os.wynik.s',
  'Wynik gh' = 'os.wynik.gh',
  'Wynik gm' = 'os.wynik.gm',
  'Wynik gh+gm' = 'os.wynik.gh.gm',
  'Laureat gh' = 'os.laureat.gh',
  'Laureat gm' = 'os.laureat.gm',
  'Liczność' = 'os.licznosc'
)

wybory.os.jednostka = list(
  'Punkty' = 'os.pt',
  'Percentyle' = 'os.perc'
)

os_UI = function(nazwa_osi, wyjatki, domyslny) {
  tags$div(id = paste0('os.', nazwa_osi),
    h4(paste('Oś', nazwa_osi)),
    radioButtons(paste0('os.wartosc.', nazwa_osi),
      label = 'Wartości',
      choices = wybory.os[!(wybory.os %in% wyjatki)],
      selected = domyslny
    ),
    radioButtons(paste0('os.jednostka.', nazwa_osi),
      label = 'Skala',
      choices = wybory.os.wart,
      selected = 'os.pt'
    )
  )
}

os_logika = list(
  os.wynik.s = function(dane) {
    dane %>% mutate(os = wynik_s)
  },
  os.wynik.gh = function(dane) {
    dane %>% mutate(os = wynik_gh)
  },
  os.wynik.gm = function(dane) {
    dane %>% mutate(os = wynik_gm)
  },
  os.wynik.gh.gm = function(dane) {
    dane %>% mutate(os = wynik_gh + wynik_gm)
    # dane %>% mutate(os = (wynik_gh - wynik_gm)^2)
  },
  os.laureat.gh = function(dane) {
    dane %>% mutate(os = ifelse(!is.na(laureat_gh) & laureat_gh, 1, 0))
  },
  os.laureat.gm = function(dane) {
    dane %>% mutate(os = ifelse(!is.na(laureat_gm) & laureat_gm, 1, 0))
  },

  os.licznosc = function(dane) {
    dane %>% mutate(os = 1)
  },

  os.pt = function(dane, wartosc) {
    dane
  },
  os.perc = function(dane, wartosc) {
    spl = strsplit(wartosc, split='\\.')[[1]]
    koncowka = spl[[length(spl)]]
    typ_roku = ifelse(koncowka == 's', 's', 'gm')
    dane %>%
      percentyle_rocznikowe('os', 'os_perc', typ_roku) %>%
      select(-os) %>%
      sp_rename('os_perc', 'os')
  }
)

os_Y_agreguj = function(dane, wartosc) {
  if (wartosc %in% c('os.licznosc')) {
    dane %>% summarize(os_Y = sum(os_Y))
  } else {
    dane %>% summarize(os_Y = mean(os_Y))
  }
}

dodaj_os = function(dane, ktora_os, input) {
  wartosc = input[[paste0('os.wartosc.', ktora_os)]]
  jednostka = input[[paste0('os.jednostka.', ktora_os)]]
  dane %>%
    os_logika[[wartosc]]() %>%
    os_logika[[jednostka]](wartosc) %>%
    sp_rename('os', paste0('os_', ktora_os))
}
