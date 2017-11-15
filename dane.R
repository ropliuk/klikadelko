
sp_rename = function(dane, from, to) {
  dane %>% rename_(.dots = setNames(list(from), nm = to))
}

# percentyle = function(dane, nkol_zr, nkol_doc) {
#   pom_perc = dane %>%
#     sp_rename(nkol_zr, 'klucz') %>%
#     group_by(klucz) %>%
#     drop_na(klucz) %>%
#     summarize(licznosc = n()) %>%
#     mutate(perc = cumsum(licznosc) / sum(licznosc)) %>%
#     sp_rename('perc', nkol_doc) %>%
#     select(-licznosc)
#   dane %>% left_join(pom_perc, by=list(x=nkol_zr, y='klucz'))
# }

percentyle = function(dane, nkol_zr, nkol_doc, nkol_gr) {
  pom_perc = dane %>%
    sp_rename(nkol_zr, '.perc.zr') %>%
    sp_rename(nkol_gr, '.perc.gr') %>%
    group_by(.perc.gr, .perc.zr) %>%
    drop_na(.perc.gr, .perc.zr) %>%
    summarize(.perc.licznosc = n()) %>%
    group_by(.perc.gr) %>%
    mutate(.perc.wynik = cumsum(.perc.licznosc) / sum(.perc.licznosc)) %>%
    ungroup() %>%
    sp_rename('.perc.wynik', nkol_doc) %>%
    sp_rename('.perc.zr', nkol_zr) %>%
    sp_rename('.perc.gr', nkol_gr) %>%
    select(-.perc.licznosc)
  dane %>% left_join(pom_perc)
}

wcisnij_w_przedzial = function(wart, dol, gora) {
  ifelse(wart < dol, dol, ifelse(wart > gora, gora, wart))
}

percentyle_rocznikowe = function(dane, nkol_zr, nkol_doc, typ_roku) {
  prog_min = ifelse(typ_roku == 's', 2010, 2013)
  prog_max = ifelse(typ_roku == 's', 2013, 2016)
  nkol_rok = paste0('rok_', typ_roku)
  dane %>%
    sp_rename(nkol_rok, '.perc.rok') %>%
    mutate(.perc.rok.efektywny = wcisnij_w_przedzial(.perc.rok, prog_min, prog_max)) %>%
    sp_rename('.perc.rok', nkol_rok) %>%
    percentyle(nkol_zr, nkol_doc, '.perc.rok.efektywny') %>%
    select(-.perc.rok.efektywny)
}
