
sp_rename = function(dane, from, to) {
  d2 = dane
  names(d2)[names(d2) == from] = to
  d2
}

percentyle = function(dane, nkol_zr, nkol_doc) {
  pom_perc = dane %>%
    sp_rename(nkol_zr, 'klucz') %>%
    group_by(klucz) %>%
    drop_na(klucz) %>%
    summarize(licznosc = n()) %>%
    mutate(perc = cumsum(licznosc) / sum(licznosc)) %>%
    sp_rename('perc', nkol_doc) %>%
    select(-licznosc)
  dane %>% left_join(pom_perc, by=list(x=nkol_zr, y='klucz'))
}

percentyle_rocznikowe = function(dane, nkol_zr, nkol_doc, typ_roku) {
  prog_min = ifelse(typ_roku == 's', 2010, 2013)
  prog_max = ifelse(typ_roku == 's', 2013, 2016)
  nkol_rok = paste0('rok_', typ_roku)
  pom_perc = dane %>%
    sp_rename(nkol_zr, 'klucz') %>%
    sp_rename(nkol_rok, 'rok') %>%
    mutate(rok_efektywny = min(max(rok, prog_min), prog_max)) %>%
    group_by(rok_efektywny, klucz) %>%
    drop_na(klucz) %>%
    summarize(licznosc = n()) %>%
    group_by(rok_efektywny) %>%
    mutate(perc = cumsum(licznosc) / sum(licznosc)) %>%
    sp_rename('perc', nkol_doc) %>%
    sp_rename('rok', nkol_rok) %>%
    ungroup() %>%
    select(-licznosc, -rok_efektywny)
  dane %>% left_join(pom_perc, by=list(x=nkol_zr, y='klucz'))
}
