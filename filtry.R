SLOWNIK_PARAMETROW = list(
  f.rejonowosc = list('p.wedr.od.procent'),
  f.w.szk.s = list('p.w.szk.min.s', 'p.w.szk.max.s'),
  f.w.szk.g = list('p.w.szk.min.g', 'p.w.szk.max.g'),
  f.ludnosc.s = list('p.ludnosc.min.s', 'p.ludnosc.max.s'),
  f.ludnosc.g = list('p.ludnosc.min.g', 'p.ludnosc.max.g'),
  f.wynik.s.szkoly.s = list('p.wynik.s.szkoly.min.s', 'p.wynik.s.szkoly.max.s'),
  f.wynik.s.szkoly.g = list('p.wynik.s.szkoly.min.g', 'p.wynik.s.szkoly.max.g'),
  f.wynik.s = list('p.wynik.min.s', 'p.wynik.max.s'),
  f.rok.s = list('p.rok.min.s', 'p.rok.max.s'),
  f.rok.g = list('p.rok.min.g', 'p.rok.max.g'),
  f.gl.rok = list('p.rok.g', 'p.rok.m'),
  f.woj.s = list('p.woj.s', 'p.oke.s'),
  f.woj.g = list('p.woj.g', 'p.oke.g')
)

filtry = list(
  ogol = function(dane, kontekst) { TRUE },

  # CECHY SZKOLY

  miasto.s = function(dane, kontekst) { !is.na(dane$wies_sp) & dane$wies_sp == 0 },
  wies.s = function(dane, kontekst) { !is.na(dane$wies_sp) & dane$wies_sp == 1 },
  miasto.g = function(dane, kontekst) { !is.na(dane$wies_gim) & dane$wies_gim == 0 },
  wies.g = function(dane, kontekst) { !is.na(dane$wies_gim) & dane$wies_gim == 1 },

  publ.s = function(dane, kontekst) { !is.na(dane$publiczna_sp) & dane$publiczna_sp == 1 },
  pryw.s = function(dane, kontekst) { !is.na(dane$publiczna_sp) & dane$publiczna_sp == 0 },
  publ.g = function(dane, kontekst) { !is.na(dane$publiczna_gim) & dane$publiczna_gim == 1 },
  pryw.g = function(dane, kontekst) { !is.na(dane$publiczna_gim) & dane$publiczna_gim == 0 },

  w.szk.s = function(dane, kontekst) {
    dane_wielkosc = dane %>%
      group_by(id_szkoly_sp) %>%
      mutate(wielkosc_sp = n())

    (!is.na(dane$id_szkoly_sp)) &
      dane_wielkosc$wielkosc_sp >= as.numeric(kontekst$p.w.szk.min.s) &
      dane_wielkosc$wielkosc_sp <= as.numeric(kontekst$p.w.szk.max.s)
  },
  w.szk.g = function(dane, kontekst) {
    dane_wielkosc = dane %>%
      group_by(id_szkoly_gim) %>%
      mutate(wielkosc_gim = n())

    (!is.na(dane$id_szkoly_gim)) &
      dane_wielkosc$wielkosc_gim >= as.numeric(kontekst$p.w.szk.min.g) &
      dane_wielkosc$wielkosc_gim <= as.numeric(kontekst$p.w.szk.max.g)
  },

  ludnosc.s = function(dane, kontekst) {
    (!is.na(dane$ludnosc_sp)) &
      dane$ludnosc_sp >= as.numeric(kontekst$p.ludnosc.min.s) * 1000 &
      dane$ludnosc_sp <= as.numeric(kontekst$p.ludnosc.max.s) * 1000
  },
  ludnosc.g = function(dane, kontekst) {
    (!is.na(dane$ludnosc_gim)) &
      dane$ludnosc_gim >= as.numeric(kontekst$p.ludnosc.min.g) * 1000 &
      dane$ludnosc_gim <= as.numeric(kontekst$p.ludnosc.max.g) * 1000
  },

  wynik.s.szkoly.s = function(dane, kontekst) {
    dane_wynik_s = dane %>%
      group_by(id_szkoly_sp) %>%
      mutate(sr_wynik_s = mean(wynik_s, na.rm=TRUE))

    (!is.na(dane$id_szkoly_sp)) &
      (!is.na(dane_wynik_s$sr_wynik_s)) &
      dane_wynik_s$sr_wynik_s >= as.numeric(kontekst$p.wynik.s.szkoly.min.s) &
      dane_wynik_s$sr_wynik_s <= as.numeric(kontekst$p.wynik.s.szkoly.max.s)
  },
  wynik.s.szkoly.g = function(dane, kontekst) {
    dane_wynik_s = dane %>%
      group_by(id_szkoly_gim) %>%
      mutate(sr_wynik_s = mean(wynik_s, na.rm=TRUE))

    (!is.na(dane$id_szkoly_gim)) &
      (!is.na(dane_wynik_s$sr_wynik_s)) &
      dane_wynik_s$sr_wynik_s >= as.numeric(kontekst$p.wynik.s.szkoly.min.g) &
      dane_wynik_s$sr_wynik_s <= as.numeric(kontekst$p.wynik.s.szkoly.max.g)
  },

  woj.s = function(dane, kontekst) {
    (!is.na(dane$teryt_sp)) &
      dane$teryt_sp %/% 200000 == as.numeric(kontekst$p.woj.s)
  },
  woj.g = function(dane, kontekst) {
    (!is.na(dane$teryt_gim)) &
      dane$teryt_gim %/% 200000 == as.numeric(kontekst$p.woj.g)
  },

  oke.s = function(dane, kontekst) {
    (!is.na(dane$teryt_sp)) &
      unlist(strsplit(kontekst$p.oke.s, ''))[dane$teryt_sp %/% 200000] == '1'
    #  (dane$teryt_s %/% 100000) %in% kontekst$oke.s
  },
  oke.gm = function(dane, kontekst) {
    (!is.na(dane$teryt_gim)) &
      unlist(strsplit(kontekst$p.oke.g, ''))[dane$teryt_gim %/% 200000] == '1'
    #  (dane$teryt_gm %/% 100000) %in% kontekst$oke.gm
  },

  # CECHY UCZNIA W SZKOLE

  rok.s = function(dane, kontekst) {
    (!is.na(dane$rok_sp)) &
      dane$rok_sp >= as.numeric(kontekst$p.rok.min.s) &
      dane$rok_sp <= as.numeric(kontekst$p.rok.max.s)
  },
  rok.g = function(dane, kontekst) {
    wynik = c(TRUE)
    if (!is.null(kontekst$p.rok.g)) {
      wynik = wynik &
        (!is.na(dane$rok_gim)) &
        dane$rok_gim == as.numeric(kontekst$p.rok.g)
    }
    if (!is.null(kontekst$p.rok.min.g)) {
      wynik = wynik &
        (!is.na(dane$rok_gim)) &
        dane$rok_gim >= as.numeric(kontekst$p.rok.min.g) &
        dane$rok_gim <= as.numeric(kontekst$p.rok.max.g)
    }
    wynik
  },
  rok.m = function(dane, kontekst) {
    #TODO
    dane
  },

  zm.pow = function(dane, kontekst) {
    (!is.na(dane$teryt_sp)) &
      (!is.na(dane$teryt_gim)) &
      dane$teryt_sp %/% 1000 != dane$teryt_gim %/% 1000
  },
  brak.zm.pow = function(dane, kontekst) {
    (!is.na(dane$teryt_sp)) &
      (!is.na(dane$teryt_gim)) &
      dane$teryt_sp %/% 1000 == dane$teryt_gim %/% 1000
  },

  starsi =  function(dane, kontekst) { !is.na(dane$rok_sp) & !is.na(dane$rok_gim) & dane$rok_gim - dane$rok_sp > 3 },
  o.czasie = function(dane, kontekst) { !is.na(dane$rok_sp) & !is.na(dane$rok_gim) & dane$rok_gim - dane$rok_sp == 3 },
  mlodsi = function(dane, kontekst) { !is.na(dane$rok_sp) & !is.na(dane$rok_gim) & dane$rok_gim - dane$rok_sp < 3 },

  wedrowniejsi = function(dane, kontekst) {
    dane2 = dane %>%
      group_by(id_szkoly_sp) %>%
      mutate(l_kol_sp = n()) %>%
      group_by(id_szkoly_sp, id_szkoly_gim) %>%
      mutate(wsp_przech = n()/l_kol_sp)

    !is.na(dane$id_szkoly_sp) &
      !is.na(dane$id_szkoly_gim) &
      dane2$wsp_przech < 1 - as.numeric(kontekst$p.wedr.od.procent)/100
  },

  # CECHY UCZNIA NA EGZAMINIE

  dysl.s = function(dane, kontekst) { !is.na(dane$dysleksja_s) & dane$dysleksja_s == 1 },
  brak.dysl.s = function(dane, kontekst) { !is.na(dane$dysleksja_s) & dane$dysleksja_s == 0 },
  dysl.g = function(dane, kontekst) { !is.na(dane$dysleksja_gm) & dane$dysleksja_gm == 1 },
  brak.dysl.g = function(dane, kontekst) { !is.na(dane$dysleksja_gm) & dane$dysleksja_gm == 0 },

  laur.s = function(dane, kontekst){ !is.na(dane$laureat_s) & dane$laureat_s == 1},
  laur.gm = function(dane, kontekst){ !is.na(dane$laureat_gm) & dane$laureat_gm == 1},
  laur.gh = function(dane, kontekst){ !is.na(dane$laureat_gh) & dane$laureat_gh == 1},

  nie.laur.s = function(dane, kontekst){ !is.na(dane$laureat_s) & dane$laureat_s == 0},
  nie.laur.gm = function(dane, kontekst){ !is.na(dane$laureat_gm) & dane$laureat_gm == 0},
  nie.laur.gh = function(dane, kontekst){ !is.na(dane$laureat_gh) & dane$laureat_gh == 0},

  wynik.s = function(dane, kontekst) {
    (!is.na(dane$wynik_s)) &
      dane$wynik_s >= as.numeric(kontekst$p.wynik.min.s) &
      dane$wynik_s <= as.numeric(kontekst$p.wynik.max.s)
  }
)

lista.filtrow = function(input) {
  klucze = names(input)
  klucze[substr(klucze, 0, 2) == 'f.']
}

lista.parametrow = function(input) {
  klucze = names(input)
  klucze[substr(klucze, 0, 2) == 'p.']
}
