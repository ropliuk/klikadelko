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
  f.gl.rok.g = list('p.rok.g'),
  f.woj.s = list('p.woj.s', 'p.oke.s'),
  f.woj.g = list('p.woj.g', 'p.oke.g')
)

filtry = list(
  ogol = function(dane, kontekst) { TRUE },

  miasto.s = function(dane, kontekst) { !is.na(dane$wies_s) & dane$wies_s == 0 },
  wies.s = function(dane, kontekst) { !is.na(dane$wies_s) & dane$wies_s == 1 },
  miasto.g = function(dane, kontekst) { !is.na(dane$wies_gm) & dane$wies_gm == 0 },
  wies.g = function(dane, kontekst) { !is.na(dane$wies_gm) & dane$wies_gm == 1 },

  publ.s = function(dane, kontekst) { !is.na(dane$publiczna_s) & dane$publiczna_s == 1 },
  pryw.s = function(dane, kontekst) { !is.na(dane$publiczna_s) & dane$publiczna_s == 0 },
  publ.g = function(dane, kontekst) { !is.na(dane$publiczna_gm) & dane$publiczna_gm == 1 },
  pryw.g = function(dane, kontekst) { !is.na(dane$publiczna_gm) & dane$publiczna_gm == 0 },

  starsi =  function(dane, kontekst) { !is.na(dane$rok_s) & dane$rok_gm - dane$rok_s > 3 },
  o.czasie = function(dane, kontekst) { !is.na(dane$rok_s) & dane$rok_gm - dane$rok_s == 3 },
  mlodsi = function(dane, kontekst) { !is.na(dane$rok_s) & dane$rok_gm - dane$rok_s < 3 },

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

  wedrowniejsi = function(dane, kontekst) {
    dane2 = dane %>%
      group_by(id_szkoly_s) %>%
      mutate(l_kol_s = n()) %>%
      group_by(id_szkoly_s, id_szkoly_gm) %>%
      mutate(wsp_przech = n()/l_kol_s)

    !is.na(dane$id_szkoly_s) &
      !is.na(dane$id_szkoly_gm) &
      dane2$wsp_przech < 1 - as.numeric(kontekst$p.wedr.od.procent)/100
  },

  w.szk.s = function(dane, kontekst) {
    dane_wielkosc = dane %>%
      group_by(id_szkoly_s) %>%
      mutate(wielkosc_s = n())

    (!is.na(dane$id_szkoly_s)) &
      dane_wielkosc$wielkosc_s >= as.numeric(kontekst$p.w.szk.min.s) &
      dane_wielkosc$wielkosc_s <= as.numeric(kontekst$p.w.szk.max.s)
  },
  w.szk.g = function(dane, kontekst) {
    dane_wielkosc = dane %>%
      group_by(id_szkoly_gm) %>%
      mutate(wielkosc_gm = n())

    (!is.na(dane$id_szkoly_gm)) &
      dane_wielkosc$wielkosc_gm >= as.numeric(kontekst$p.w.szk.min.g) &
      dane_wielkosc$wielkosc_gm <= as.numeric(kontekst$p.w.szk.max.g)
  },

  ludnosc.s = function(dane, kontekst) {
    (!is.na(dane$ludnosc_s)) &
      dane$ludnosc_s >= as.numeric(kontekst$p.ludnosc.min.s) * 1000 &
      dane$ludnosc_s <= as.numeric(kontekst$p.ludnosc.max.s) * 1000
  },
  ludnosc.g = function(dane, kontekst) {
    (!is.na(dane$ludnosc_gm)) &
      dane$ludnosc_gm >= as.numeric(kontekst$p.ludnosc.min.g) * 1000 &
      dane$ludnosc_gm <= as.numeric(kontekst$p.ludnosc.max.g) * 1000
  },

  wynik.s.szkoly.s = function(dane, kontekst) {
    dane_wynik_s = dane %>%
      group_by(id_szkoly_s) %>%
      mutate(sr_wynik_s = mean(wynik_s, na.rm=TRUE))

    (!is.na(dane$id_szkoly_s)) &
      (!is.na(dane_wynik_s$sr_wynik_s)) &
      dane_wynik_s$sr_wynik_s >= as.numeric(kontekst$p.wynik.s.szkoly.min.s) &
      dane_wynik_s$sr_wynik_s <= as.numeric(kontekst$p.wynik.s.szkoly.max.s)
  },
  wynik.s.szkoly.g = function(dane, kontekst) {
    dane_wynik_s = dane %>%
      group_by(id_szkoly_gm) %>%
      mutate(sr_wynik_s = mean(wynik_s, na.rm=TRUE))

    (!is.na(dane$id_szkoly_gm)) &
      (!is.na(dane_wynik_s$sr_wynik_s)) &
      dane_wynik_s$sr_wynik_s >= as.numeric(kontekst$p.wynik.s.szkoly.min.g) &
      dane_wynik_s$sr_wynik_s <= as.numeric(kontekst$p.wynik.s.szkoly.max.g)
  },

  wynik.s = function(dane, kontekst) {
    (!is.na(dane$wynik_s)) &
      dane$wynik_s >= as.numeric(kontekst$p.wynik.min.s) &
      dane$wynik_s <= as.numeric(kontekst$p.wynik.max.s)
  },

  zm.pow = function(dane, kontekst) {
    (!is.na(dane$teryt_s)) &
      (!is.na(dane$teryt_gm)) &
      dane$teryt_s %/% 1000 != dane$teryt_gm %/% 1000
  },
  brak.zm.pow = function(dane, kontekst) {
    (!is.na(dane$teryt_s)) &
      (!is.na(dane$teryt_gm)) &
      dane$teryt_s %/% 1000 == dane$teryt_gm %/% 1000
  },

  rok.s = function(dane, kontekst) {
    (!is.na(dane$rok_s)) &
      dane$rok_s >= as.numeric(kontekst$p.rok.min.s) &
      dane$rok_s <= as.numeric(kontekst$p.rok.max.s)
  },
  rok.g = function(dane, kontekst) {
    wynik = c(TRUE)
    if (!is.null(kontekst$p.rok.g)) {
      wynik = wynik &
        (!is.na(dane$rok_gm)) &
        dane$rok_gm == as.numeric(kontekst$p.rok.g)
    }
    if (!is.null(kontekst$p.rok.min.g)) {
      wynik = wynik &
        (!is.na(dane$rok_gm)) &
        dane$rok_gm >= as.numeric(kontekst$p.rok.min.g) &
        dane$rok_gm <= as.numeric(kontekst$p.rok.max.g)
    }
    wynik
  },
  woj.s = function(dane, kontekst) {
    (!is.na(dane$teryt_s)) &
      dane$teryt_s %/% 200000 == as.numeric(kontekst$p.woj.s)
  },
  woj.g = function(dane, kontekst) {
    (!is.na(dane$teryt_gm)) &
      dane$teryt_gm %/% 200000 == as.numeric(kontekst$p.woj.g)
  },
  oke.s = function(dane, kontekst) {
    (!is.na(dane$teryt_s)) &
      unlist(strsplit(kontekst$p.oke.s, ''))[dane$teryt_s %/% 200000] == '1'
    #  (dane$teryt_s %/% 100000) %in% kontekst$oke.s
  },
  oke.gm = function(dane, kontekst) {
    (!is.na(dane$teryt_gm)) &
      unlist(strsplit(kontekst$p.oke.g, ''))[dane$teryt_gm %/% 200000] == '1'
    #  (dane$teryt_gm %/% 100000) %in% kontekst$oke.gm
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
