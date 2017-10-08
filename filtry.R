SLOWNIK_PARAMETROW = list(
  f.rejonowosc.g = list('p.wedr.od.procent.g'),
  f.rejonowosc.m = list('p.wedr.od.procent.m'),
  f.w.szk.s = list('p.w.szk.min.s', 'p.w.szk.max.s'),
  f.w.szk.g = list('p.w.szk.min.g', 'p.w.szk.max.g'),
  f.w.szk.m = list('p.w.szk.min.m', 'p.w.szk.max.m'),
  f.ludnosc.s = list('p.ludnosc.min.s', 'p.ludnosc.max.s'),
  f.ludnosc.g = list('p.ludnosc.min.g', 'p.ludnosc.max.g'),
  f.ludnosc.m = list('p.ludnosc.min.m', 'p.ludnosc.max.m'),
  f.wynik.s.szkoly.s = list('p.wynik.s.szkoly.min.s', 'p.wynik.s.szkoly.max.s'),
  f.wynik.s.szkoly.g = list('p.wynik.s.szkoly.min.g', 'p.wynik.s.szkoly.max.g'),
  f.wynik.s.szkoly.m = list('p.wynik.s.szkoly.min.m', 'p.wynik.s.szkoly.max.m'),
  f.wynik.s = list('p.wynik.min.s', 'p.wynik.max.s'),
  f.wynik.gh = list('p.wynik.min.gh', 'p.wynik.max.gh'),
  f.wynik.gm = list('p.wynik.min.gm', 'p.wynik.max.gm'),
  f.rok.s = list('p.rok.min.s', 'p.rok.max.s'),
  f.rok.g = list('p.rok.min.g', 'p.rok.max.g'),
  f.rok.m = list('p.rok.min.m', 'p.rok.max.m'),
  f.gl.rok = list('p.rok.g', 'p.rok.m'),
  f.woj.s = list('p.woj.s', 'p.oke.s'),
  f.woj.g = list('p.woj.g', 'p.oke.g'),
  f.woj.m = list('p.woj.m', 'p.oke.m'),
  f.typ.szkoly.m = list('p.typ.szkoly.m')
)

pom_filtr_wynik_X_szkoly_Y = function(X, Y, dane_wynik, kontekst) {
  (!is.na(dane_wynik$sr_wynik)) &
    dane_wynik$sr_wynik >=
      as.numeric(kontekst[[sprintf("p.wynik.%s.szkoly.min.%s", X, Y)]]) &
    dane_wynik$sr_wynik <=
      as.numeric(kontekst[[sprintf("p.wynik.%s.szkoly.max.%s", X, Y)]])
}

filtry = list(
  ogol = function(dane, kontekst) { TRUE },

  # CECHY SZKOLY

  miasto.s = function(dane, kontekst) { !is.na(dane$wies_sp) & dane$wies_sp == 0 },
  wies.s = function(dane, kontekst) { !is.na(dane$wies_sp) & dane$wies_sp == 1 },
  miasto.g = function(dane, kontekst) { !is.na(dane$wies_gim) & dane$wies_gim == 0 },
  wies.g = function(dane, kontekst) { !is.na(dane$wies_gim) & dane$wies_gim == 1 },
  miasto.m = function(dane, kontekst) { !is.na(dane$wies_mat) & dane$wies_mat == 0 },
  wies.m = function(dane, kontekst) { !is.na(dane$wies_mat) & dane$wies_mat == 1 },

  publ.s = function(dane, kontekst) { !is.na(dane$publiczna_sp) & dane$publiczna_sp == 1 },
  pryw.s = function(dane, kontekst) { !is.na(dane$publiczna_sp) & dane$publiczna_sp == 0 },
  publ.g = function(dane, kontekst) { !is.na(dane$publiczna_gim) & dane$publiczna_gim == 1 },
  pryw.g = function(dane, kontekst) { !is.na(dane$publiczna_gim) & dane$publiczna_gim == 0 },
  publ.m = function(dane, kontekst) { !is.na(dane$publiczna_mat) & dane$publiczna_mat == 1 },
  pryw.m = function(dane, kontekst) { !is.na(dane$publiczna_mat) & dane$publiczna_mat == 0 },

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
  w.szk.m = function(dane, kontekst) {
    dane_wielkosc = dane %>%
      group_by(id_szkoly_mat) %>%
      mutate(wielkosc_mat = n())

    (!is.na(dane$id_szkoly_mat)) &
      dane_wielkosc$wielkosc_mat >= as.numeric(kontekst$p.w.szk.min.m) &
      dane_wielkosc$wielkosc_mat <= as.numeric(kontekst$p.w.szk.max.m)
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
  ludnosc.m = function(dane, kontekst) {
    (!is.na(dane$ludnosc_mat)) &
      dane$ludnosc_mat >= as.numeric(kontekst$p.ludnosc.min.m) * 1000 &
      dane$ludnosc_mat <= as.numeric(kontekst$p.ludnosc.max.m) * 1000
  },

  wynik.s.szkoly.s = function(dane, kontekst) {
    dane_wynik = dane %>%
      group_by(id_szkoly_sp) %>%
      mutate(sr_wynik = mean(wynik_s, na.rm=TRUE))

    (!is.na(dane$id_szkoly_sp)) &
      pom_filtr_wynik_X_szkoly_Y(dane_wynik, kontekst, 's', 's')
  },
  wynik.s.szkoly.g = function(dane, kontekst) {
    dane_wynik = dane %>%
      group_by(id_szkoly_gim) %>%
      mutate(sr_wynik = mean(wynik_s, na.rm=TRUE))

    (!is.na(dane$id_szkoly_gim)) &
      pom_filtr_wynik_X_szkoly_Y(dane_wynik, kontekst, 's', 'g')
  },
  wynik.s.szkoly.m = function(dane, kontekst) {
    dane_wynik = dane %>%
      group_by(id_szkoly_mat) %>%
      mutate(sr_wynik = mean(wynik_s, na.rm=TRUE))

    (!is.na(dane$id_szkoly_mat)) &
      pom_filtr_wynik_X_szkoly_Y(dane_wynik, kontekst, 's', 'm')
  },
  wynik.gh.szkoly.m = function(dane, kontekst) {
    dane_wynik = dane %>%
      group_by(id_szkoly_mat) %>%
      mutate(sr_wynik = mean(wynik_gh, na.rm=TRUE))

    (!is.na(dane$id_szkoly_mat)) &
      pom_filtr_wynik_X_szkoly_Y(dane_wynik, kontekst, 'gh', 'm')
  },
  wynik.gm.szkoly.m = function(dane, kontekst) {
    dane_wynik = dane %>%
      group_by(id_szkoly_mat) %>%
      mutate(sr_wynik = mean(wynik_gm, na.rm=TRUE))

    (!is.na(dane$id_szkoly_mat)) &
      pom_filtr_wynik_X_szkoly_Y(dane_wynik, kontekst, 'gm', 'm')
  },

  woj.s = function(dane, kontekst) {
    (!is.na(dane$teryt_sp)) &
      dane$teryt_sp %/% 200000 == as.numeric(kontekst$p.woj.s)
  },
  woj.g = function(dane, kontekst) {
    (!is.na(dane$teryt_gim)) &
      dane$teryt_gim %/% 200000 == as.numeric(kontekst$p.woj.g)
  },
  woj.m = function(dane, kontekst) {
    (!is.na(dane$teryt_mat)) &
      dane$teryt_mat %/% 200000 == as.numeric(kontekst$p.woj.m)
  },

  oke.s = function(dane, kontekst) {
    (!is.na(dane$teryt_sp)) &
      unlist(strsplit(kontekst$p.oke.s, ''))[dane$teryt_sp %/% 200000] == '1'
  },
  oke.g = function(dane, kontekst) {
    (!is.na(dane$teryt_gim)) &
      unlist(strsplit(kontekst$p.oke.g, ''))[dane$teryt_gim %/% 200000] == '1'
  },
  oke.m = function(dane, kontekst) {
    (!is.na(dane$teryt_mat)) &
      unlist(strsplit(kontekst$p.oke.m, ''))[dane$teryt_mat %/% 200000] == '1'
  },

  typ.szkoly.m = function(dane, kontekst) {
    (!is.na(dane$typ_szkoly_mat)) &
      dane$typ_szkoly_mat == kontekst$p.typ.szkoly.m
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
    wynik = c(TRUE)
    if (!is.null(kontekst$p.rok.m)) {
      wynik = wynik &
        (!is.na(dane$rok_mat)) &
        dane$rok_mat == as.numeric(kontekst$p.rok.m)
    }
    if (!is.null(kontekst$p.rok.min.m)) {
      wynik = wynik &
        (!is.na(dane$rok_mat)) &
        dane$rok_mat >= as.numeric(kontekst$p.rok.min.m) &
        dane$rok_mat <= as.numeric(kontekst$p.rok.max.m)
    }
    wynik
  },

  zm.pow.g = function(dane, kontekst) {
    (!is.na(dane$teryt_sp)) &
      (!is.na(dane$teryt_gim)) &
      dane$teryt_sp %/% 1000 != dane$teryt_gim %/% 1000
  },
  brak.zm.pow.g = function(dane, kontekst) {
    (!is.na(dane$teryt_sp)) &
      (!is.na(dane$teryt_gim)) &
      dane$teryt_sp %/% 1000 == dane$teryt_gim %/% 1000
  },
  zm.pow.m = function(dane, kontekst) {
    (!is.na(dane$teryt_gim)) &
      (!is.na(dane$teryt_mat)) &
      dane$teryt_gim %/% 1000 != dane$teryt_mat %/% 1000
  },
  brak.zm.pow.m = function(dane, kontekst) {
    (!is.na(dane$teryt_gim)) &
      (!is.na(dane$teryt_mat)) &
      dane$teryt_gim %/% 1000 == dane$teryt_mat %/% 1000
  },

  starsi.g =  function(dane, kontekst) {
    !is.na(dane$rok_sp) &
      !is.na(dane$rok_gim) &
      dane$rok_gim - dane$rok_sp > 3
  },
  o.czasie.g = function(dane, kontekst) {
    !is.na(dane$rok_sp) &
      !is.na(dane$rok_gim) &
      dane$rok_gim - dane$rok_sp == 3
  },
  mlodsi.g = function(dane, kontekst) {
    !is.na(dane$rok_sp) &
      !is.na(dane$rok_gim) &
      dane$rok_gim - dane$rok_sp < 3
  },
  starsi.m =  function(dane, kontekst) {
    !is.na(dane$rok_gim) &
      !is.na(dane$rok_mat) &
      !is.na(dane$typ_szkoly_mat) &
      (
        ((dane$typ_szkoly_mat == 'T') & (dane$rok_mat - dane$rok_gim > 4)) |
        ((dane$typ_szkoly_mat %in% c('LO', 'LP', 'ZZ') &
          (dane$rok_mat - dane$rok_gim > 3))) |
        ((dane$typ_szkoly_mat %in% c('TU', 'LOU')))
      )
  },
  o.czasie.m = function(dane, kontekst) {
    !is.na(dane$rok_gim) &
      !is.na(dane$rok_mat) &
      !is.na(dane$typ_szkoly_mat) &
      (
        ((dane$typ_szkoly_mat == 'T') & (dane$rok_mat - dane$rok_gim == 4)) |
        ((dane$typ_szkoly_mat %in% c('LO', 'LP', 'ZZ') &
          (dane$rok_mat - dane$rok_gim == 3)))
      )
  },
  mlodsi.m = function(dane, kontekst) {
    !is.na(dane$rok_gim) &
      !is.na(dane$rok_mat) &
      !is.na(dane$typ_szkoly_mat) &
      (
        ((dane$typ_szkoly_mat == 'T') & (dane$rok_mat - dane$rok_gim < 4)) |
        ((dane$typ_szkoly_mat %in% c('LO', 'LP', 'ZZ') &
          (dane$rok_mat - dane$rok_gim < 3)))
      )
  },

  wedrowniejsi.g = function(dane, kontekst) {
    dane2 = dane %>%
      group_by(id_szkoly_sp) %>%
      mutate(l_kol_sp = n()) %>%
      group_by(id_szkoly_sp, id_szkoly_gim) %>%
      mutate(wsp_przech = n()/l_kol_sp)

    !is.na(dane$id_szkoly_sp) &
      !is.na(dane$id_szkoly_gim) &
      dane2$wsp_przech < 1 - as.numeric(kontekst$p.wedr.od.procent.g)/100
  },
  wedrowniejsi.m = function(dane, kontekst) {
    dane2 = dane %>%
      group_by(id_szkoly_gim) %>%
      mutate(l_kol_gim = n()) %>%
      group_by(id_szkoly_gim, id_szkoly_mat) %>%
      mutate(wsp_przech = n()/l_kol_gim)

    !is.na(dane$id_szkoly_gim) &
      !is.na(dane$id_szkoly_mat) &
      dane2$wsp_przech < 1 - as.numeric(kontekst$p.wedr.od.procent.m)/100
  },

  # CECHY UCZNIA NA EGZAMINIE

  dysl.s = function(dane, kontekst) { !is.na(dane$dysleksja_s) & dane$dysleksja_s == 1 },
  brak.dysl.s = function(dane, kontekst) { !is.na(dane$dysleksja_s) & dane$dysleksja_s == 0 },
  dysl.g = function(dane, kontekst) { !is.na(dane$dysleksja_gm) & dane$dysleksja_gm == 1 },
  brak.dysl.g = function(dane, kontekst) { !is.na(dane$dysleksja_gm) & dane$dysleksja_gm == 0 },
  dysl.m = function(dane, kontekst) { !is.na(dane$dysleksja_mpo) & dane$dysleksja_mpo == 1 },
  brak.dysl.m = function(dane, kontekst) { !is.na(dane$dysleksja_mpo) & dane$dysleksja_mpo == 0 },

  laur.s = function(dane, kontekst){ !is.na(dane$laureat_s) & dane$laureat_s == 1},
  nie.laur.s = function(dane, kontekst){ !is.na(dane$laureat_s) & dane$laureat_s == 0},
  laur.gm = function(dane, kontekst){ !is.na(dane$laureat_gm) & dane$laureat_gm == 1},
  nie.laur.gm = function(dane, kontekst){ !is.na(dane$laureat_gm) & dane$laureat_gm == 0},
  laur.gh = function(dane, kontekst){ !is.na(dane$laureat_gh) & dane$laureat_gh == 1},
  nie.laur.gh = function(dane, kontekst){ !is.na(dane$laureat_gh) & dane$laureat_gh == 0},
  laur.mpo = function(dane, kontekst){ !is.na(dane$laureat_mpo) & dane$laureat_mpo == 1},
  nie.laur.mpo = function(dane, kontekst){ !is.na(dane$laureat_mpo) & dane$laureat_mpo == 0},
  laur.mma = function(dane, kontekst){ !is.na(dane$laureat_mma) & dane$laureat_mma == 1},
  nie.laur.mma = function(dane, kontekst){ !is.na(dane$laureat_mma) & dane$laureat_mma == 0},
  laur.mja = function(dane, kontekst){ !is.na(dane$laureat_mja) & dane$laureat_mja == 1},
  nie.laur.mja = function(dane, kontekst){ !is.na(dane$laureat_mja) & dane$laureat_mja == 0},

  wynik.s = function(dane, kontekst) {
    (!is.na(dane$wynik_s)) &
      dane$wynik_s >= as.numeric(kontekst$p.wynik.min.s) &
      dane$wynik_s <= as.numeric(kontekst$p.wynik.max.s)
  },
  wynik.gh = function(dane, kontekst) {
    (!is.na(dane$wynik_gh)) &
      dane$wynik_gh >= as.numeric(kontekst$p.wynik.min.gh) &
      dane$wynik_gh <= as.numeric(kontekst$p.wynik.max.gh)
  },
  wynik.gm = function(dane, kontekst) {
    (!is.na(dane$wynik_gm)) &
      dane$wynik_gm >= as.numeric(kontekst$p.wynik.min.gm) &
      dane$wynik_gm <= as.numeric(kontekst$p.wynik.max.gm)
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

filtr.dot.matury = function(nazwa) {
  substr(nazwa, nchar(nazwa) - 1, nchar(nazwa)) == '.m' |
    (substr(nazwa, nchar(nazwa) - 3, nchar(nazwa)) %in%
      c('.mpo', '.mma', '.mja'))
}
