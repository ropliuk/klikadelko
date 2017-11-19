library(parallel)

source('system/koduj.R')

# Koniec = setRefClass('Koniec', fields = list())

# LogDziecka = setRefClass('LogDziecka', fields = list(
#   tekst = 'character' # string
# ))

wyslij_tekst = function(wejscie) {
  write(file=stdout(), koduj_tekst(wejscie))
}

wyslij_do_dziecka = function(dziecko, wejscie) {
  loguj('WEJSCIE', nchar(koduj_tekst(wejscie)))
  parallel:::sendChildStdin(dziecko, paste0(koduj_tekst(wejscie), '\n'))
}

wyslij_do_rodzica = function(wyjscie) {
  parallel:::sendMaster(charToRaw(koduj_hex(wyjscie)))
}

odbieraj_tekst = function(obsluga_wejscia) {
  czy_koniec = FALSE
  f <- file("stdin")
  open(f)
  while(!czy_koniec && length(line <- readLines(f, n=1)) > 0) {
    wejscie = dekoduj_tekst(line)
    if (!is.null(wejscie)) {
      if (wejscie$typ == 'Koniec') {
        czy_koniec = TRUE
      } else {
        obsluga_wejscia(wejscie)
      }
    }
  }
  close(f)
}

odbieraj_od_rodzica = function(obsluga_wejscia) {
  odbieraj_tekst(obsluga_wejscia)
}

odbieraj_od_dziecka = function(dziecko, obsluga_wyjscia) {
  czy_koniec = FALSE
  while (!czy_koniec) {
    wyjscie = dekoduj_hex(rawToChar(parallel:::readChild(dziecko)))
    if (wyjscie$typ == 'Koniec') {
      czy_koniec = TRUE
    } else {
      obsluga_wyjscia(wyjscie)
    }
  }
}

zamknij_dziecko = function(dziecko) {
  wyslij_do_dziecka(dziecko, koniec())
  parallel:::mckill(dziecko, signal = 9)
}

loguj_dz = function(tekst) {
  wyslij_do_rodzica(list(typ = 'LogDziecka', tekst = tekst))
}

koniec = function() {
  list(typ = 'Koniec')
}
