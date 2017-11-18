library(parallel)

source('system/koduj.R')

# Koniec = setRefClass('Koniec', fields = list())

# LogDziecka = setRefClass('LogDziecka', fields = list(
#   tekst = 'character' # string
# ))

wyslij_do_dziecka = function(dziecko, wejscie) {
  loguj('WEJSCIE', nchar(koduj_tekst(wejscie)))
  parallel:::sendChildStdin(dziecko, paste0(koduj_tekst(wejscie), '\n'))
}

odbieraj_od_rodzica = function(obsluga_wejscia) {
  czy_koniec = FALSE
  koniec = function() {
    czy_koniec <<- TRUE
  }
  f <- file("stdin")
  open(f)
  while(!czy_koniec && length(line <- readLines(f, n=1)) > 0) {
    wejscie = dekoduj_tekst(line)
    obsluga_wejscia(wejscie, koniec)
  }
  close(f)
}

wyslij_do_rodzica = function(wyjscie) {
  parallel:::sendMaster(charToRaw(koduj_hex(wyjscie)))
}

odbieraj_od_dziecka = function(dziecko, obsluga_wyjscia) {
  czy_koniec = FALSE
  koniec = function() {
    czy_koniec <<- TRUE
  }
  while (!czy_koniec) {
    wyjscie = dekoduj_hex(rawToChar(parallel:::readChild(dziecko)))
    loguj('WYJSCIE', wyjscie)
    obsluga_wyjscia(wyjscie, koniec)
  }
}

zamknij_dziecko = function(dziecko) {
  wyslij_do_dziecka(dziecko, list(typ = 'Koniec'))
  parallel:::mckill(dziecko, signal = 9)
}

loguj_dz = function(tekst) {
  wyslij_do_rodzica(list(typ = 'LogDziecka', tekst = tekst))
}
