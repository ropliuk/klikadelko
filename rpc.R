library(parallel)

Koniec = setRefClass('Koniec', fields = list())

LogDziecka = setRefClass('LogDziecka', fields = list(
  tekst = 'character' # string
))

wyslij_do_dziecka = function(dziecko, wejscie) {
  save(wejscie, file='../wejscie.RData')
  parallel:::sendChildStdin(dziecko, 'cmd\n')
}

odbieraj_od_rodzica = function(obsluga_wejscia) {
  czy_koniec = FALSE
  koniec = function() {
    czy_koniec <<- TRUE
  }
  f <- file("stdin")
  open(f)
  while(!czy_koniec && length(line <- readLines(f,n=1)) > 0) {
    load('../wejscie.RData')
    obsluga_wejscia(wejscie, koniec)
  }
  close(f)
}

wyslij_do_rodzica = function(wyjscie) {
  parallel:::sendMaster(serialize(wyjscie, NULL))
}

odbieraj_od_dziecka = function(dziecko, obsluga_wyjscia) {
  czy_koniec = FALSE
  koniec = function() {
    czy_koniec <<- TRUE
  }
  while (!czy_koniec) {
    wyjscie = unserialize(parallel:::readChild(dziecko))
    obsluga_wyjscia(wyjscie, koniec)
  }
}

zamknij_dziecko = function(dziecko) {
  wyslij_do_dziecka(dziecko, Koniec())
  parallel:::mckill(dziecko, signal = 9)
}

loguj_dz = funtion(tekst) {
  wyslij_do_rodzica(LogDziecka(tekst))
}
