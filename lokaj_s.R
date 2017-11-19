
source('system/rpc.R')

nianiu_odswiez = function(wejscie) {
  wyslij_tekst(list(
    typ = 'Odswiez',
    wejscie = wejscie
  ))
}

nianiu_wyslij = function(wejscie) {
  wyslij_tekst(list(
    typ = 'Wyslij',
    wejscie = wejscie
  ))
}

nianiu_odbieraj = function(obsluz_wyjscie) {
  odbieraj_tekst(obsluz_wyjscie)
}
