
source('niania.R')
source('system/rpc.R')

odbieraj_tekst(function(cmd) {
  if (cmd$typ == 'Odswiez') {
    nianiu_odswiez(cmd$wejscie)
  } else if (cmd$typ == 'Wyslij') {
    nianiu_wyslij(cmd$wejscie)
  }

  nianiu_odbieraj(function(wyjscie) {
    wyslij_tekst(wyjscie)
  })
  wyslij_tekst(koniec())
})
