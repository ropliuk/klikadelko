library(shiny)

source('dziecko.R')
source('system/rpc.R')
source('system/system.R')

dziecko = NULL

nianiu_odswiez = function(wejscie) {
  if (!is.null(dziecko)) {
    zamknij_dziecko(dziecko)
  }
  dziecko <<- mcparallel(proces_dziecka(wejscie))
}

nianiu_wyslij = function(wejscie) {
  wyslij_do_dziecka(dziecko, wejscie)
}

nianiu_odbieraj = function(obsluz_wyjscie) {
  odbieraj_od_dziecka(dziecko, obsluz_wyjscie)
}

proces_dziecka = function(wejscie) {
  withCallingHandlers(captureStackTraces(kod_dziecka(wejscie)),
    error = function(e) {
      loguj('Blad dziecka', e, fn='../blad_dziecka.log')
      wyslij_do_rodzica(list(
        typ = 'BladDziecka',
        opis = conditionMessage(e),
        stos = formatStackTrace(conditionStackTrace(e))
      ))
    }
  )
}
