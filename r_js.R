odbierajZdarzenie = function(input, session, nazwa, obsluga) {
  klucz = paste0('event.', nazwa)

  pierwszyRaz = TRUE

  observe({
    nowa.wartosc = input[[klucz]]
    session$sendCustomMessage(type = paste0('on_', nazwa), nowa.wartosc)
    if (pierwszyRaz) {
      pierwszyRaz <<- FALSE
    } else {
      isolate(obsluga())
    }
  })
}
