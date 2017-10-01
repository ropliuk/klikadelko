odbierajZdarzenie = function(input, session, nazwa, obsluga) {
  klucz = paste0('event.', nazwa)

  pierwszyRaz = TRUE

  observe({
    nowa.wartosc = input[[klucz]][1]
    if (pierwszyRaz) {
      pierwszyRaz <<- FALSE
    } else {
      isolate(obsluga())
    }
  })
}
