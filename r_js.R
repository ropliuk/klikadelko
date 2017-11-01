odbierajZdarzenie = function(input, session, nazwa, obsluga) {
  klucz = paste0('event.', nazwa)
  klucz_parametru = paste0('event.param.', nazwa)

  pierwszyRaz = TRUE

  observe({
    nowa.wartosc = input[[klucz]][1]
    if (pierwszyRaz) {
      pierwszyRaz <<- FALSE
    } else {
      isolate({
        if (klucz_parametru %in% names(input) && input[[klucz_parametru]][1] != '') {
          obsluga(input[[klucz_parametru]][1])
        } else {
          obsluga()
        }
      })
    }
  })
}
