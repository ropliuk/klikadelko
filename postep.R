postep_start = function(n, msg='Obliczam') {
  postep <<- shiny::Progress$new()
  postep$set(message = msg, value = 0)
  postep.max <<- n
  # print(paste('PS', n))
  postep
}

postep_krok = function(dane=NULL, tekst=NULL, faza=NULL, p=postep.gl, i=1) {
  # print(paste('PK', i, msg))
  p$inc(i/postep.max)
  postep_faza(faza, tekst, p)
  dane
}

postep_faza = function(faza=NULL, tekst=NULL, p=postep.gl) {
  if (!is.null(faza)) {
    p$set(message = faza)
  }
  if (!is.null(tekst)) {
    p$set(detail = tekst)
  }
}

postep_koniec = function(p) {
  p$close()
}
