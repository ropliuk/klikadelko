postep_start = function(n, msg='Ładuję dane') {
  postep <<- shiny::Progress$new()
  postep$set(message = msg, value = 0)
  postep.max <<- n
  # print(paste('PS', n))
  postep
}

postep_krok = function(dane=NULL, p, i=1, msg='') {
  # print(paste('PK', i, msg))
  p$inc(i/postep.max, detail = msg)
  dane
}

postep_koniec = function(p) {
  p$close()
}
