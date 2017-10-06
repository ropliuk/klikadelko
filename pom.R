
WIERSZE = 10

# pomocnicza tabela do zabaw
df = data.frame(
  a = c(1, 1, 1, 2, 2, 3, 3, 3),
  b = c(1, 2, 3, 1, 1, 2, 2, 2),
  c = c(4, 5, 6, 4, 4, 5, 6, 7)
)

slownik.odwr = function(slownik) {
  res = list()
  lapply(names(slownik), function(klucz) {
    wartosc = slownik[[klucz]]
    res[[wartosc]] <<- klucz
  })
  res
}
