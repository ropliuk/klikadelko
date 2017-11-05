library(ggsci)

koloruj = function(tekst, kolor) {
  paste0('<font color=\"', kolor, '\">', tekst, '</font>')
}

kolory = pal_d3('category10')(10)

kolorWykres = function(i) {
  kolory[i]
}

kolorHtml = function(i) {
  kolor = substr(kolory[i], 0, nchar(kolory[i]) - 2)
}
