source('./system.R')

czy_koniec = FALSE

cin <- file("stdin")
open(cin)

while(!czy_koniec && length(line <- readLines(cin, n=1)) > 0) {
  if (line == 'KKD KONIEC') {
    koniec = TRUE
  } else if (substr(line, 1, 4) == 'KKD ') {
    tab = strsplit(substr(line, 5, 100), ' ')[[1]]
    len = as.numeric(tab[[1]])
    s = tab[[2]]
    if (nchar(s) != len) {
      loguj('Blad wyjscia', 'dlugosc deklarowana:', len, 'rzeczywista:', nchar(s))
    } else {
      cat(file=stdout(), paste0(s, '\n'))
    }
  } else {
    loguj('Zbedne wyjscie', line)
  }
}

close(cin)
