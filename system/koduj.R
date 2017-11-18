raw_do_hex = function(r) {
  do.call(paste0, lapply(as.integer(r), function(x) sprintf('%02x', x)))
}

hex_do_raw = function(h) {
  tab = sapply(seq(1, nchar(h), by=2), function(x) substr(h, x, x+1))
  as.raw(strtoi(tab, 16L))
}

koduj_raw = function(x) {
  serialize(x, NULL)
}

dekoduj_raw = function(x) {
  unserialize(x)
}

koduj_hex = function(x) {
  raw_do_hex(koduj_raw(x))
}

dekoduj_hex = function(x) {
  dekoduj_raw(hex_do_raw(x))
}

koduj_tekst = function(x) {
  h = koduj_hex(x)
  sprintf('KKD %d %s', nchar(h), h)
}

dekoduj_tekst = function(x) {
  if (substr(x, 1, 4) == 'KKD ') {
    tab = strsplit(substr(x, 5, 100), ' ')[[1]]
    len = as.numeric(tab[[1]])
    s = tab[[2]]
    if (nchar(s) != len) {
      # loguj_dz('Blad wyjscia', 'dlugosc deklarowana:', len, 'rzeczywista:', nchar(s))
      NULL
    } else {
      dekoduj_hex(s)
    }
  } else {
    # loguj_dz('Zbedne wyjscie', x)
    NULL
  }
}
