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
