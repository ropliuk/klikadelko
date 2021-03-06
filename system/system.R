# raportPamieci = function() {
#   lista = unlist(lapply(search(), function(e) { ls(e, all.names=TRUE) }))
#   l = sort( sapply(lista, function(x) { object.size(get(x))}) )
#   start = max(1, length(l)-3)
#   print(l[start:length(l)] / 1048576)
#   print(mem_used())
# }

loguj = function(..., fn=NULL) {
  l = list(...)
  if (length(l) > 1) {
    ogon = l[2:length(l)]
  } else {
    ogon = list()
  }
  if (!is.null(fn)) {
    f = file(fn, open='a')
  } else {
    f = stderr()
  }
  cat(file=f,
    '[K]',
    format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"),
    sprintf('%2s%-20s', '', l[[1]]),
    paste(ogon),
    "\n"
  )
  if (!is.null(fn)) {
    close(f)
  }
}

liczba_z_basha = function(polecenie) {
  as.numeric(system(polecenie, intern=TRUE))
}

pam_uzyta = function() {
  liczba_z_basha(sprintf('ps -p %d -o rss=', Sys.getpid())) / 1048576
}

pam_wolna = function() {
  liczba_z_basha('cat /proc/meminfo | head -n 2 | tail -n 1 | cut -f2 -d: | tr -d \ bBkBmMgGtT') / 1048576
}

pam_cala = function(pam_dziecka=0) {
  pam_uzyta() + pam_dziecka + pam_wolna()
}
