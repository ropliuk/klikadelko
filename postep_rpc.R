source('system/rpc.R')
source('system/system.R')

# Postep = setRefClass('Postep', fields = list(
#   op = 'character', # 'start', 'krok', 'koniec'
#   krok = 'numeric',
#   max = 'numeric',
#   faza = 'character', # string
#   tekst = 'character', # string
#   pam_uzyta = 'numeric'
# ))

p.op = 'start'
p.krok = 0
p.max = 1
p.faza = ''
p.tekst = ''
p.pam_uzyta = 0

wyslij_postep = function() {
  wyslij_do_rodzica(list(
    typ = 'Postep',
    op = p.op,
    krok = p.krok,
    max = p.max,
    faza = p.faza,
    tekst = p.tekst,
    pam_uzyta = p.pam_uzyta
  ))
}

postep_start = function(n) {
  p.max <<- n
  wyslij_postep()
  NULL
}

postep_ustaw_teksty = function(faza=NULL, tekst=NULL, p) {
  if (!is.null(faza)) {
    p.faza <<- faza
  }
  if (!is.null(tekst)) {
    p.tekst <<- tekst
  }
  p.pam_uzyta <<- pam_uzyta()
}

postep_krok = function(dane=NULL, tekst=NULL, faza=NULL, p=postep.gl, i=1) {
  p.op <<- 'krok'
  p.krok <<- p.krok + 1
  postep_ustaw_teksty(faza, tekst, p)
  wyslij_postep()
  dane
}

postep_faza = function(faza=NULL, tekst=NULL, p=postep.gl) {
  p.op <<- 'krok'
  postep_ustaw_teksty(faza, tekst, p)
  wyslij_postep()
}

postep_koniec = function(p) {
  p.op <<- 'koniec'
  wyslij_postep()
}
