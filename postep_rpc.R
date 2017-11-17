source('system/rpc.R')
source('system/system.R')

Postep = setRefClass('Postep', fields = list(
  op = 'character', # 'start', 'krok', 'koniec'
  krok = 'numeric',
  max = 'numeric',
  faza = 'character', # string
  tekst = 'character', # string
  pam_uzyta = 'numeric'
))

postep_start = function(n) {
  p = Postep(
    op = 'start',
    krok = 0,
    max = n,
    faza = '',
    tekst = ''
  )
  wyslij_do_rodzica(p)
  p
}

postep_ustaw_teksty = function(faza=NULL, tekst=NULL, p) {
  if (!is.null(faza)) {
    p$faza = faza
  }
  if (!is.null(tekst)) {
    p$tekst = tekst
  }
  p$pam_uzyta = pam_uzyta()
}

postep_krok = function(dane=NULL, tekst=NULL, faza=NULL, p=postep.gl, i=1) {
  p$op = 'krok'
  p$krok = p$krok + 1
  postep_ustaw_teksty(faza, tekst, p)
  wyslij_do_rodzica(p)
  dane
}

postep_faza = function(faza=NULL, tekst=NULL, p=postep.gl) {
  p$op = 'krok'
  postep_ustaw_teksty(faza, tekst, p)
  wyslij_do_rodzica(p)
}

postep_koniec = function(p) {
  p$op = 'koniec'
  wyslij_do_rodzica(p)
}
