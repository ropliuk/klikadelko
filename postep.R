source('postep_rpc.R')

wyswietl_postep = function(p, stan) {
  if (p$op == 'start') {
    postep.gl <<- shiny::Progress$new(max = p$max)
    postep.gl$set(value = 0)
  } else if (p$op == 'krok') {
    postep.gl$set(
      value = p$krok,
      message = p$faza,
      detail = p$tekst
    )
  } else if (p$op == 'koniec') {
    postep.gl$close()

    stan$pam_uzyta = pam_uzyta()
    stan$pam_dziecka = p$pam_uzyta
    stan$pam_cala = stan$pam_uzyta + stan$pam_dziecka + pam_wolna()

    stan$ost_czas = proc.time()[['elapsed']] - stan$ost_start
  }
}
