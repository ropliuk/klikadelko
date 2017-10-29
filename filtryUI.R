#     column(2,
#       h4("Miasto / wieś"),
#       checkboxInput(ns('mw.ogol'), 'Ogół', TRUE),
#       checkboxInput(ns('mw.m'), 'Gimnazjum miejskie', FALSE),
#       checkboxInput(ns('mw.w'), 'Gimnazjum wiejskie', FALSE)
#     ),

ui.mw = function(ns, typ) {
  radioButtons(ns(paste0('f.mw.', typ)),
    label = 'Miasto / wieś',
    choices = list(
      'Ogół' = 'ogol',
      'Miasto' = paste0('miasto.', typ),
      'Wieś' = paste0('wies.', typ)
    ),
    selected = 'ogol'
  )
}

ui.dysl = function(ns, typ) {
  radioButtons(ns(paste0('f.dysl.', typ)),
    label = 'Dysleksja',
    choices = list(
      'Ogół' = 'ogol',
      'Dysl' = paste0('dysl.', typ),
      'Brak dysl' = paste0('brak.dysl.', typ)
    ),
    selected = 'ogol'
  )
}

ui.pp = function(ns, typ) {
  radioButtons(ns(paste0('f.pp.', typ)),
    label = 'Publiczna / prywatna',
    choices = list(
      'Ogół' = 'ogol',
      'Szkoła publiczna' = paste0('publ.', typ),
      'Szkoła prywatna' = paste0('pryw.', typ)
    ),
    selected = 'ogol'
  )
}

ui.laureat = function(ns, typ){
  radioButtons(ns(paste0('f.laureat.', typ)),
    label = paste0('Laureat ', typ),
    choices = list(
      'Ogół' = 'ogol',
      'Laureat' = paste0('laur.', typ),
      'Brak laureactwa' = paste0('nie.laur.', typ)
    ),
    selected = 'ogol'
  )
}

ui.ludnosc = function(ns, typ) {
  radioButtons(ns(paste0('f.ludnosc.', typ)),
    label = 'Ludność (TERYT) [tys.]',
    choiceNames = list(
      'Ogół',
      fluidRow(
        column(4, textInput(ns(paste0('p.ludnosc.min.', typ)), 'Od', '0')),
        column(4, textInput(ns(paste0('p.ludnosc.max.', typ)), 'do', '2000'))
      )
    ),
    choiceValues = list(
      'ogol',
      paste0('ludnosc.', typ)
    ),
    selected = 'ogol'
  )
}

ui.w.szk = function(ns, typ) {
  radioButtons(ns(paste0('f.w.szk.', typ)),
    label = 'Liczba uczniów',
    choiceNames = list(
      'Ogół',
      fluidRow(
        column(4, textInput(ns(paste0('p.w.szk.min.', typ)), 'Od', '0')),
        column(4, textInput(ns(paste0('p.w.szk.max.', typ)), 'do', '1000'))
      )
    ),
    choiceValues = list(
      'ogol',
      paste0('w.szk.', typ)
    ),
    selected = 'ogol'
  )
}

wybory.woj = list(
  'Dolnośląskie' = '1',
  'Kujawsko-pomorskie' = '2',
  'Lubelskie' = '3',
  'Lubuskie' = '4',
  'Łódzkie' = '5',
  'Małopolskie' = '6',
  'Mazowieckie' = '7',
  'Opolskie' = '8',
  'Podkarpackie' = '9',
  'Podlaskie' = '10',
  'Pomorskie' = '11',
  'Śląskie' = '12',
  'Świętokrzyskie' = '13',
  'Warmińsko-mazurskie' = '14',
  'Wielkopolskie' = '15',
  'Zachodniopomorskie' = '16'
)

wybory.oke = list(
  'Gdańsk'    = '0100000000100000',
  'Jaworzno'  = '0000000000010000',
  'Kraków'    = '0010010010000000',
  'Łomża'     = '0000000001000100',
  'Łódź'      = '0000100000001000',
  'Poznań'    = '0001000000000011',
  'Warszawa'  = '0000001000000000',
  'Wrocław'   = '1000000100000000'
)

ui.wojewodztwo = function(ns, typ) {
  radioButtons(ns(paste0('f.woj.', typ)),
    label = 'Lokalizacja',
    choiceNames = list(
      'Ogół',
      # textInput(ns(paste0('p.woj.', typ)), 'Numer wg. TERYT', '0'),
      selectInput(ns(paste0('p.woj.', typ)), '',
        wybory.woj,
        width="120px"
      ),
      selectInput(ns(paste0('p.oke.', typ)), 'OKE:',
        wybory.oke,
        width="120px"
      ),
      textInput(ns(paste0('p.teryt.', typ)), 'Prefiks TERYT', '')
    ),
    choiceValues = list(
      'ogol',
      paste0('woj.', typ),
      paste0('oke.', typ),
      paste0('teryt.', typ)
    ),
    selected = 'ogol'
  )
}

ui.typ.szkoly.m = function(ns) {
  radioButtons(ns('f.typ.szkoly.m'),
    label = 'Typ szkoły',
    choiceNames = list(
      'Ogół',
      # textInput(ns(paste0('p.woj.', typ)), 'Numer wg. TERYT', '0'),
      selectInput(ns('p.typ.szkoly.m'), '',
        c(
          'liceum ogólnokszt.' = 'LO',
          'liceum profilowane' = 'LP',
          'technikum' = 'T',
          'liceum uzupełniające' = 'LOU',
          'technikum uzupełniające' = 'TU'
        ),
        width="200px"
      )
    ),
    choiceValues = list(
      'ogol',
      'typ.szkoly.m'
    ),
    selected = 'ogol'
  )
}

ui.dla.dor.m = function(ns) {
  radioButtons(ns('f.dla.dor.m'),
    label = 'Dla dorosłych',
    choicesNames = list(
      'Ogół',
      'Dla dorosłych',
      'Dla młodzieży'
    ),
    choiceValues = list(
      'ogol',
      'dla.dor.m',
      'dla.ml.m',
    ),
    selected = 'ogol'
}

ui.wynik.s.szkoly = function(ns, typ) {
  radioButtons(ns(paste0('f.wynik.s.szkoly.', typ)),
    label = 'Średni wynik testu SP',
    choiceNames = list(
      'Ogół',
      fluidRow(
        column(4, textInput(ns(paste0('p.wynik.s.szkoly.min.', typ)), 'Od', '0')),
        column(4, textInput(ns(paste0('p.wynik.s.szkoly.max.', typ)), 'do', '40'))
      )
    ),
    choiceValues = list(
      'ogol',
      paste0('wynik.s.szkoly.', typ)
    ),
    selected = 'ogol'
  )
}

ui.wynik = function(ns, typ, max) {
  radioButtons(ns(paste0('f.wynik.', typ)),
    label = 'Wynik testu',
    choiceNames = list(
      'Ogół',
      fluidRow(
        column(4, textInput(ns(paste0('p.wynik.min.', typ)), 'Od', '0')),
        column(4, textInput(ns(paste0('p.wynik.max.', typ)), 'do', max))
      )
    ),
    choiceValues = list(
      'ogol',
      paste0('wynik.', typ)
    ),
    selected = 'ogol'
  )
}

ui.rocznik = function(ns, typ) {
  radioButtons(ns(paste0('f.rocznik.', typ)),
    label = 'Czas spędzony w szkole',
    choices = list(
      'Ogół' = 'ogol',
      'poniżej planowego' = paste0('mlodsi.', typ),
      'planowy' = paste0('o.czasie.', typ),
      'powyżej planowego' = paste0('starsi.', typ)
    ),
    selected = 'ogol'
  )
}

ui.rok = function(ns, typ) {
  min = ifelse(typ == 's', 2010, 2013)
  max = ifelse(typ == 's', 2013, 2016)
  radioButtons(ns(paste0('f.rok.', typ)),
    label = 'Rok',
    choiceNames = list(
      'Ogół',
      # min:max,
      fluidRow(
        column(4, textInput(ns(paste0('p.rok.min.', typ)), 'Od', min)),
        column(4, textInput(ns(paste0('p.rok.max.', typ)), 'do', max))
      )
    ),
    choiceValues = list(
      'ogol',
      # lapply(min:max, function(x) { sprintf('rok.%d.%s', x, typ) }),
      paste0('rok.', typ)
    ),
    selected = 'ogol'
  )
}

ui.rejonowosc = function(ns, typ) {
  radioButtons(ns(paste0('f.rejonowosc.', typ)),
    label = 'Nietypowy wybór szkoły',
    choiceNames = list(
      'Ogół',
      fluidRow(
        textInput(ns(paste0('p.wedr.od.procent.', typ)), 'Inna niż', '80'),
        '% uczniów poprzedniej'
      )
    ),
    choiceValues = list(
      'ogol',
      paste0('wedrowniejsi.', typ)
    ),
    selected = 'ogol'
  )
}

ui.zmiana.powiatu = function(ns, typ) {
  radioButtons(ns(paste0('f.zmiana.powiatu.', typ)),
    label = 'Inny powiat niż poprzednia szkoła',
    choices = list(
      'Ogół' = 'ogol',
      'Zmiana powiatu' = paste0('zm.pow.', typ),
      'Brak zmiany' = paste0('brak.zm.pow.', typ)
    ),
    selected = 'ogol'
  )
}
