source('filtryUI.R')
source('filtry.R')
source('kolory.R')

wierszUI = function(id) {
  ns = NS(id)
  fluidRow(
    column(1,
      div(class="nr_wiersza",
        htmlOutput(ns('numer'))
      )
    ),
    column(6, htmlOutput(ns('opis'))),
    column(2, actionButton(ns('edytujWiersz'), 'Edytuj')),
    ifelse(id != 'wiersz0',
      tagList(
        column(2, actionButton(ns('pokazWielkoscProbek'), 'Liczności'))
      ),
      tagList()
    ),
    ifelse(id != 'wiersz0',
      tagList(
        column(1, actionButton(ns('usunWiersz'), 'X'))
      ),
      tagList()
    ),
    bsModal(ns("modalnew"), "Edytuj warunki wiersza", "BUTnew", size = "large",
      h4('Cechy SP'),
      fluidRow(
        # lapply(
        #   LISTA_FILTROW,
        #   function(nazwa){ column(4, filtryUI[[nazwa]](ns)) }
        # )
        column(2, ui.mw(ns, 's')),
        column(2, ui.pp(ns, 's')),
        column(4, ui.ludnosc(ns, 's')),
        column(4, ui.w.szk(ns, 's')),
        column(4, ui.wynik.s.szkoly(ns, 's')),
        column(4, ui.wojewodztwo(ns, 's'))
      ),
      h4('Cechy ucznia w SP'),
      fluidRow(
        column(4, ui.rok(ns, 's')),
        column(4, ui.wynik(ns, 's', '40')),
        column(3, ui.dysl(ns, 's')),
        column(3, ui.laureat(ns, 's'))
      ),
      h4('Cechy gimnazjum'),
      fluidRow(
        column(2, ui.mw(ns, 'g')),
        column(2, ui.pp(ns, 'g')),
        column(4, ui.ludnosc(ns, 'g')),
        column(4, ui.w.szk(ns, 'g')),
        column(4, ui.wynik.s.szkoly(ns, 'g')),
        column(4, ui.wojewodztwo(ns, 'g'))
      ),
      h4('Cechy ucznia w gimnazjum'),
      fluidRow(
        column(4, ui.rok(ns, 'g')),
        column(3, ui.dysl(ns, 'g')),
        column(3, ui.rocznik(ns)),
        column(3, ui.zmiana.powiatu(ns)),
        column(3, ui.rejonowosc(ns)),
        column(3, ui.laureat(ns, 'gh')),
        column(3, ui.laureat(ns, 'gm'))
      )
    )
  )
}

koloruj = function(tekst, kolor) {
  paste0('<font color=\"', kolor, '\">', tekst, '</font>')
}


wierszModul = function(input, output, session, id, numer, fun_wyszarzen) {
  observeEvent(input$edytujWiersz, {
    toggleModal(session, "modalnew", toggle = "open")
    for (nazwa in lista.filtrow(input)) {
      if (fun_wyszarzen(nazwa)) {
        shinyjs::disable(nazwa)
      } else {
        shinyjs::enable(nazwa)
      }
    }
  }, priority = 1)

#   (for (nazwa in LISTA_FILTROW) {
#     if (fun_wyszarzen_wsp(nazwa)) {
#       shinyjs::disable(nazwa)
#     } else {
#       shinyjs::enable(nazwa)
#     }
#   }
# }, priority = 1)

    # )

  # observeEvent(input$pokazWielkoscProbek, {
  #   input$ktory.wykres = numer
  # })

  output$numer <- renderText({
    if (numer > 0) {
      numer %>% koloruj(kolorHtml(numer))
    } else {
      ''
    }
  })

  output$opis <- renderText({
    opis.dla.warunkow(input, NULL) %>% koloruj(kolorHtml(numer))
  })

  # lapply(names(SLOWNIK_PARAMETROW), function(nazwa) {
  #   for (ev in c('mouseenter', 'mouseclick', 'hover')) {
  #     shinyjs::onevent(ev, nazwa, print(paste('Z', nazwa, ev)))
  #   }
  #   shinyjs::onclick(nazwa, {
  #     print(paste('A', nazwa))
  #     if (input[[nazwa]] == 'ogol') {
  #       print(paste('B', nazwa))
  #       for (par in SLOWNIK_PARAMETROW[[nazwa]]) {
  #         shinyjs::disable(par)
  #       }
  #     } else {
  #       for (par in SLOWNIK_PARAMETROW[[nazwa]]) {
  #         shinyjs::enable(par)
  #       }
  #     }
  #   })
  # })

  input
}


# wierszModul1 = function(input, output, session, id, numer) {
#
#   toggleModal(session, "modalnew", toggle = "open")
#
#
#
#   output$numer <- renderText({
#     paste0('', numer)
#   })
#
#   output$opis <- renderText({
#     opis.dla.warunkow(input)
#   })
#
#   input
# }
