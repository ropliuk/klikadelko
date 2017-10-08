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
      # h4('Cechy SP'),
      fluidRow(

        tabsetPanel(
          tabPanel('Cechy SP', fluidRow(column(2, ui.mw(ns, 's')),
          column(2, ui.pp(ns, 's')),
          column(4, ui.ludnosc(ns, 's')),
          column(4, ui.w.szk(ns, 's')),
          column(4, ui.wynik.s.szkoly(ns, 's')),
          column(4, ui.wojewodztwo(ns, 's')))),
          tabPanel('Cechy ucznia SP', fluidRow(
            column(4, ui.rok(ns, 's')),
            column(4, ui.wynik(ns, 's', '40')),
            column(3, ui.dysl(ns, 's')),
            column(3, ui.laureat(ns, 's'))
          )),
          tabPanel('Cechy gimnazjum', fluidRow(
            column(2, ui.mw(ns, 'g')),
            column(2, ui.pp(ns, 'g')),
            column(4, ui.ludnosc(ns, 'g')),
            column(4, ui.w.szk(ns, 'g')),
            column(4, ui.wynik.s.szkoly(ns, 'g')),
            column(4, ui.wojewodztwo(ns, 'g'))
          )),
          tabPanel('Cechy ucznia gimnazjum', fluidRow(
            column(4, ui.rok(ns, 'g')),
            column(3, ui.dysl(ns, 'g')),
            column(3, ui.rocznik(ns, 'g')),
            column(3, ui.zmiana.powiatu(ns, 'g')),
            column(3, ui.rejonowosc(ns, 'g')),
            column(3, ui.laureat(ns, 'gh')),
            column(3, ui.laureat(ns, 'gm'))
          )),
          tabPanel('Cechy szkoły PG', fluidRow(
            column(2, ui.mw(ns, 'm')),
            column(2, ui.pp(ns, 'm')),
            column(4, ui.ludnosc(ns, 'm')),
            column(4, ui.w.szk(ns, 'm')),
            column(4, ui.wynik.s.szkoly(ns, 'm')),
            column(4, ui.wojewodztwo(ns, 'm')),
            column(6, ui.typ.szkoly.m(ns))
          )),
          tabPanel('Cechy ucznia szkoły PG', fluidRow(
            column(4, ui.rok(ns, 'm')),
            column(3, ui.dysl(ns, 'm')),
            column(3, ui.rocznik(ns, 'm')),
            column(3, ui.zmiana.powiatu(ns, 'm')),
            column(3, ui.rejonowosc(ns, 'm')),
            column(3, ui.laureat(ns, 'mpo')),
            column(3, ui.laureat(ns, 'mma')),
            column(3, ui.laureat(ns, 'mja'))
          ))
        )
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
    js$otwartoModalWiersza(id, '-modalnew')
    for (nazwa in lista.filtrow(input)) {
      if (fun_wyszarzen(nazwa)) {
        shinyjs::disable(nazwa)
      } else {
        shinyjs::enable(nazwa)
      }
    }
  }, priority = 1)

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
