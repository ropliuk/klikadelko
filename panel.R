source('kolory.R')

zarzadzajPanelem = function(input, output) {
  stan = reactiveValues(
    parametry = TRUE
  )

  observeEvent(input$pokazParametry, {
    stan$parametry <<- TRUE
  })

  observeEvent(input$ukryjParametry, {
    stan$parametry <<- FALSE
  })

  output$parametry <- reactive({ stan$parametry })
  outputOptions(output, 'parametry', suspendWhenHidden = FALSE)

  output$uiPokazParametry <- renderUI({
    if (!stan$parametry) {
      actionButton('pokazParametry', 'Opcje')
    }
  })
}

rzeczyWPanelu = function(input, output, stan) {
  shinyjs::disable('liniowy')

  observeEvent(input$liniowy, {
    stan$rodzaj_wykresu <<- 'liniowy'
    shinyjs::show('os.X')
    shinyjs::disable('liniowy')
    shinyjs::enable('slupkowy')
  })

  observeEvent(input$slupkowy, {
    stan$rodzaj_wykresu <<-'slupkowy'
    shinyjs::hide('os.X')
    shinyjs::enable('liniowy')
    shinyjs::disable('slupkowy')
  })

  output$pamiec = renderText({
    sprintf('%.2f (+ %s) / %.2f GB',
      stan$pam_uzyta,
      koloruj(
        sprintf('%.2f', stan$pam_dziecka),
        ifelse(stan$duze_dziecko, 'ff0000', '000000')
      ),
      stan$pam_cala)
  })

  output$czas = renderText({
    sprintf('czas (ost.): %.2f s', stan$ost_czas)
  })
}
