library(shiny)
library(lineprof)

source('ui.R')
source('server.R')
prof = lineprof(runApp('.'))
# runApp('.')
