instaluj = function(p) {
	if (p %in% installed.packages()) {
		print(sprintf('Pakiet %s juz zainstalowany', p))
	} else {
		install.packages(p, repos='http://cran.uni-muenster.de/') 
	}
}

instaluj('shiny')
instaluj('dplyr')
instaluj('httr')
instaluj('hexbin')
instaluj('plotly')
instaluj('shinyjs')
instaluj('shinyBS')
instaluj('pryr')
instaluj('ggsci')

