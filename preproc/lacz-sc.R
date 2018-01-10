
library(dplyr)

nazwy = c(
	'sp',
	'gim',
	'mat',
	's',
	'gh',
	'gm',
	'mpo',
	'mpor',
	'mma',
	'mmar',
	'mja',
	'mjar'
)

konwertuj = function(nazwa) {
	print(nazwa)
	dane = read.csv(sprintf('sc-%s.csv', nazwa))
	dane = dane[dane[[paste0('id_szkoly_', nazwa)]] >= 0,]
	save(dane, file=sprintf('sc-%s.RData', nazwa))
}

lacz = function() {

  polaczona = NULL

	print_start = function(stan) {
		print(stan)
		print(Sys.time())
	}

	print_sum = function(stan) {
		print(paste(paste0(stan, ','),
  		nrow(polaczona),
			'wierszy,',
			ncol(polaczona),
			'kolumn'))
		print(Sys.time())
	}

  lapply(nazwy, function(nazwa) {
		plik = sprintf('sc-%s.RData', nazwa)
		print(paste('Wczytuje:', plik))
		load(plik)
		tab = dane
		if (nazwa %in% c('sp', 'gim', 'mat')) {
			do_usun = c('l_podejsc', 'kat')
		} else {
			do_usun = c('l_podejsc', 'l_testow', 'czy_popr', 'zrodlo')
		}
		lapply(do_usun, function(prefiks) {
			tab[[paste0(prefiks, '_', nazwa)]] <<- NULL
		})

  	if (is.null(polaczona)) {
  		polaczona <<- tab
  	} else {
  		print_start('Lacze')
			if (nazwa %in% c('sp', 'gim', 'mat')) {
	  		polaczona <<- full_join(polaczona, tab)
			} else {
				polaczona <<- left_join(polaczona, tab)
			}
			print_sum('Polaczono')
  	}
		if (nazwa == 'mat') {
			print_start('Filtruje')
			polaczona <<- polaczona %>%
				filter(
					rok_mat %in% c(2015, 2016) |
					rok_gim %in% c(2013, 2014, 2015))
  		print_sum('Przefiltrowano')

			print_start('Lacze z typami szkol')
			load('typy_szkol.RData')
			typy_szkol = dane
			polaczona <<- polaczona %>%
				left_join(typy_szkol, by = c("id_szkoly_mat" = "id_szkoly")) %>%
				rename(typ_szkoly_mat = typ_szkoly)
			print_sum('Polaczono')

			print_start('Lacze z dla_doroslych')
			load('nr_dla_doroslych.RData')
			polaczona <<- polaczona %>%
				left_join(nr_dla_doroslych %>% mutate(dla_doroslych_mat = 1), by = c("id_szkoly_mat" = "id_szkoly")) %>%
				mutate(dla_doroslych_mat = !is.na(dla_doroslych_mat))
			print_sum('Polaczono')
		}
  })

	dane = polaczona

  wyjscie = 'sciezki4.RData'
  save(dane, file = wyjscie)
}
