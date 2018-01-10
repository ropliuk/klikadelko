library(DBI)
library(ZPD)

src = polacz()
db = src_sqlite('db/7.sqlite.db')

wez_sql = function(
  query,
	file,
	name
){
	test = tbl(src, sql(query))
	print("Rozpoczynam pobieranie...")
	test = test %>% collect(n = Inf)
	print(sprintf("Pobralem %d wierszy. Zapisuje...", nrow(test)))
	filename = sprintf("%s.RData", file)
	save(test, file=e(filename))
	assign(name, test, inherits=TRUE)
	print("OK!")
}

wez_tabele = function(
  name
){
	wez_sql(sprintf('select * from %s', name), sprintf('rdata/%s.RData', name), name)
}

podejrzyj_test = function(
  id
){
	DBI::dbGetQuery(src$con, e(sprintf("CREATE TEMPORARY VIEW temp%d AS SELECT 1", id)))
	query = sprintf("SELECT zbuduj_widok_testu('temp%d', %d, true, null, false, true);", id, id)
	DBI::dbGetQuery(src$con, e(query))
	print(tbl(src, sql(sprintf("SELECT count(*) FROM temp%d", id))))
}

wez_test = function(
  id
){
	test = pobierz_wyniki_testu(src, id, TRUE)
	print("Rozpoczynam pobieranie...")
	test = test %>% collect(n = Inf)
	print(sprintf("Pobralem %d wierszy. Zapisuje...", nrow(test)))
	filename = sprintf("test%d.RData", id)
	save(test, file=e(filename))
	print("OK!")
}

wez_testy_obs = function(
  id
){
	wez_sql(
		sprintf("SELECT * from testy_obserwacje where id_testu = %d", id),
		sprintf("rdata/testy_obs/testy_obs_%d.RData", id),
		'test')
}

wez_testy_plik = function(
  file
){
	lines <- readLines(file)
	for (line in lines) {
		print(sprintf("Test: %s", line))
		wez_test(strtoi(line))
	}
}

sq = function(
	query
){
	return(tbl(src, sql(query)))
}

rdata_nazwa = function(
  file
){
	name_ext = tail(strsplit(file, "/")[[1]], n=1)
	name = strsplit(name_ext, "\\.")[[1]][1]
	return(name)
}

wczytaj_rdata = function(
  file
){
	name=rdata_nazwa(file)
	load(file)
	assign(name, test, inherits=TRUE)
	return(get(name))
	print(sprintf("Wynik: %s", name))
}

sumuj_wyniki = function(
  name
){
	kol = colnames(get(name))
	query = sprintf("SELECT %s, %s AS wynik FROM %s",
	  do.call(paste, c(as.list(kol[substr(kol,0,2)!="k_"]), sep=", ")),
	  do.call(
			paste,
			c(
				lapply(
					as.list(kol[substr(kol,0,2)=="k_"]),
					function(k) { sprintf("ifnull(%s, 0)", k) }
				),
				sep="+"
			)),
		name)
	res = tbl(db, sql(e(query)))
	res = res %>% collect(n = Inf)
	return(res)
}

sumuj_wyniki_plik = function(
  file
){
	name=rdata_nazwa(file)
	print(sprintf("Wczytuje %s z %s", name, file))
	table = wczytaj_rdata(file)
	if (ncol(table) == 0) {
		print("Tabela ma 0 kolumn! Pomijam")
	} else {
  	zapisz_sqlite(table, name)
  	s_table = sumuj_wyniki(name)
  	s_name = sprintf("s_%s", name)
  	zapisz_sqlite(s_table, s_name)
  	db$con %>% db_drop_table(name)
	}
}

sumuj_wyniki_kat = function(
  folder
){
	files <- list.files(path=folder, pattern="*.RData", full.names=TRUE, recursive=FALSE)
	lapply(files, function(file) {
		sumuj_wyniki_plik(file)
	})
}

zapisz_plik = function(
  file
){
	name=rdata_nazwa(file)
	print(sprintf("Wczytuje %s z %s", name, file))
	table = wczytaj_rdata(file)
	if (ncol(table) == 0) {
		print("Tabela ma 0 kolumn! Pomijam")
	} else {
  	zapisz_sqlite(table, name)
	}
}

zapisz_kat = function(
	folder
){
	files <- list.files(path=folder, pattern="*.RData", full.names=TRUE, recursive=FALSE)
	lapply(files, function(file) {
		zapisz_plik(file)
	})
}

zapisz_sqlite = function(
  table,
  name
){
	w = nrow(table)
	k = ncol(table)
	print(sprintf("Zapisuje tabele %s [%d x %d] do bazy danych", name, w, k))
	copy_to(db, table, name=name, temporary=FALSE)
}
