#!/bin/bash

now () {
	sep='-'
	if test $# -eq 1
	then
	  sep=$1
	fi
	seplen=`echo $sep | wc -c`
	date +%Y%m%d$sep%H%M%S$sep%N | cut -c -`echo 2*$seplen+15 | bc`
}

echo "Zamykam poprzednie procesy bazy..."
killall R 2>/dev/null

cd ~/soc/klikadelko

echo "Uruchamiam nowy proces bazy..."
if test "$1" == "zlozony"
then
	SKR=start_zlozony.R
else
	SKR=start.R
fi
Rscript $SKR 2> ~/soc/log/klikadelko-`now`.log &

sleep 3

echo "Uruchamiam przegladarke..."
firefox localhost:38381

