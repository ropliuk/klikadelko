#!/bin/bash

instaluj() {
	sudo apt-get -y install $1
}

cd ~/soc/klikadelko

git pull

cd inst

instaluj vim
instaluj r-base-core
instaluj g++
instaluj openssh-server
instaluj libcurl4-openssl-dev
instaluj libssl-dev
instaluj gfortran

Rscript inst.R

