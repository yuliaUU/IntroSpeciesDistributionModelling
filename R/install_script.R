# SCRIPT FOR INSTALLING R PACKAGES FOR COURSE ECOLOGICAL NICHE MODELLING USING R (ENMR02)

# ALL PRACTICALS WORK PROPERLY UNDER R version 3.4.3

# install all necessary packages for practicals
install.packages(c("raster","rgdal","maptools","dismo","biomod2","ecospat"),dependencies = T)

# check installed packages
library(raster)
library(rgdal)
library(maptools)
library(dismo)
library(biomod2)
library(ecospat)

# Please, send a mail to neftali.sillero@gmail.com to confirm that all packages were installed successfully
# You copy and paste the console output when loading the packages
# If you have problems during installation, please mail to neftali-sillero@gmail.com