###################################################
#                                                 #
# Script pour l'analyse des données de MyOiken    #
#                                                 #
###################################################

rm(list=ls())
gc()

# Chargement des librairies
library(ggplot2)
library(data.table)
library(openxlsx)
# library(tidyr)
# library(dplyr)
# library(lubridate)


## Lecture des données
# Chemins d'accès aux dossiers de données
ddpath  <- "C:/Users/doglo/OneDrive/Stat_R/Data/"
# ddpath  <- "C:/Users/Hugo/OneDrive/Stat_R/Data/" # Desktop

# Chemins d'accès aux dossiers de travail
wdpath  <- "C:/Users/doglo/OneDrive/Stat_R/work/"
# wdpath  <- "C:/Users/Hugo/OneDrive/Stat_R/work/" # Desktop



## Lecture des données de base
# Résultats horaires
data <- as.data.table(read.xlsx(paste(ddpath, "Rapport_Oiken_horaire.xlsx", sep=""), sheet="2026", colNames = TRUE, detectDates = TRUE, startRow = 3))


# ----------------------- #
# Préparation des données #
# ----------------------- #

# Renommage des colonnes




