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
setnames(data, c("Consommation.[kWh]", "Réinjection.[kWh]", "Total.total.[kWh]", "Total.positive.[kWh]", "Total.negative.[kWh]", "Courbe.de.charge.nette.Consommation.[kWh]", "Courbe.de.charge.nette.de.l'alimentation.[kWh]", "Charge.nette.Total.[kWh]", "Puissance.moyennes.[kW]"), c("Consommation", "Réinjection", "Total", "Total_positif", "Total_negatif", "Courbe_charge_nette_Consommation", "Courbe_charge_nette_alimentation", "Charge_nette_Total", "Puissance_moyenne"))

# Gestion des dates
data[, date := as.Date(paste(substr(Temps, 7, 10), substr(Temps, 4, 5), substr(Temps, 1, 2), sep="-"))]
data[, heure := factor(substr(Temps, 12, 13), levels = sprintf("%02d", 0:23))]

# Flag pour savoir si la mesure est absente (1) ou présente (0)
data[, flag_abs := ifelse(is.na(Consommation), 1, 0)]


# -------------------- #
# Création d'une table #
# -------------------- #

# Table quotidien
quot <- data[, .(conso = sum(Consommation), reinjection = sum(Réinjection), total = sum(Total), total_pos = sum(Total_positif), total_neg = sum(Total_negatif), charge_nette_conso = sum(Courbe_charge_nette_Consommation), charge_nette_alim = sum(Courbe_charge_nette_alimentation), charge_nette_total = sum(Charge_nette_Total), abs = sum(flag_abs)), by="date"][order(date)]



summary(data$flag_abs)
