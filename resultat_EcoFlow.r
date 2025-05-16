###################################################
#                                                 #
# Script pour l'analyse de la production EcoFlow  #
#                                                 #
###################################################

rm(list=ls())
gc()

# Chargement des librairies
library(ggplot2)
library(data.table)
library(openxlsx)


## Lecture des données
# Chemins d'accès aux dossiers de données
ddpath  <- "C:/Users/doglo/OneDrive/Stat_R/Data/"

# Chemins d'accès aux dossiers de travail
wdpath  <- "C:/Users/doglo/OneDrive/Stat_R/work/" 

# fixe le dossier de travail et de données
setwd(wdpath)


## Lecture des données de base
# Résultats quotidien
quot <- as.data.table(read.xlsx(paste(ddpath, "ecoflow.xlsx", sep=""), sheet="quotidien", colNames = TRUE, detectDates = TRUE))

# Résultats détaillés (horaires)
hora <- as.data.table(read.xlsx(paste(ddpath, "ecoflow.xlsx", sep=""), sheet="horaire", colNames = TRUE, detectDates = TRUE, startRow = 2))


# ----------------------- #
# Préparation des données #
# ----------------------- #

# Données quotidiennes
quot[, an := year(date)][, mois := month(date)][, semaine := isoweek(date)][, trim := as.factor(quarter(date))][, jour := as.factor(wday(date))]
quot[an == 2025, prix_achat := .3084][an == 2025, prix_vente := .12]

# Donnée horaires
hora[, an := year(date)][, mois := month(date)][, semaine := isoweek(date)][, trim := as.factor(quarter(date))][, jour := as.factor(wday(date))]


# Structure des données
str(quot)
str(hora)


# ------- #
# Analyse #
# ------- #

## Graphique

# Quotidien
ggplot(quot, aes(x = date)) +
  geom_bar(aes(y = production), stat = "identity", fill = "yellow2") + 
  labs(x = "Date", y = "Production (en Wh)", title = "Production quotidienne")

# Horaire

# Création du data set
hora2 <- subset(hora, date == '2025-05-16', select=c(prod_0, prod_1, prod_2, prod_3, prod_4, prod_5,prod_6,prod_7,prod_8,prod_9,prod_10,prod_11,prod_12,prod_13,prod_14,prod_15,prod_16,prod_17,prod_18,prod_19,prod_20,prod_21, prod_22, prod_23))
setnames(hora2, c('prod_0', 'prod_1', 'prod_2', 'prod_3', 'prod_4', 'prod_5', 'prod_6', 'prod_7', 'prod_8', 'prod_9', 'prod_10', 'prod_11', 'prod_12', 'prod_13', 'prod_14', 'prod_15', 'prod_16', 'prod_17', 'prod_18', 'prod_19', 'prod_20', 'prod_21', 'prod_22', 'prod_23'), 
         c('prod_0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23'))


# format long
melt(hora2)

ggplot(hora, aes(x = date)) +
  geom_line(aes(y = ))