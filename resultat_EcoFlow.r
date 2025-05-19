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
library(tidyr)
library(dplyr)


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
prod <- as.data.table(read.xlsx(paste(ddpath, "ecoflow.xlsx", sep=""), sheet="Entrée", colNames = TRUE, detectDates = TRUE, startRow = 1))


# ----------------------- #
# Préparation des données #
# ----------------------- #

# Données quotidiennes
quot[, an := year(date)][, mois := month(date)][, semaine := isoweek(date)][, trim := as.factor(quarter(date))][, jour := as.factor(wday(date))]
quot[an == 2025, prix_achat := .3084][an == 2025, prix_vente := .12]

# Donnée horaires
prod[, an := year(date)][, mois := month(date)][, semaine := isoweek(date)][, trim := as.factor(quarter(date))][, jour := as.factor(wday(date))]


# Structure des données
str(quot)
str(prod)


# ------- #
# Analyse #
# ------- #

## Graphique

# Quotidien
ggplot(quot, aes(x = date)) +
  geom_bar(aes(y = production), stat = "identity", fill = "yellow2") + 
  labs(x = "Date", y = "Production (en Wh)", title = "Production quotidienne")

# Horaire
# format long
df_long <- prod %>%
  pivot_longer(
    cols = -c(date, an, mois, semaine, trim, jour), # Toutes les colonnes sauf dates
    names_to = "Heure", # Nouvelle colonne pour les heures
    values_to = "Production") %>%  # Valeurs de production horaire
  mutate(
    Heure = as.integer(Heure),
    date = as.Date(date)
  )
  
df_long <- as.data.table(df_long)
#df_long <- subset(df_long, date %in% c("2025-05-15", "2025-05-17"))

jour_ligne <- "2025-05-18"
jour_barre <- "2025-05-19"

df_ligne <- subset(df_long, date == jour_ligne)
df_barre <- subset(df_long, date == jour_barre)


ggplot() +
  geom_col(data = df_barre, aes(x = Heure, y = Production), fill = "#a7a824", alpha = 0.7) +
  geom_line(data = df_ligne, aes(x = Heure, y = Production), color = "#ffd700") +
  geom_point(data = df_ligne, aes(x = Heure, y = Production), color = "#ffd700", size = 2) +
  labs(title = "Production horaire des panneaux solaires",
       x = "Heure de la journée",
       y = "Production (Wh)", 
       subtitle = paste("Barres =", jour_barre, "| Ligne =", jour_ligne)) +
  theme_gray()


