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
library(lubridate)


## Lecture des données
# Chemins d'accès aux dossiers de données
ddpath  <- "C:/Users/doglo/OneDrive/Stat_R/Data/"
# ddpath  <- "C:/Users/Hugo/OneDrive/Stat_R/Data/" # Desktop

# Chemins d'accès aux dossiers de travail
wdpath  <- "C:/Users/doglo/OneDrive/Stat_R/work/" 
# wdpath  <- "C:/Users/Hugo/OneDrive/Stat_R/work/" # Desktop 

# fixe le dossier de travail et de données
# setwd(wdpath)


## Lecture des données de base
# Résultats quotidien
quot <- as.data.table(read.xlsx(paste(ddpath, "ecoflow.xlsx", sep=""), sheet="quotidien", colNames = TRUE, detectDates = TRUE))

# Résultats détaillés (horaires)
prod <- as.data.table(read.xlsx(paste(ddpath, "ecoflow.xlsx", sep=""), sheet="Production", colNames = TRUE, detectDates = TRUE, startRow = 1))


# ----------------------- #
# Préparation des données #
# ----------------------- #

# Date aujourd'hui
auj <- Sys.Date()

# Données quotidiennes
quot[, an := year(date)][, mois := month(date)][, semaine := isoweek(date)][, trim := as.factor(quarter(date))][, jour := as.factor(wday(date, week_start = 1))]
quot[an == 2025 & date >= "2025-06-22", prix_achat := .3084][an == 2025 & date >= "2025-06-22", prix_vente := .1102]

# Donnée horaires
prod[, an := year(date)][, mois := month(date)][, semaine := isoweek(date)][, trim := as.factor(quarter(date))][, jour := as.factor(wday(date, week_start = 1))]


## Format long
# Horaire
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

# Calcul du surplus théorique horaire (en kWh)
df_long[, surplus := ifelse(Production < 180, 0, Production - 180)]
sum(df_long[, surplus], na.rm=TRUE) / 1000

surplus_jour <- df_long[, .(surplus = sum(surplus, na.rm=TRUE)), by="date"][order(date)]

# quot <- quot[date >= "2025-06-22",] # A déplacer

# Structure des données
str(quot)
str(prod)


# ------- #
# Analyse #
# ------- #

# Journée production plus élevée
(date_max <- quot[which.max(quot[, production]), date])
(date_min <- quot[which.min(quot[date > "2025-05-16", production])+2, date])


## Graphiques

# Quotidien
ggplot(quot, aes(x = date)) +
  geom_bar(aes(y = production), stat = "identity", fill = "#a7a824") + 
  labs(x = "Date", y = "Production (en Wh)", title = "Production quotidienne")

jour_ligne <- date_max
jour_barre <- auj - 1

df_ligne <- subset(df_long, date == jour_ligne)
df_barre <- subset(df_long, date == jour_barre)


ggplot() +
  geom_col(data = df_barre, aes(x = Heure, y = Production), fill = "#a7a824", alpha = 0.75) +
  geom_line(data = df_ligne, aes(x = Heure, y = Production), color = "#ffd700") +
  geom_point(data = df_ligne, aes(x = Heure, y = Production), color = "#ffd700", size = 2) +
#  geom_hline(yintercept = 180, linetype = "dashed", color = "khaki3") +
  labs(title = "Production horaire des panneaux solaires",
       x = "Heure de la journée",
       y = "Production (Wh)", 
       subtitle = paste("Barres =", jour_barre, "| Ligne =", jour_ligne)) +
  scale_x_continuous(breaks = seq(0, 23, 1)) +
  theme_gray() +
  theme(axis.text.x = element_text(angle=90, vjust = 0.5, hjust = 1))


# Résumés des production hebdomadaires, mensuels, timestrielles, etc.
prod_hebd <- quot[, .(prod_kWh = (sum(production)/1000)), by = c("semaine", "an")][order(semaine, an)]
prod_mens <- quot[, .(prod_kWh = (sum(production)/1000)), by = c("mois", "an")][order(mois, an)]
prod_trim <- quot[, .(prod_kWh = (sum(production)/1000)), by = c("trim", "an")][order(trim, an)]
prod_an <- quot[, .(prod_kWh = (sum(production)/1000)), by = "an"][order(an)]


## Graphique
# Hebdomadaire
ggplot() +
  geom_col(data = prod_hebd, aes(x = semaine, y = prod_kWh), fill = "#ffd700", alpha = 0.85) +
  labs(title = "Production hebdomadaire (en kWh)",
       x = "Semaine",
       y = "Production (kWh)") +
  scale_x_continuous(labels = scales::number_format(accuracy = 1)) +
  theme_gray() +
  theme(axis.text.x = element_text(angle=90, vjust = 0.5, hjust = 1), plot.title = element_text(hjust=0.5))

# Mensuel
ggplot() +
  geom_col(data = prod_mens, aes(x = mois, y = prod_kWh), fill = "#ffd700", alpha = 0.85) +
  labs(title = "Production mensuelle (en kWh)",
       x = "Mois",
       y = "Production (kWh)") +
  scale_x_continuous(breaks = seq(5,6,1)) +
  theme_gray() +
  theme(axis.text.x = element_text(angle=90, vjust = 0.5, hjust = 1), plot.title = element_text(hjust=0.5))


## Maximum heure par heure
(max_heure <- df_long[, .(max_heure = max(Production, na.rm=TRUE)), by="Heure"][order(Heure)])
sum(max_heure[, max_heure])
max(quot[, production])

100*(max(quot[, production]) / sum(max_heure[, max_heure]))


## Calcul d'une moyenne sur 7 jours
moy_heure <- df_long[date < auj & date > auj - 8, .(moy_heure = mean(Production, na.rm = TRUE)), by="Heure"][order(Heure)]

# Ajouter une colonne "jour" dans df_long (à faire avant le ggplot)
df_long$jour <- factor(
  paste0("J-", as.numeric(auj - df_long$date)),
  levels = paste0("J-", 1:7)
)

# Filtrer les données à 7 derniers jours
df_7jours <- df_long[df_long$date %in% (auj - 1:7), ]


ggplot() +
  geom_line(data = moy_heure, aes(x = Heure, y = moy_heure), color = "#ffd700", linetype = "solid", linewidth = 1) +
  geom_line(data = df_7jours, aes(x = Heure, y = Production, color = jour), linetype = "dashed", linewidth = 0.5) +
              scale_color_manual(
                name = "Jour",
                values = c(
                  "J-1" = "green",
                  "J-2" = "lightgreen",
                  "J-3" = "springgreen",
                  "J-4" = "tomato4",
                  "J-5" = "gainsboro",
                  "J-6" = "deepskyblue3",
                  "J-7" = "darkorchid2"
                )
              ) +
  labs(title = "Production horaire des panneaux solaires, 7 derniers jours", x = "Heure de la journée",y = "Production (Wh)") +
  scale_x_continuous(breaks = seq(0, 23, 1)) +
  theme_gray() +
  theme(axis.text.x = element_text(angle=90, vjust = 0.5, hjust = 1), legend.position = "bottom")

###
# Table quotidien
quot[, production := production/1000][, economie := round(((domestique + batterie) * prix_achat) + (reseau * prix_vente), 2)]
