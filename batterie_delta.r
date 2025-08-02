# ===================================================================== #
# Script pour le calcul du temps de chargement de la batterie EcoFlow   #
# ou inversement pour calculer le pourcentage après x temps             #
# ===================================================================== #

rm(list = ls())

# Calcul du temps restant # Pourcentage de départ et d'arrivée, puissance en Wh
temps_restant <- function(pc_dep, pc_arr, puiss) {

  t = ((pc_arr-pc_dep)*2048)/puiss  
  
  heures <- floor(t)
  minutes <- round((t-heures) * 60)
  
  paste0(heures, " heure", ifelse(heures == 1, "", "s"), " et ", minutes, " minute", ifelse(minutes == 1, " ", "s "), "(", round(t, 4), ")")  
}

pc_arr = .8
pc_dep = .63
puiss = 600

temps_restant(pc_dep = pc_dep, pc_arr = pc_arr, puiss = puiss)


# Calcul du pourcentage final après x temps # Pourcentage départ, puissance Wh, temps
pc_cible <- function(t, pc_dep, puiss) {
  pc_arr = ((t * puiss) / 2048) + pc_dep
  
  return(pc_arr)
}

pc_cible(t=.93, pc_dep=.54, puiss = 303)
