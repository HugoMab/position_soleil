# ===================================================================== #
# Script pour le calcul du temps de chargement de la batterie EcoFlow   #
# ou inversement pour calculer le pourcentage après x temps             #
# ===================================================================== #

rm(list = ls())

# Calcul du temps restant # Pourcentage de départ et d'arrivée, puissance en Wh
temps_restant <- function(pc_dep, pc_arr, puiss) {
  t = ((pc_arr-pc_dep)*2048)/puiss  
  
  return(t)
}

temps_restant(pc_dep = 0.52, pc_arr = .8, puiss = 443)


# Calcul du pourcentage final après x temps # Pourcentage départ, puissance Wh, temps
pc_cible <- function(t, pc_dep, puiss) {
  pc_arr = ((t * puiss) / 2048) + pc_dep
  
  return(pc_arr)
}

pc_cible(.55, .68, puiss = 599)
