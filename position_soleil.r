# ----------------------- #
# Analyse position soleil #
# ----------------------- #

library(suncalc)
#install.packages("solartime")
library(solartime)
library(ggplot2)
library(dplyr)
library(lubridate)


## Exemples basiques
# suncalc
getSunlightPosition(date = as.POSIXct("2025-06-21 12:00:00", tz = "UTC"),
                    lat = 46.223307, lon = 7.286646)


# solartime
computeSunPosition(timestamp =  as.POSIXct("2025-06-21 12:00:00", tz = "UTC"), latDeg = 46.223307, longDeg = 7.286646)



## Position
lat = 46.223307
lon = 7.286646

# Séquence de date
date_seq <- seq(
  as.POSIXct("2025-06-03 00:00:00", tz = "Europe/Paris"),
  as.POSIXct("2025-06-03 23:59:00", tz = "Europe/Paris"),
  by = "10 min"
)

# 3 Calcul de la position du soleil
sun_positions <- getSunlightPosition(date = date_seq, lat = lat, lon = lon)
sun_positions$time <- date_seq
sun_positions$elevation_deg <- sun_positions$altitude * 180 / pi  # Conversion radians → degrés


# 4. Tracer la courbe de la hauteur du soleil
ggplot(sun_positions, aes(x = time, y = elevation_deg)) +
  geom_line(color = "orange", size = 1) +
  labs(
    title = "Hauteur du Soleil à Vétroz le 14 mai 2025",
    x = "Heure (UTC)",
    y = "Élévation solaire (°)"
  ) +
  theme_minimal()


# --------- #
# Plusieurs jours

# 2. Définir la fonction pour obtenir la courbe solaire d’un jour
get_sun_data_for_day <- function(date_str, lat, lon, tz = "Europe/Paris") {
  date_seq <- seq(
    as.POSIXct(paste0(date_str, " 00:00:00"), tz = tz),
    as.POSIXct(paste0(date_str, " 23:59:00"), tz = tz),
    by = "10 min"
  )
  
  sun_pos <- getSunlightPosition(date = date_seq, lat = lat, lon = lon)
  data.frame(
    time = date_seq,
    elevation = sun_pos$altitude * 180 / pi,
    date = as.Date(date_str)
  )
}

# 3. Récupérer les données pour plusieurs jours
dates <- as.character(seq(as.Date("2025-06-03"), as.Date("2025-06-07"), by = "1 day"))

sun_data <- bind_rows(lapply(dates, get_sun_data_for_day, lat = lat, lon = lon))


# 4. Tracer les courbes
ggplot(sun_data, aes(x = time, y = elevation, color = as.factor(date))) +
  geom_line(size = 1) +
  labs(
    title = "Hauteur du Soleil à Vétroz autour du Solstice (2025)",
    x = "Heure locale",
    y = "Élévation solaire (°)",
    color = "Date"
  ) +
  scale_x_datetime(date_labels = "%H:%M") +
  theme_minimal()


# ----------------------------------- #
# Production d'énergie photovoltaïque #
# ----------------------------------- #

# Paramètres de base
inclinaison_deg <- 30   # Inclinaison du panneau
puissance_max <- 600    # Puissance crête du panneau (en W)

# 📅 Générer une séquence horaire pour un jour
date_seq <- seq(
  as.POSIXct("2025-06-03 00:00:00", tz = "Europe/Paris"),
  as.POSIXct("2025-06-03 23:59:00", tz = "Europe/Paris"),
  by = "5 min"
)

# ☀️ Récupérer la position du soleil
sun <- getSunlightPosition(date = date_seq, lat = lat, lon = lon)
sun$time <- date_seq
sun$elevation_deg <- sun$altitude * 180 / pi
sun$azimuth_deg <- sun$azimuth * 180 / pi



# 🎯 Calcul de l’angle d’incidence
sun <- sun %>%
  mutate(
    # Direction du panneau (sud = 180°)
    panel_azimuth = 180,
    panel_tilt = inclinaison_deg,
    
    # Angle d’incidence simplifié (formule plane)
    incidence_angle = acos(
      cos(panel_tilt * pi/180) * sin(altitude) +
        sin(panel_tilt * pi/180) * cos(altitude) * cos((azimuth_deg - panel_azimuth) * pi/180)
    ),
    
    incidence_deg = incidence_angle * 180 / pi,
    
    # Irradiance reçue (simplifié, max 1000 W/m² quand incidence = 0)
    irradiance = ifelse(elevation_deg > 0,
                        1000 * cos(incidence_angle),
                        0),
    
    # Puissance instantanée (limitée à la puissance du panneau)
    power_W = pmin(irradiance, 1000) * (puissance_max / 1000)
  )

# 🔋 Énergie journalière (en Wh)
sun <- sun %>%
  mutate(energy_Wh = power_W * 5 / 60)  # 5 minutes → fraction d'heure

total_energy <- sum(sun$energy_Wh, na.rm = TRUE)
cat("Energie totale produite le 03 juin 2025: ", round(total_energy), "Wh\n")

# 📈 Visualisation de la puissance au cours de la journée
ggplot(sun, aes(x = time, y = power_W)) +
  geom_line(color = "orange1", size = 1) +
  labs(
    title = "Production instantanée d'un panneau 600W à Vétroz (15 mai 2025)",
    x = "Heure",
    y = "Puissance (W)"
  ) +
  theme_minimal()
