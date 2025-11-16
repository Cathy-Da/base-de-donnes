source("Packages.R")
fichier_entrée <- "nasa_disaster_correction.csv"

# Fonction
extraire_coordonnees <- function(chaine) {
  latitude <- longitude <- rep(NA_real_, length(chaine))
  ok <- !is.na(chaine) & nzchar(chaine)
  morceaux <- strsplit(chaine[ok], ",")
  for (i in seq_along(morceaux)) {
    morceaux_trim <- trimws(morceaux[[i]])
    if (length(morceaux_trim) == 2) {
      lat_val <- as.numeric(morceaux_trim[1])
      lon_val <- as.numeric(morceaux_trim[2])
      latitude[which(ok)[i]]  <- lat_val
      longitude[which(ok)[i]] <- lon_val
    }
  }
  list(lat = latitude, lon = longitude)
}

# Lecture
base_de_donnees <- read.csv(
  fichier_entrée,
  check.names = FALSE,
  fileEncoding = "UTF-8"
)

# Extraction des coordonnées
coordonnees <- extraire_coordonnees(base_de_donnees$geolocation)
base_de_donnees$latitude  <- coordonnees$lat
base_de_donnees$longitude <- coordonnees$lon

# Filtrer les lignes valides
valide <- !is.na(base_de_donnees$latitude) & !is.na(base_de_donnees$longitude) &
  base_de_donnees$latitude >= -90 & base_de_donnees$latitude <= 90 &
  base_de_donnees$longitude >= -180 & base_de_donnees$longitude <= 180

catastrophes <- base_de_donnees[valide, c("location", "adm1", "country", "disastertype", "latitude", "longitude", "continent")]
catastrophes$nombre <- 1

# Fond de carte
monde <- map_data("world")

# Thème graphique
theme_carte <- theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.background = element_rect(fill = "white", colour = "grey"),
    plot.title = element_text(face = "bold", size = 16),
    legend.title = element_text(face = "bold")
  )

#  Carte
carte_globale <- ggplot() +
  geom_polygon(
    data = monde,
    aes(long, lat, group = group),
    fill = "white", colour = "black", linewidth = 0.2
  ) +
  geom_point(
    data = catastrophes,
    aes(x = longitude, y = latitude, colour = disastertype),
    alpha = 0.35, size = 1.1
  ) +
  scale_color_brewer(palette = "Paired", name = "Type de catastrophe") +
  labs(
    title = "Carte mondiale des catastrophes naturelles (événements individuels)",
    caption = paste0(
      "Source : nasa_disaster_correction.csv\n",
      "Total d’événements : ", nrow(catastrophes)
    )
  ) +
  coord_sf(crs = 4326) +
  theme_carte

ggsave(
  "Carte_Catastrophes_Global.png",
  carte_globale,
  width = 13, height = 7.5, dpi = 220, bg = "white"
)

#  Carte par type
types_disponibles <- sort(unique(catastrophes$disastertype))
catastrophes_filtre <- catastrophes[catastrophes$disastertype %in% types_disponibles, ]

carte_types <- ggplot() +
  geom_polygon(
    data = monde,
    aes(long, lat, group = group),
    fill = "white", colour = "black", linewidth = 0.2
  ) +
  geom_point(
    data = catastrophes_filtre,
    aes(x = longitude, y = latitude, colour = disastertype),
    alpha = 0.4, size = 1.0
  ) +
  scale_color_brewer(palette = "Paired", name = "Type de catastrophe") +
  labs(
    title = "Catastrophes naturelles par type (événements individuels)",
    caption = paste0(
      "Source : nasa_disaster_correction.csv\n",
      "Total d’événements : ", nrow(catastrophes)
    )
  ) +
  coord_sf(crs = 4326) +
  facet_wrap(~ disastertype, ncol = 3) +
  theme_carte

ggsave(
  "Carte_Catastrophes_ParType.png",
  carte_types,
  width = 13, height = 9.5, dpi = 220, bg = "white"
)

cat("- Carte_Catastrophes_Global.png\n")
cat("- Carte_Catastrophes_ParType.png\n")
cat("- Nombre total d’événements tracés :", nrow(catastrophes), "\n")
