# --- Carte_Catastrophes_ParVille.R ---
# Carte mondiale des catastrophes naturelles par ville et par type
# Requiert : ggplot2, maps, mapdata
# Source : nasa_disaster_correction.csv

source("Packages.R")

fichier_entrée <- "nasa_disaster_correction.csv"

# ---------- Fonction utilitaire : extraire les coordonnées ----------
extraire_coordonnees <- function(chaine) {
  # Format attendu : "latitude, longitude"
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

# ---------- Lecture du fichier principal ----------
base_de_donnees <- read.csv(
  fichier_entrée,
  check.names = FALSE,
  fileEncoding = "UTF-8"
)

# Vérification des colonnes requises
colonnes_requises <- c("id", "country", "geolocation", "adm1", "location", "disastertype", "continent")
colonnes_manquantes <- setdiff(colonnes_requises, names(base_de_donnees))
if (length(colonnes_manquantes)) {
  stop("Colonnes manquantes : ", paste(colonnes_manquantes, collapse = ", "))
}

# Extraction des coordonnées depuis la colonne geolocation
coordonnees <- extraire_coordonnees(base_de_donnees$geolocation)
base_de_donnees$latitude  <- coordonnees$lat
base_de_donnees$longitude <- coordonnees$lon

# Filtrer les lignes valides (coordonnées dans les limites géographiques)
valide <- !is.na(base_de_donnees$latitude) & !is.na(base_de_donnees$longitude) &
  is.finite(base_de_donnees$latitude) & is.finite(base_de_donnees$longitude) &
  base_de_donnees$latitude >= -90 & base_de_donnees$latitude <= 90 &
  base_de_donnees$longitude >= -180 & base_de_donnees$longitude <= 180

catastrophes <- base_de_donnees[valide, c("location", "adm1", "country", "disastertype", "latitude", "longitude", "continent")]

# ---------- Agrégation : regrouper les catastrophes par ville, région, pays et type ----------
cle <- paste(catastrophes$location, catastrophes$adm1, catastrophes$country, catastrophes$disastertype, sep = " | ")
index_premier <- !duplicated(cle)
comptes <- table(cle)
regroupe <- catastrophes[index_premier, ]
regroupe$nombre <- as.integer(comptes[cle[index_premier]])

# ---------- Données du fond de carte ----------
monde <- map_data("world")

# ---------- Thème graphique uniforme ----------
theme_carte <- theme_minimal(base_family = NULL) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    legend.background = element_rect(fill = "white", colour = "grey80"),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    legend.title = element_text(face = "bold")
  )

# ---------- Carte globale : toutes les catastrophes ----------
carte_globale <- ggplot() +
  geom_polygon(
    data = monde,
    aes(long, lat, group = group),
    fill = "grey95", colour = "grey80", linewidth = 0.2
  ) +
  geom_point(
    data = regroupe,
    aes(x = longitude, y = latitude, colour = disastertype, size = pmin(nombre, 50)),
    alpha = 0.75
  ) +
  scale_color_brewer(palette = "Set2", name = "Type de catastrophe") +
  scale_size_continuous(name = "Nombre d’événements (par ville et type)", range = c(0.6, 5)) +
  labs(
    title = "Carte mondiale des catastrophes naturelles",
    subtitle = "Couleur : type de catastrophe  •  Taille : nombre d’événements par ville et par type",
    caption = "Source : nasa_disaster_correction.csv"
  ) +
  coord_fixed(1.3) +
  theme_carte

# Sauvegarde de la carte globale avec fond blanc
ggsave(
  "Carte_Catastrophes_Global.png",
  carte_globale,
  width = 13, height = 7.5, dpi = 220, bg = "white"
)

# ---------- Carte par type de catastrophe ----------
types_disponibles <- sort(unique(regroupe$disastertype))
regroupe_filtre <- regroupe[regroupe$disastertype %in% types_disponibles, ]

carte_types <- ggplot() +
  geom_polygon(
    data = monde,
    aes(long, lat, group = group),
    fill = "grey95", colour = "grey80", linewidth = 0.2
  ) +
  geom_point(
    data = regroupe_filtre,
    aes(x = longitude, y = latitude, colour = disastertype, size = pmin(nombre, 50)),
    alpha = 0.8
  ) +
  scale_color_brewer(palette = "Set2", name = "Type de catastrophe") +
  scale_size_continuous(name = "Nombre d’événements", range = c(0.6, 4.5)) +
  labs(
    title = "Catastrophes naturelles par type",
    subtitle = paste("Types représentés :", length(types_disponibles)),
    caption = "Source : nasa_disaster_correction.csv"
  ) +
  coord_fixed(1.3) +
  facet_wrap(~ disastertype, ncol = 3) +
  theme_carte

# Sauvegarde de la carte par type avec fond blanc
ggsave(
  "Carte_Catastrophes_ParType.png",
  carte_types,
  width = 13, height = 9.5, dpi = 220, bg = "white"
)

# ---------- Résumé final ----------
cat("✅ Cartes générées avec succès :\n")
cat("- Carte_Catastrophes_Global.png : carte mondiale de toutes les catastrophes\n")
cat("- Carte_Catastrophes_ParType.png : carte détaillée par type de catastrophe\n")
