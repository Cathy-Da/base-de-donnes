# pas sur que je le garde comme cela, les cartes ne sont pas bien.et pas de legende ni de titre.
# --- Map_Disasters_ByCity.R ---
# Carte mondiale des catastrophes par ville et par type
# Requiert : ggplot2, maps, mapdata (tu les as déjà), et ton CSV normalisé.

suppressWarnings(suppressMessages({
  library(ggplot2)
  library(maps)
  library(mapdata)
}))

INFILE <- "nasa_disaster_correction.csv"

# ---------- Helpers ----------
parse_geolocation <- function(s){
  # s attendu: "lat, lon"
  lat <- lon <- rep(NA_real_, length(s))
  ok <- !is.na(s) & nzchar(s)
  parts <- strsplit(s[ok], ",")
  # remplir lat/lon (sécurisé)
  for (i in seq_along(parts)) {
    p <- trimws(parts[[i]])
    if (length(p) == 2) {
      lat[which(ok)[i]] <- suppressWarnings(as.numeric(p[1]))
      lon[which(ok)[i]] <- suppressWarnings(as.numeric(p[2]))
    }
  }
  list(lat = lat, lon = lon)
}

# ---------- Lire les données ----------
x <- read.csv(INFILE, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")

# Vérifs de base
req <- c("id","country","geolocation","adm1","location","disastertype","continent")
miss <- setdiff(req, names(x))
if (length(miss)) stop("Colonnes manquantes: ", paste(miss, collapse = ", "))

# Parser geolocation -> lat/lon
pp <- parse_geolocation(x$geolocation)
x$lat <- pp$lat
x$lon <- pp$lon

# Conserver lignes valides
valid <- !is.na(x$lat) & !is.na(x$lon) & is.finite(x$lat) & is.finite(x$lon) &
  x$lat >= -90 & x$lat <= 90 & x$lon >= -180 & x$lon <= 180
d <- x[valid, c("location","adm1","country","disastertype","lat","lon","continent")]

# Agrégation par (ville, adm1, pays, type) pour limiter la sur-plot
# On compte le nombre d'événements par combinaison
key <- paste(d$location, d$adm1, d$country, d$disastertype, sep = " | ")
# indices uniques
idx_first <- !duplicated(key)
# somme par clé
counts <- table(key)
# récupérer lat/lon/infos de la première occurrence (elles sont identiques pour une même ville)
agg <- d[idx_first, ]
agg$count <- as.integer(counts[key[idx_first]])

# (Optionnel) limiter les types très rares à "Other"
# Seuil = 100 événements (ajuste si besoin)
min_events_type <- 100
type_counts <- sort(table(agg$disastertype), decreasing = TRUE)
rare_types <- names(type_counts[type_counts < min_events_type])
if (length(rare_types)) {
  agg$disastertype <- ifelse(agg$disastertype %in% rare_types, "Other", agg$disastertype)
}

# ---------- Données carte ----------
world <- map_data("world")  # de 'maps'

# Palette discrète
# (ggplot choisira des couleurs distinctes; ajuste si tu veux un scale_brewer)
# scale_color_brewer(palette="Set2") est une bonne option si désirée.
base_theme <- theme_minimal(base_family = NULL) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )

# ---------- Carte globale (tous types) ----------
p_all <- ggplot() +
  geom_polygon(data = world, aes(long, lat, group = group), fill = "grey95", colour = "grey80", linewidth = 0.2) +
  geom_point(
    data = agg,
    aes(x = lon, y = lat, colour = disastertype, size = pmin(count, 50)), # cap visuel à 50
    alpha = 0.75
  ) +
  scale_size_continuous(name = "Nb événements (par ville/type)", range = c(0.6, 5)) +
  guides(size = guide_legend(override.aes = list(alpha = 1))) +
  labs(
    title = "Catastrophes naturelles par ville",
    subtitle = "Couleur = type de catastrophe • Taille = nombre d'événements (par ville et type)",
    caption = "Source: nasa_disaster_correction.csv"
  ) +
  coord_fixed(1.3) +
  base_theme

# Sauvegarde
ggsave("map_disasters_global.png", p_all, width = 13, height = 7.5, dpi = 220)

# ---------- Facets par type (grille) ----------
# Pour éviter trop de cases, on garde les 8 types les plus fréquents (+ "Other" si présent)
top_n_types <- 8
type_counts2 <- sort(table(agg$disastertype), decreasing = TRUE)
keep_types <- names(type_counts2)[seq_len(min(top_n_types, length(type_counts2)))]
if ("Other" %in% agg$disastertype && !("Other" %in% keep_types)) {
  keep_types <- c(keep_types, "Other")
}
agg_small <- agg[agg$disastertype %in% keep_types, ]

p_facets <- ggplot() +
  geom_polygon(data = world, aes(long, lat, group = group), fill = "grey95", colour = "grey80", linewidth = 0.2) +
  geom_point(
    data = agg_small,
    aes(x = lon, y = lat, colour = disastertype, size = pmin(count, 50)),
    alpha = 0.8
  ) +
  scale_size_continuous(name = "Nb événements", range = c(0.6, 4.5)) +
  guides(size = guide_legend(override.aes = list(alpha = 1))) +
  labs(
    title = "Catastrophes par type (facettes)",
    subtitle = paste0("Top ", length(keep_types), " types • Taille = nb d'événements (par ville)"),
    caption = "Source: nasa_disaster_correction.csv"
  ) +
  coord_fixed(1.3) +
  facet_wrap(~ disastertype, ncol = 3) +
  base_theme

ggsave("map_disasters_bytype.png", p_facets, width = 13, height = 9.5, dpi = 220)

# ---------- Messages finaux ----------
cat("✅ Cartes générées :\n")
cat("- map_disasters_global.png (carte globale, tous types)\n")
cat("- map_disasters_bytype.png (grille par type)\n")

# ---------- (Optionnel) Zoom par continent ----------
# Exemple d'usage :
# continent_to_plot <- "Asia"  # "Europe", "Africa", ...
# sub <- agg[agg$continent == continent_to_plot, ]
# p_zoom <- ggplot() +
#   geom_polygon(data = world, aes(long, lat, group = group), fill = "grey95", colour = "grey80", linewidth = 0.2) +
#   geom_point(data = sub, aes(lon, lat, colour = disastertype, size = pmin(count, 50)), alpha = 0.8) +
#   coord_fixed(1.3, xlim = range(sub$lon, na.rm = TRUE) + c(-10,10), ylim = range(sub$lat, na.rm = TRUE) + c(-6,6)) +
#   base_theme +
#   labs(title = paste("Zoom -", continent_to_plot))
# ggsave(paste0("map_disasters_", tolower(continent_to_plot), ".png"), p_zoom, width = 10, height = 7, dpi = 220)
