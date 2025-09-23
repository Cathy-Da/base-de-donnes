# Fix_locations_from_original.R
# Objectif : corriger 'location' dans la base corrigée en s'appuyant sur la base originale,
# uniquement pour les lignes où 'geolocation' est manquante ("NA, NA", NA, ou vide).

library(stringi)

# 1) Lire les deux bases
orig <- read.csv("nasa_disaster_dataset.csv", stringsAsFactors = FALSE, check.names = FALSE)
corr <- read.csv("nasa_disaster_correction.csv", stringsAsFactors = FALSE, check.names = FALSE)

# 2) Garde-fous simples
if (nrow(orig) != nrow(corr)) {
  stop(sprintf("Les deux fichiers n'ont pas le même nombre de lignes: original=%d, corrigé=%d",
               nrow(orig), nrow(corr)))
}
if (!("location" %in% names(corr))) stop("La colonne 'location' est absente de la base corrigée.")
if (!("geolocation" %in% names(corr))) stop("La colonne 'geolocation' est absente de la base corrigée.")

# 3) Détection des lignes problématiques dans la base corrigée
geo_corr <- trimws(as.character(corr$geolocation))
is_bad <- is.na(geo_corr) | geo_corr == "" | geo_corr == "NA, NA"
cat("Lignes à corriger (geolocation manquante):", sum(is_bad), "\n")

# 4) Fonctions utilitaires
normalize_text <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x <- gsub("\\s+", " ", x)
  x <- stri_trans_general(x, "Any-Latin; Latin-ASCII")
  x <- tolower(x)
  x[x %in% c("", "na", "n/a", "none", "null", "unknown")] <- NA
  x
}

looks_like_coords <- function(s) {
  # TRUE si s ~ "lat, lon" avec deux nombres plausibles
  if (is.na(s) || !nzchar(s)) return(FALSE)
  m <- gregexpr("[-+]?[0-9]*\\.?[0-9]+", s, perl = TRUE)
  vals <- regmatches(s, m)[[1]]
  if (length(vals) < 2) return(FALSE)
  nums <- suppressWarnings(as.numeric(vals[1:2]))
  if (any(is.na(nums))) return(FALSE)
  latlon <- abs(nums[1]) <= 90  & abs(nums[2]) <= 180
  lonlat <- abs(nums[1]) <= 180 & abs(nums[2]) <= 90
  latlon || lonlat
}
looks_like_coords_vec <- function(v) vapply(v, looks_like_coords, logical(1), USE.NAMES = FALSE)

# 5) Construire un candidat de remplacement pour 'location' à partir de l'originale
orig_loc <- if ("location" %in% names(orig)) as.character(orig$location) else rep(NA_character_, nrow(orig))
orig_geo <- if ("geolocation" %in% names(orig)) as.character(orig$geolocation) else rep(NA_character_, nrow(orig))

cand_from_loc <- normalize_text(orig_loc)

cand_from_geo <- as.character(orig_geo)
mask_coords   <- looks_like_coords_vec(cand_from_geo)   # TRUE là où 'geolocation' originale ressemble à des coords
cand_from_geo[mask_coords] <- NA_character_             # on écarte les géolocs numériques (on veut un nom de lieu)
cand_from_geo <- normalize_text(cand_from_geo)

# Candidat final : prioriser le 'location' original ; sinon une 'geolocation' textuelle de secours
cand_final <- ifelse(!is.na(cand_from_loc) & nzchar(cand_from_loc), cand_from_loc, cand_from_geo)

# 6) Appliquer uniquement sur les lignes problématiques et si on a un candidat non vide
to_update <- which(is_bad & !is.na(cand_final) & nzchar(cand_final))
cat("Lignes où 'location' sera remplacé par un nom venant de l'originale :", length(to_update), "\n")

if (length(to_update) > 0) {
  # Sauvegarde des anciennes valeurs pour bilan (optionnel)
  old_vals <- corr$location[to_update]
  new_vals <- cand_final[to_update]
  
  corr$location[to_update] <- new_vals
  
  # Petit bilan des changements (optionnel)
  cat("Exemples de remplacements (jusqu'à 10) :\n")
  ex_n <- min(10, length(to_update))
  for (i in seq_len(ex_n)) {
    cat(sprintf("  - '%s'  ->  '%s'\n", old_vals[i], new_vals[i]))
  }
} else {
  cat("Aucun remplacement effectué (aucun candidat viable).\n")
}

# 7) Écrire le fichier (pas de nouvelles colonnes)
write.csv(corr, "nasa_disaster_correction.csv", row.names = FALSE)

# 8) Bilan après correction (combien restent à géocoder)
geo_corr2 <- trimws(as.character(corr$geolocation))
still_bad <- is.na(geo_corr2) | geo_corr2 == "" | geo_corr2 == "NA, NA"
cat("Reste de lignes sans coordonnées (à géocoder plus tard):", sum(still_bad), "\n")
cat("OK: 'location' mis à jour à partir de la base originale là où c'était utile.\n")
