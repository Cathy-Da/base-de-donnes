# ArcGIS_chunked_retry.R — géocodage robuste, UTF-8, sans changer la structure (9 colonnes)

library(tidygeocoder)
library(dplyr)
library(stringi)

# ---------------------------
# Paramètres
# ---------------------------
CHUNK_SIZE   <- 300        # 100–300 recommandé
MAX_RETRIES  <- 3
SLEEP_RETRY  <- 2          # backoff: r * SLEEP_RETRY
SLEEP_BETWEEN_CHUNKS <- 1  # pause entre lots
CACHE_FILE   <- "geocode_cache_arcgis.csv"  # petit cache pour éviter les re-calls inutiles

# ---------------------------
# Utilitaires
# ---------------------------
# Nettoyage minimal de l'adresse (on garde les accents !)
clean_addr <- function(x) {
  x <- trimws(x)
  x <- gsub("\\s+", " ", x)
  x
}

to_txt <- function(lat, long) ifelse(is.na(lat) | is.na(long), NA, sprintf("%.7f, %.7f", lat, long))

# Appel ArcGIS avec retries/backoff
geo_arcgis_retry <- function(addr_vec, max_retries = MAX_RETRIES, sleep_base = SLEEP_RETRY) {
  for (r in seq_len(max_retries)) {
    res <- tryCatch(
      geo(address = addr_vec, method = "arcgis", limit = 1, quiet = TRUE),
      error = function(e) {
        message(sprintf("   ⚠️ ArcGIS erreur (retry %d/%d): %s", r, max_retries, conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(res) && all(c("address","lat","long") %in% names(res))) return(res)
    Sys.sleep(sleep_base * r)  # backoff
  }
  # si échec: DF compatible pour ces adresses
  data.frame(address = addr_vec, lat = NA_real_, long = NA_real_, stringsAsFactors = FALSE)
}

# ---------------------------
# 0) Lire la base (UTF-8) et garde-fous
# ---------------------------
data <- read.csv(
  "nasa_disaster_correction.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8"
)

# ---------------------------
# 1) Construire l'adresse (temporaire)
# ---------------------------
data$full_address <- paste(data$location, data$adm1, data$country, sep = ", ")
data$full_address <- clean_addr(data$full_address)

# ---------------------------
# 2) Extraire adresses uniques à traiter
#    - on ne traite pas les adresses déjà présentes au cache
#    - on ignore NA et vides
# ---------------------------
addr_unique <- sort(unique(na.omit(data$full_address)))
addr_unique <- addr_unique[addr_unique != ""]

cat("Adresses uniques (potentielles) :", length(addr_unique), "\n")

# Charger cache si présent
cache <- if (file.exists(CACHE_FILE)) {
  read.csv(CACHE_FILE, stringsAsFactors = FALSE, fileEncoding = "UTF-8", check.names = FALSE)
} else {
  data.frame(address = character(), lat = numeric(), long = numeric(), stringsAsFactors = FALSE)
}

# Filtrer ce qui manque encore (non résolu)
addr_todo <- setdiff(addr_unique, cache$address)
cat("Adresses à géocoder (après cache) :", length(addr_todo), "\n")

# ---------------------------
# 3) Allonger le timeout R le temps du run
# ---------------------------
old_to <- getOption("timeout")
options(timeout = max(600, old_to))

# ---------------------------
# 4) Géocoder par lots ce qui reste uniquement
# ---------------------------
geo_new <- NULL
if (length(addr_todo) > 0) {
  n <- length(addr_todo)
  n_chunks <- ceiling(n / CHUNK_SIZE)
  res_list <- vector("list", n_chunks)
  
  for (i in seq_len(n_chunks)) {
    from <- (i - 1) * CHUNK_SIZE + 1
    to   <- min(i * CHUNK_SIZE, n)
    cat(sprintf("Lot %d/%d : %d -> %d\n", i, n_chunks, from, to))
    vec <- addr_todo[from:to]
    res_list[[i]] <- geo_arcgis_retry(vec)
    Sys.sleep(SLEEP_BETWEEN_CHUNKS)
  }
  geo_new <- bind_rows(res_list)
} else {
  geo_new <- data.frame(address = character(), lat = numeric(), long = numeric(), stringsAsFactors = FALSE)
}

# ---------------------------
# 5) Mettre à jour le cache (en UTF-8)
# ---------------------------
cache2 <- bind_rows(cache, geo_new) |>
  distinct(address, .keep_all = TRUE)

con_cache <- file(CACHE_FILE, open = "w", encoding = "UTF-8")
write.csv(cache2, con_cache, row.names = FALSE)
close(con_cache)

# ---------------------------
# 6) Créer la table finale address→(lat,long) (cache + nouveau)
# ---------------------------
geo_res <- cache2

# ---------------------------
# 7) Remapper vers toutes les lignes et mettre à jour geolocation
# ---------------------------
lat_map  <- geo_res$lat[match(data$full_address,  geo_res$address)]
long_map <- geo_res$long[match(data$full_address, geo_res$address)]
data$geolocation <- to_txt(lat_map, long_map)

# ---------------------------
# 8) Nettoyage (retirer la colonne temporaire) et garde-fous structure
# ---------------------------
data$full_address <- NULL
stopifnot(identical(names(data), orig_names))  # on doit revenir à exactement 9 colonnes

# ---------------------------
# 9) Sauvegarder (UTF-8) + bilan
# ---------------------------
con <- file("nasa_disaster_correction.csv", open = "w", encoding = "UTF-8")
write.csv(data, con, row.names = FALSE)
close(con)

nb_ok  <- sum(!is.na(data$geolocation))
nb_na  <- sum(is.na(data$geolocation))
cat("Terminé. Coordonnées remplies :", nb_ok, " | Restant sans coordonnées :", nb_na, "\n")

# Remettre le timeout d'avant
options(timeout = old_to)
