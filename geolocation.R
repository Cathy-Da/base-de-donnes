# --- geolocate_from_correction.R ---
# Force le géocodage ArcGIS pour TOUTES les lignes
# Source : nasa_disaster_correction.csv
# Schéma attendu : id,country,geolocation,level,adm1,location,disastertype,continent

source("Packages.R")

INFILE  <- "nasa_disaster_correction.csv"
OUTFILE <- "nasa_disaster_correction.csv"   # On réécrit au même endroit

CHUNK_SIZE    <- 300   # Ajuste entre 100–300 selon la stabilité
MAX_RETRIES   <- 3
SLEEP_RETRY   <- 2     # Pause progressive en cas d’erreur
SLEEP_BETWEEN <- 1     # Pause entre lots (secondes)

# ---------------- Utils ----------------
clean_addr <- function(x) {
  x <- trimws(x)
  x <- gsub("\\s+", " ", x)
  x <- gsub(",\\s*,", ", ", x)           # évite ", ,"
  x <- gsub("^,\\s*|\\s*,$", "", x)      # évite virgule en début/fin
  x
}

to_txt <- function(lat, long) {
  ifelse(is.na(lat) | is.na(long), NA, sprintf("%.7f, %.7f", lat, long))
}

geo_arcgis_retry <- function(addr_vec, max_retries = MAX_RETRIES, sleep_base = SLEEP_RETRY) {
  for (r in seq_len(max_retries)) {
    res <- tryCatch(
      geo(address = addr_vec, method = "arcgis", limit = 1, quiet = TRUE),
      error = function(e) {
        message(sprintf("   ⚠️ ArcGIS erreur (retry %d/%d): %s", r, max_retries, conditionMessage(e)))
        NULL
      }
    )
    if (!is.null(res) && all(c("address", "lat", "long") %in% names(res))) return(res)
    Sys.sleep(sleep_base * r)
  }
  # En dernier recours, renvoyer NA pour ces adresses
  data.frame(address = addr_vec, lat = NA_real_, long = NA_real_, stringsAsFactors = FALSE)
}

# 0) Lire la base (UTF-8)
data <- read.csv(INFILE, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
orig_names <- names(data)  # On préservera exactement cette structure
req <- c("location", "adm1", "country", "geolocation")
missing <- setdiff(req, names(data))
if (length(missing)) stop("Colonnes manquantes dans INFILE: ", paste(missing, collapse = ", "))

# --- Correction des anciennes régions du Pakistan ---
data$adm1 <- gsub("(?i)\\bN\\.?\\s*W\\.?\\s*F\\.?\\s*P\\.?\\b", "Khyber Pakhtunkhwa", data$adm1, perl = TRUE)
data$adm1 <- gsub("(?i)\\bF\\.?\\s*A\\.?\\s*T\\.?\\s*A\\.?\\b", "Khyber Pakhtunkhwa", data$adm1, perl = TRUE)

data$location <- gsub("(?i)\\bN\\.?\\s*W\\.?\\s*F\\.?\\s*P\\.?\\b", "Khyber Pakhtunkhwa", data$location, perl = TRUE)
data$location <- gsub("(?i)\\bF\\.?\\s*A\\.?\\s*T\\.?\\s*A\\.?\\b", "Khyber Pakhtunkhwa", data$location, perl = TRUE)

# 1) Construire les adresses pour TOUTES les lignes (on force le recalcul)
full_address <- clean_addr(paste(data$location, data$adm1, data$country, sep = ", "))

addr_unique <- sort(unique(na.omit(full_address)))
addr_unique <- addr_unique[addr_unique != ""]
cat("Adresses uniques à géocoder :", length(addr_unique), "\n")

if (length(addr_unique) == 0) {
  warning("Aucune adresse exploitable. 'geolocation' sera mise à NA.")
  data$geolocation <- NA
  write.csv(data, OUTFILE, row.names = FALSE)
  cat("Fichier écrit :", OUTFILE, "\n")
  invisible(NULL)
} else {
  # 2) Géocodage par lots (ArcGIS)
  n <- length(addr_unique)
  n_chunks <- ceiling(n / CHUNK_SIZE)
  res_list <- vector("list", n_chunks)
  
  old_to <- getOption("timeout"); options(timeout = max(600, old_to))
  
  for (i in seq_len(n_chunks)) {
    from <- (i - 1) * CHUNK_SIZE + 1
    to   <- min(i * CHUNK_SIZE, n)
    cat(sprintf("Lot %d/%d : %d -> %d\n", i, n_chunks, from, to))
    vec <- addr_unique[from:to]
    res_list[[i]] <- geo_arcgis_retry(vec)
    Sys.sleep(SLEEP_BETWEEN)
  }
  
  options(timeout = old_to)
  
  geo_res <- bind_rows(res_list)  # cols: address, lat, long
  
  # 3) Remapper vers toutes les lignes et (ré)écrire geolocation
  lat_map  <- geo_res$lat[match(full_address, geo_res$address)]
  long_map <- geo_res$long[match(full_address, geo_res$address)]
  data$geolocation <- to_txt(lat_map, long_map)
  
  # 4) Garde-fou: structure inchangée
  if (!identical(names(data), orig_names)) {
    cat("Colonnes attendues :", paste(orig_names, collapse = ", "), "\n")
    cat("Colonnes actuelles :", paste(names(data), collapse = ", "), "\n")
    stop("ECHEC : les noms de colonnes ne correspondent pas.")
  }
  
  # 5) Sauvegarder
  write.csv(data, OUTFILE, row.names = FALSE, fileEncoding = "UTF-8")
  
  # 6) Bilan
  nb_ok <- sum(!is.na(data$geolocation))
  nb_na <- sum(is.na(data$geolocation))
  cat("Terminé. Coordonnées remplies :", nb_ok, " | Restant sans coordonnées :", nb_na, "\n")
}
