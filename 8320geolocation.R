# Geocode_arcgis_missing_only.R — remplir 'geolocation' manquantes avec ArcGIS

library(tidygeocoder)
library(stringi)
library(dplyr)

# 0) Charger la base
data <- read.csv("nasa_disaster_correction.csv", stringsAsFactors = FALSE, check.names = FALSE)

# 1) Repérer les lignes à géocoder
geo_clean <- trimws(as.character(data$geolocation))
is_bad <- is.na(geo_clean) | geo_clean == "" | geo_clean == "NA, NA"
cat("À géocoder (ArcGIS) :", sum(is_bad), "/", nrow(data), "lignes\n")

if (sum(is_bad) == 0) {
  cat("Rien à faire : geolocation est déjà remplie partout.\n")
} else {
  
  # 2) Normalisation douce pour construire des adresses propres
  normalize_text <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x <- gsub("\\s+", " ", x)
    x <- stri_trans_general(x, "Any-Latin; Latin-ASCII")  # enlève accents/encodages bizarres
    x <- tolower(x)
    x[x %in% c("", "na", "n/a", "none", "null", "unknown")] <- NA
    x
  }
  
  sub <- data[is_bad, c("location","adm1","country")]
  sub$location_clean <- normalize_text(sub$location)
  sub$adm1_clean     <- normalize_text(sub$adm1)
  sub$country_clean  <- normalize_text(sub$country)
  
  # 3) Construire l'adresse la plus informative possible
  sub$address <- ifelse(!is.na(sub$location_clean) & nzchar(sub$location_clean),
                        paste(sub$location_clean, sub$adm1_clean, sub$country_clean, sep=", "),
                        paste(sub$adm1_clean, sub$country_clean, sep=", "))
  only_country <- is.na(sub$address) | sub$address %in% c(", NA","NA, NA","NA, NA, NA","NA,")
  sub$address[only_country] <- sub$country_clean[only_country]
  
  # 4) Adresses uniques à géocoder
  addr_df <- data.frame(address = sort(unique(na.omit(sub$address))), stringsAsFactors = FALSE)
  cat("Adresses uniques à géocoder (ArcGIS) :", nrow(addr_df), "\n")
  
  # 5) Géocodage par lots (ArcGIS) sur un VECTEUR d'adresses
  chunk_size <- 1000   # ajuste si besoin (ArcGIS tolère généralement bien)
  n <- nrow(addr_df)
  res_list <- vector("list", ceiling(n / chunk_size))
  
  for (i in seq_along(res_list)) {
    idx_from <- (i - 1) * chunk_size + 1
    idx_to   <- min(i * chunk_size, n)
    cat(sprintf("  Lot %d/%d : adresses %d à %d (ArcGIS)...\n", i, length(res_list), idx_from, idx_to))
    
    addr_vec <- addr_df$address[idx_from:idx_to]
    
    res_list[[i]] <- tryCatch(
      geo(address = addr_vec, method = "arcgis", limit = 1, quiet = TRUE),
      error = function(e) {
        cat("    ⚠️  Erreur géocodage lot ", i, ": ", conditionMessage(e), "\n", sep = "")
        data.frame(address = addr_vec, lat = NA_real_, long = NA_real_, stringsAsFactors = FALSE)
      }
    )
    
    Sys.sleep(0.5)  # petite pause de courtoisie
  }
  
  geo_res <- bind_rows(res_list)  # colonnes: address, lat, long
  
  # 6) Rejoindre aux lignes à corriger
  sub2 <- sub %>% left_join(geo_res, by = "address")
  
  # 7) Construire "lat, lon" uniquement quand les deux valeurs existent
  to_text <- function(lat, long) {
    if (is.na(lat) || is.na(long)) return(NA_character_)
    sprintf("%.7f, %.7f", lat, long)
  }
  new_geo <- mapply(to_text, sub2$lat, sub2$long)
  
  # 8) Mettre à jour 'geolocation' uniquement pour les lignes manquantes ET géocodées
  idx_bad <- which(is_bad)
  ok <- which(!is.na(new_geo))
  data$geolocation[idx_bad[ok]] <- new_geo[ok]
  
  # 9) Écriture
  write.csv(data, "nasa_disaster_correction.csv", row.names = FALSE)
  
  # 10) Bilan final
  geo_clean2 <- trimws(as.character(data$geolocation))
  still_bad <- is.na(geo_clean2) | geo_clean2 == "" | geo_clean2 == "NA, NA"
  cat("Remplissages effectués (ArcGIS) :", length(ok), "\n")
  cat("Reste sans coordonnées :", sum(still_bad), "\n")
  cat("OK : 'geolocation' mise à jour uniquement là où c'était vide (ArcGIS).\n")
}
