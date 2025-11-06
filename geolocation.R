# --- geolocalisation_depuis_correction.R ---
# Force le géocodage ArcGIS pour TOUTES les lignes
# Source : nasa_disaster_correction.csv
# Schéma attendu : id,country,geolocation,level,adm1,location,disastertype,continent

source("Packages.R")

entrée  <- "nasa_disaster_correction.csv"
sortie  <- "nasa_disaster_correction.csv"   # On réécrit au même endroit

Taille_lot       <- 300   # Ajuste entre 100–300 selon la stabilité
Nb_tentatives    <- 3
Pause_erreur     <- 2     # Pause progressive en cas d’erreur
Pause_lots       <- 1     # Pause entre lots (secondes)

# ---------------- Fonctions utilitaires ----------------
nettoyer_adresse <- function(texte) {
  texte <- trimws(texte)
  texte <- gsub("\\s+", " ", texte)
  texte <- gsub(",\\s*,", ", ", texte)      # évite ", ,"
  texte <- gsub("^,\\s*|\\s*,$", "", texte) # évite virgule au début/fin
  texte
}

former_geolocation <- function(lat, long) {
  ifelse(is.na(lat) | is.na(long), NA, sprintf("%.7f, %.7f", lat, long))
}

# ✅ Correction : valeurs fixes dans les arguments par défaut
geolocation_arcgis <- function(vecteur_adresses, Nb_tentatives = 3, pause_base = 2) {
  for (tentative in seq_len(Nb_tentatives)) {
    resultat <- tryCatch(
      geo(address = vecteur_adresses, method = "arcgis", limit = 1, quiet = TRUE),
      error = function(e) {
        message(sprintf("   ⚠️ Erreur ArcGIS (tentative %d/%d) : %s",
                        tentative, Nb_tentatives, conditionMessage(e)))
        NULL
      }
    )
    
    # Si on a bien obtenu un résultat complet
    if (!is.null(resultat) && all(is.element(c("address", "lat", "long"), names(resultat)))) {
      return(resultat)
    }
    
    # Pause croissante avant la prochaine tentative
    Sys.sleep(pause_base * tentative)
  }
  
  # Si toutes les tentatives échouent, retourner NA
  data.frame(address = vecteur_adresses, lat = NA_real_, long = NA_real_)
}

# 0) Lecture de la base de données
base_de_données <- read.csv(entrée, check.names = FALSE, fileEncoding = "UTF-8")
noms_origine <- names(base_de_données)
colonnes_requises <- c("location", "adm1", "country", "geolocation")
colonnes_manquantes <- setdiff(colonnes_requises, names(base_de_données))
if (length(colonnes_manquantes)) {
  stop("Colonnes manquantes dans le fichier d’entrée : ",
       paste(colonnes_manquantes, collapse = ", "))
}

# --- Correction des anciennes régions du Pakistan ---
base_de_données$adm1 <- gsub("(?i)\\bN\\.?\\s*W\\.?\\s*F\\.?\\s*P\\.?\\b",
                             "Khyber Pakhtunkhwa", base_de_données$adm1, perl = TRUE)
base_de_données$adm1 <- gsub("(?i)\\bF\\.?\\s*A\\.?\\s*T\\.?\\s*A\\.?\\b",
                             "Khyber Pakhtunkhwa", base_de_données$adm1, perl = TRUE)

base_de_données$location <- gsub("(?i)\\bN\\.?\\s*W\\.?\\s*F\\.?\\s*P\\.?\\b",
                                 "Khyber Pakhtunkhwa", base_de_données$location, perl = TRUE)
base_de_données$location <- gsub("(?i)\\bF\\.?\\s*A\\.?\\s*T\\.?\\s*A\\.?\\b",
                                 "Khyber Pakhtunkhwa", base_de_données$location, perl = TRUE)

# 1) Construire les adresses pour TOUTES les lignes
adresses_completes <- nettoyer_adresse(
  paste(base_de_données$location, base_de_données$adm1, base_de_données$country, sep = ", ")
)

adresses_uniques <- sort(unique(na.omit(adresses_completes)))
adresses_uniques <- adresses_uniques[adresses_uniques != ""]
cat("Nombre d’adresses uniques à géocoder :", length(adresses_uniques), "\n")

if (length(adresses_uniques) == 0) {
  warning("Aucune adresse exploitable. 'geolocation' sera mise à NA.")
  base_de_données$geolocation <- NA
  write.csv(base_de_données, sortie, row.names = FALSE, fileEncoding = "UTF-8")
  cat("Fichier écrit :", sortie, "\n")
  invisible(NULL)
} else {
  # 2) Géocodage par lots (ArcGIS)
  nb_total <- length(adresses_uniques)
  nb_lots <- ceiling(nb_total / Taille_lot)
  resultats_lots <- vector("list", nb_lots)
  
  ancien_timeout <- getOption("timeout")
  options(timeout = max(600, ancien_timeout))
  
  for (i in seq_len(nb_lots)) {
    debut <- (i - 1) * Taille_lot + 1
    fin   <- min(i * Taille_lot, nb_total)
    cat(sprintf("Lot %d/%d : lignes %d -> %d\n", i, nb_lots, debut, fin))
    
    vecteur <- adresses_uniques[debut:fin]
    
    # ✅ Appel inchangé : pas de passage d'arguments globaux ici
    resultats_lots[[i]] <- geolocation_arcgis(vecteur)
    
    Sys.sleep(Pause_lots)
  }
  
  options(timeout = ancien_timeout)
  
  geo_resultats <- bind_rows(resultats_lots)  # colonnes : address, lat, long
  
  # 3) Associer les résultats à chaque ligne
  latitude  <- geo_resultats$lat[match(adresses_completes, geo_resultats$address)]
  longitude <- geo_resultats$long[match(adresses_completes, geo_resultats$address)]
  base_de_données$geolocation <- former_geolocation(latitude, longitude)
  
  # 4) Vérification de la structure du fichier
  if (!identical(names(base_de_données), noms_origine)) {
    cat("Colonnes attendues :", paste(noms_origine, collapse = ", "), "\n")
    cat("Colonnes actuelles :", paste(names(base_de_données), collapse = ", "), "\n")
    stop("ERREUR : la structure du fichier a été modifiée.")
  }
  
  # 5) Sauvegarder le fichier final
  write.csv(base_de_données, sortie, row.names = FALSE, fileEncoding = "UTF-8")
  
  # 6) Résumé
  nb_coordonnees <- sum(!is.na(base_de_données$geolocation))
  nb_vide <- sum(is.na(base_de_données$geolocation))
  
  cat("✅ Géocodage terminé avec ArcGIS.\n")
  cat("- Coordonnées trouvées :", nb_coordonnees, "\n")
  cat("- Coordonnées manquantes :", nb_vide, "\n")
  cat("Fichier mis à jour :", sortie, "\n")
}
