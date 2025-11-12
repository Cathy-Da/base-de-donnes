source("Packages.R")

entrée  <- "nasa_disaster_correction.csv"
sortie  <- "nasa_disaster_correction.csv"

données <- read.csv(entrée, check.names = FALSE, fileEncoding = "UTF-8")

# Renommer la première colonne (colonne 1) en 'id'
names(données)[1] <- "id"

# Supprimer la deuxième colonne (colonne 2)
données <- données[, -2, drop = FALSE]

# Sauvegarde
write.csv(données, sortie, row.names = FALSE, fileEncoding = "UTF-8")

# Vérifier doublons et NA dans 'id'
na_id <- any(is.na(données$id))
doublon_id <- anyDuplicated(données$id) > 0

if (na_id || doublon_id) {
  cat("⚠️ Problèmes détectés dans 'id' :\n")
  if (na_id) cat("- Valeurs manquantes dans id (NA)\n")
  if (doublon_id) cat("- Doublons dans id\n")
} else {
  cat("✅ OK : 'id' sans doublon et sans NA\n")
}

# Diagnostic
print(names(données))
