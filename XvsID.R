# --- XvsID.R (forcer la colonne sans nom -> id, supprimer l’ancienne id) ---

entrée  <- "nasa_disaster_correction.csv"
sortie  <- "nasa_disaster_correction.csv"

# Charger tel quel en UTF-8
données <- read.csv(entrée, check.names = FALSE, fileEncoding = "UTF-8")

# Nettoyer les noms de colonnes
names(données) <- trimws(names(données))

# Candidats de colonnes anonymes
colonne_enlever <- c("", "X", "X.1", "Unnamed: 0", "ï..")

# Étape 1 : détecter une colonne anonyme
index_autres <- which(is.element(names(données), colonne_enlever))

if (length(index_autres) >= 1) {
  # Renommer la première colonne anonyme en 'id'
  names(données)[index_autres[1]] <- "id"
  
  # Supprimer toute autre colonne anonyme éventuelle
  if (length(index_autres) > 1) {
    données <- données[, -index_autres[-1], drop = FALSE]
  }
  
  # Supprimer une éventuelle colonne 'id' déjà existante (celle qui n'est pas renommée)
  index_id <- which(names(données) == "id")
  if (length(index_id) > 1) {
    # Garder seulement la première (celle renommée) et supprimer les autres
    conserver <- seq_along(names(données))
    conserver <- conserver[-index_id[-1]]
    données <- données[, conserver, drop = FALSE]
  }
  
} else {
  stop("ECHEC : aucune colonne anonyme trouvée à renommer en 'id'.")
}

# Sauvegarde
write.csv(données, sortie, row.names = FALSE, fileEncoding = "UTF-8")

# Diagnostic
cat("✅ OK : colonne sans nom renommée en 'id', ancienne colonne 'id' supprimée si présente.\n")
print(names(données))
