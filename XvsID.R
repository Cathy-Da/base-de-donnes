# --- XvsID.R (forcer la colonne sans nom -> id, supprimer l'ancienne id) ---

INFILE  <- "nasa_disaster_correction.csv"
OUTFILE <- "nasa_disaster_correction.csv"

# Charger tel quel en UTF-8
x <- read.csv(INFILE, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")

# Noms bruts (trim)
names(x) <- trimws(names(x))

# Candidats colonnes anonymes
bad_candidates <- c("", "X", "X.1", "Unnamed: 0", "ï..")

# Étape 1 : détecter une colonne anonyme
idx_bad <- which(names(x) %in% bad_candidates)

if (length(idx_bad) >= 1) {
  # renommer la première colonne anonyme en 'id'
  names(x)[idx_bad[1]] <- "id"
  
  # supprimer toute autre colonne anonyme éventuelle
  if (length(idx_bad) > 1) {
    x <- x[, -idx_bad[-1], drop = FALSE]
  }
  
  # supprimer une éventuelle colonne 'id' déjà existante (celle qui n'est pas la renommer)
  idx_id <- which(names(x) == "id")
  if (length(idx_id) > 1) {
    # garder seulement la première (celle renommée) et supprimer l'autre
    keep <- seq_along(names(x))
    keep <- keep[-idx_id[-1]]
    x <- x[, keep, drop = FALSE]
  }
  
} else {
  stop("ECHEC : aucune colonne anonyme trouvée pour devenir 'id'.")
}

# Sauvegarde
write.csv(x, OUTFILE, row.names = FALSE, fileEncoding = "UTF-8")

# Diagnostic
cat("OK : colonne sans nom renommée en 'id', ancienne 'id' supprimée si présente.\n")
print(names(x))
