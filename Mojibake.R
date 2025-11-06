# --- Détection de caractères problématiques dans le CSV initial ---

source("Packages.R")

# Lecture du fichier initial (sans modification)
data_raw <- read.csv(
  "nasa_disaster_dataset.csv",
  check.names = FALSE,
  fileEncoding = "UTF-8"
)

# Fonction pour détecter les caractères invisibles ou corrompus
detect_problemes <- function(x) {
  if (!is.character(x)) return(rep(FALSE, length(x)))
  grepl("Ã|Â|ƒ|�|[\u0080-\u009F]", x, perl = TRUE)
}

# Vérification colonne par colonne
resultats <- data.frame(
  colonne = character(),
  lignes_affectees = integer(),
  exemples = character()
)

for (col in names(data_raw)) {
  if (is.character(data_raw[[col]])) {
    mask <- detect_problemes(data_raw[[col]])
    if (any(mask)) {
      exemples <- unique(data_raw[[col]][mask])
      exemples <- exemples[1:min(3, length(exemples))]  # montre max 3 exemples
      resultats <- rbind(resultats, data.frame(
        colonne = col,
        lignes_affectees = sum(mask),
        exemples = paste(exemples, collapse = " | ")
      ))
    }
  }
}

# Résumé
if (nrow(resultats) == 0) {
  cat("✅ Aucun caractère suspect (mojibake ou C1) détecté dans la base initiale.\n")
} else {
  cat("⚠️ Des caractères suspects ont été détectés :\n")
  print(resultats, row.names = FALSE)
}
