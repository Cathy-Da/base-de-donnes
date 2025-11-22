source("Packages.R")

# Lire en UTF-8 pour éviter les erreurs
base_de_données <- read.csv("nasa_disaster_dataset.csv", fileEncoding = "UTF-8", check.names = FALSE)

# Nettoyage des caractères illisibles
nettoyer_caractères_illisibles <- function(valeur) {
  if (!is.character(valeur)) return(valeur)
  caractères_illisibles <- valeur
  caractères_illisibles <- gsub("\u00A0", " ", caractères_illisibles, fixed = TRUE)
  caractères_illisibles <- gsub("Ã", "", caractères_illisibles, fixed = TRUE)
  caractères_illisibles <- gsub("Â", "", caractères_illisibles, fixed = TRUE)
  caractères_illisibles <- gsub("ƒ", "", caractères_illisibles, fixed = TRUE)
  caractères_illisibles <- gsub("[\uFFFD]", "", caractères_illisibles, perl = TRUE)
  caractères_illisibles <- gsub("[\u0080-\u009F]", "", caractères_illisibles, perl = TRUE)
  trimws(caractères_illisibles)
}

# Uniformisation du texte
uniformiser_texte <- function(texte) {
  if (!is.character(texte)) return(texte)
  texte <- trimws(texte)
  texte <- gsub("\\s+", " ", texte)
  texte <- tolower(texte)
  texte <- stri_trans_general(texte, "Latin-ASCII")
  texte[is.element(texte, c("", "na", "n/a", "unknown", "none", "null"))] <- NA
  texte
}

# Appliquer les deux nettoyages
for (col in names(base_de_données)) {
  if (is.character(base_de_données[[col]])) {
    base_de_données[[col]] <- nettoyer_caractères_illisibles(base_de_données[[col]])
    base_de_données[[col]] <- uniformiser_texte(base_de_données[[col]])
  }
}

# Sauvegarde
write.csv(base_de_données, "nasa_disaster_correction.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("Fichier : nasa_disaster_correction.csv\n")
