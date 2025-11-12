source("Packages.R")

# Lire en UTF-8 pour éviter les erreurs
base_de_données <- read.csv(
  "nasa_disaster_dataset.csv",
  fileEncoding = "UTF-8",
  check.names = FALSE
)

base_de_données_corr <- base_de_données

# Nettoyage des mojibake
nettoyer_mojibake <- function(valeur) {
  if (!is.character(valeur)) return(valeur)
  mojibake <- valeur
  
  # Supprimer les espaces insécables et caractères corrompus
  mojibake <- gsub("\u00A0", " ", mojibake, fixed = TRUE)  # enlève les espaces insécables
  mojibake <- gsub("Ã", "", mojibake, fixed = TRUE)
  mojibake <- gsub("Â", "", mojibake, fixed = TRUE)
  mojibake <- gsub("ƒ", "", mojibake, fixed = TRUE)
  mojibake <- gsub("[\uFFFD]", "", mojibake, perl = TRUE)
  mojibake <- gsub("[\u0080-\u009F]", "", mojibake, perl = TRUE)
  trimws(mojibake)
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

for (col in names(base_de_données_corr)) {
  if (is.character(base_de_données_corr[[col]])) {
    base_de_données_corr[[col]] <- nettoyer_mojibake(base_de_données_corr[[col]])
    base_de_données_corr[[col]] <- uniformiser_texte(base_de_données_corr[[col]])
  }
}

# Sauvegarde
fichier <- file("nasa_disaster_correction.csv", open = "w", encoding = "UTF-8")
write.csv(base_de_données_corr, fichier, row.names = FALSE)
close(fichier)

cat("Fichier : nasa_disaster_correction.csv\n")
