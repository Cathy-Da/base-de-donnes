source("Packages.R")

# Lire en UTF-8
base_de_données <- read.csv(
  "nasa_disaster_dataset.csv",
  fileEncoding = "UTF-8",
  check.names = FALSE
)

base_de_données_corr <- base_de_données

# 1) Nettoyage des mojibake
nettoyer_mojibake <- function(x) {
  if (!is.character(x)) return(x)
  mojibake <- x
  # symboles typiques du mojibake déjà écrits dans le texte
  mojibake <- gsub("Ã", "", mojibake, fixed = TRUE)
  mojibake <- gsub("Â", "", mojibake, fixed = TRUE)
  mojibake <- gsub("ƒ", "", mojibake, fixed = TRUE)
  mojibake <- gsub("[\uFFFD]", "", mojibake, perl = TRUE)
  mojibake <- gsub("[\u0080-\u009F]", "", mojibake, perl = TRUE)
  trimws(mojibake)
}

# 2) Normalisation du texte (tu enlèves les accents ici)
uniformiser_texte <- function(texte) {
  if (!is.character(texte)) return(texte)
  texte <- trimws(texte)
  texte <- gsub("\\s+", " ", texte)
  texte <- tolower(texte)
  texte <- stri_trans_general(texte, "Latin-ASCII")
  # Remplace %in% par is.element()
  texte[is.element(texte, c("", "na", "n/a", "unknown", "none", "null"))] <- NA
  texte
}

# Appliquer nettoyage + normalisation aux colonnes texte
for (col in names(base_de_données_corr)) {
  if (is.character(base_de_données_corr[[col]])) {
    base_de_données_corr[[col]] <- nettoyer_mojibake(base_de_données_corr[[col]])
    base_de_données_corr[[col]] <- uniformiser_texte(base_de_données_corr[[col]])
  }
}

# Sauvegarder en UTF-8
fichier <- file("nasa_disaster_correction.csv", open = "w", encoding = "UTF-8")
write.csv(base_de_données_corr, fichier, row.names = FALSE)
close(fichier)

cat("Nettoyage terminé. Fichier écrit : nasa_disaster_correction.csv\n")
