library(stringi)

# Lire en UTF-8
data_raw <- read.csv(
  "nasa_disaster_dataset.csv",
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8",
  check.names = FALSE
)

data_corr <- data_raw

# 1) Nettoyage des mojibake
clean_mojibake <- function(x) {
  if (!is.character(x)) return(x)
  y <- x
  # symboles typiques du mojibake déjà écrits dans le texte
  y <- gsub("Ã", "", y, fixed = TRUE)
  y <- gsub("Â", "", y, fixed = TRUE)
  y <- gsub("ƒ", "", y, fixed = TRUE)     # <-- ajoute ƒ
  y <- gsub("[\uFFFD]", "", y, perl = TRUE) # caractère de remplacement
  # (optionnel) retirer les contrôles C1 s'il en reste
  y <- gsub("[\u0080-\u009F]", "", y, perl = TRUE)
  trimws(y)
}

# 2) Normalisation du texte (tu enlèves les accents ici)
normalize_text <- function(x) {
  if (!is.character(x)) return(x)
  x <- trimws(x)
  x <- gsub("\\s+", " ", x)
  x <- tolower(x)
  # si tu veux conserver les accents, commente la ligne suivante
  x <- stri_trans_general(x, "Latin-ASCII")
  x[x %in% c("", "na", "n/a", "unknown", "none", "null")] <- NA
  x
}

# Appliquer nettoyage + normalisation aux colonnes texte
for (col in names(data_corr)) {
  if (is.character(data_corr[[col]])) {
    data_corr[[col]] <- clean_mojibake(data_corr[[col]])
    data_corr[[col]] <- normalize_text(data_corr[[col]])
  }
}

# Sauvegarder en UTF-8
con <- file("nasa_disaster_correction.csv", open = "w", encoding = "UTF-8")
write.csv(data_corr, con, row.names = FALSE)
close(con)

cat("Nettoyage terminé. Fichier écrit : nasa_disaster_correction.csv\n")
