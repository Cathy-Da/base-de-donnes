library(stringi)

# base initiale (forcer UTF-8)
data_raw <- read.csv("nasa_disaster_dataset.csv",
                     stringsAsFactors = FALSE,
                     fileEncoding = "UTF-8")

# copie de travail
data_corr <- data_raw

# normalisation texte
normalize_text <- function(x) {
  if (!is.character(x)) return(x)
  x <- trimws(x)                         # enlever espaces début/fin
  x <- gsub("\\s+", " ", x)              # compresser espaces multiples
  x <- tolower(x)                        # tout en minuscules
  # garder accents ? -> commenter la ligne suivante si tu veux conserver les vrais noms
  x <- stri_trans_general(x, "Latin-ASCII")  
  x[x %in% c("", "na", "n/a", "unknown", "none", "null")] <- NA
  return(x)
}

# appliquer la normalisation
for (col in names(data_corr)) {
  if (is.character(data_corr[[col]])) {
    data_corr[[col]] <- normalize_text(data_corr[[col]])
  }
}

# sauvegarde en UTF-8
con <- file("nasa_disaster_correction.csv", open = "w", encoding = "UTF-8")
write.csv(data_corr, con, row.names = FALSE)
close(con)

# résumé avant/après
cat("Base initiale :", nrow(data_raw), "lignes et", ncol(data_raw), "colonnes\n")
cat("Base corrigée :", nrow(data_corr), "lignes et", ncol(data_corr), "colonnes\n")
cat("Fichier écrit : nasa_disaster_correction.csv\n")
