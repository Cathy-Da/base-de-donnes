# Packages
packages <-c(
  "httr",        # API Wikipédia
  "jsonlite",    # Pour traiter les réponses API-wikipédia
  "dplyr",       # manipuler données CSV
  "readr",       # Fichiers CSV
  "stringr",     # Encodage
  "R.utils",     # Décompression GZIP, BZIP2
  "zip",         # ZIP
  "brotli",      # Brotli
  "tools",       # Gestion des fichiers
  "base64enc",   # Base64
  "ggplot2",     # diagramme ggplot
  "DT",          # tableau interactif
  "htmlwidgets", # fichier html interactif
  "rvest",       # suppression balise HTML, brut lisible
  "xml2"         # convertit HTML
)

# Installation et chargement des packages
installation_chargement <-function(packages) {
  for (nom in packages) {
    if (!require(nom, character.only = TRUE)) {
      install.packages(nom, dependencies = TRUE)
      library(nom, character.only = TRUE)
    }
  }
}

installation_chargement(packages)
print("Terminé")