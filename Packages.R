# Packages
packages <-c(
  "maps",        # cartes
  "mapdata",     # cartes informations
  "stringi",      # chaine de caracteres
  "countrycode",   #
  "tidygeocoder",  #
  "ggplot2",       #
  "dplyr",          #
  "readr"          #
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