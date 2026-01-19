# Packages
packages <-c(
"maps",        # cartes
"mapdata",     # cartes informations
"stringi",     # chaine de caracteres
"countrycode", # code de pays- noms- continents
"tidygeocoder",# ArcGIS, latitude, longitude
"ggplot2",     # carte
"dplyr",       # tableau de données
"readr",       # lire fichier
"ggspatial"    # carte mondiale
)

# Installation et chargement des packages
installation_chargement <-function(packages) 
{
for (nom in packages) 
{
if (!require(nom, character.only= TRUE)) 
{
install.packages(nom, dependencies= TRUE)
library(nom, character.only= TRUE)
}
}
}

installation_chargement(packages)
print("Terminé")