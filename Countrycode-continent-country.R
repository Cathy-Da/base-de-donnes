source("Packages.R")

entrée  <- "nasa_disaster_correction.csv"
sortie  <- "nasa_disaster_correction.csv"
données <- read.csv(entrée, check.names = FALSE, fileEncoding = "UTF-8")

# Correction pour Micronesia mal affiché selon country code apres essaie
detecte_micronesia <- données$country == "micronesia"
données$country[detecte_micronesia] <- "Micronesia, Federated States of"

# Uniformisation des pays
pays_uniformisés <- countrycode(
  sourcevar    = données$country,
  origin       = "country.name",
  destination  = "country.name"
)
lignes_valide <- !is.na(pays_uniformisés) & nzchar(pays_uniformisés)
données$country[lignes_valide] <- pays_uniformisés[lignes_valide]

# Correction des continents manquants après essaie
correction_continent <- c("Kosovo" = "Europe")

données$continent <- countrycode(
  sourcevar    = données$country,
  origin       = "country.name",
  destination  = "continent",
  custom_match = correction_continent
)

# Vérification des pays non reconnus
country_na <- données$country[is.na(pays_uniformisés) | pays_uniformisés == ""]
if (length(country_na) > 0) {
  cat("Pays non reconnus du fichier countrycode :\n")
  print(unique(country_na))
}

# Sauvegarde
write.csv(données, sortie, row.names = FALSE, fileEncoding = "UTF-8")

na_country   <- sum(is.na(données$country) | données$country == "") # nombre de pays manquant
na_continent <- sum(is.na(données$continent) | données$continent == "" | données$continent == "Unknown") # nombre de continent manquant
continents_base_de_données <- sort(unique(na.omit(données$continent))) # liste des continents

cat("Pays manquants :", na_country, "\n")
cat("Continents manquants :", na_continent, "\n")
cat("Continents dans la base de données :", paste(continents_base_de_données, collapse = ", "), "\n")
