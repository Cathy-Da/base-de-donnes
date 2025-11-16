source("Packages.R")

base_de_données <- read.csv(
  "nasa_disaster_correction.csv",
  check.names = FALSE,
  fileEncoding = "UTF-8"
)

# Nombre de colonnes
if (ncol(base_de_données) != 8) {
  stop(sprintf("ECHEC: %d colonnes.", ncol(base_de_données)))
}

# 2) Colonnes essentielles
if (!is.element("country", names(base_de_données)))   stop("ECHEC: colonne 'country' absente.")
if (!is.element("continent", names(base_de_données))) stop("ECHEC: colonne 'continent' absente.")

# 3) Interdiction de NA/vides
if (any(is.na(base_de_données$country)   | base_de_données$country   == "")) stop("ECHEC: NA/vide dans 'country'.")
if (any(is.na(base_de_données$continent) | base_de_données$continent == "")) stop("ECHEC: NA/vide dans 'continent'.")

# 4) Liste officielle des continents selon countrycode
continents_ok <- sort(unique(na.omit(countrycode::codelist$continent)))
# par sécurité, s'assurer que North/South America sont bien inclus
continents_ok <- union(continents_ok, c("North America","South America"))

# 5) Refuser explicitement 'Unknown'
if (any(base_de_données$continent == "Unknown")) {
  stop("ECHEC: 'continent' contient 'Unknown'.")
}

# 6) Vérifier que toutes les valeurs appartiennent à la liste countrycode
valeurs_invalides <- setdiff(unique(base_de_données$continent), continents_ok)
if (length(valeurs_invalides)) {
  stop(sprintf("ECHEC: 'continent' contient des valeurs inattendues: %s",
               paste(valeurs_invalides, collapse=", ")))
}

# 7) Vérification informative : nombre de "no-data" (aucun STOP)
nb_no_data <- sum(tolower(as.matrix(base_de_données)) == "no-data", na.rm = TRUE)
cat("Info : nombre de champs 'no-data' détectés :", nb_no_data, "\n")

cat("OK: 8 colonnes, aucun NA/vide, continents valides selon countrycode (North/South America acceptés).\n")
