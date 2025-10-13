# Verification_min.R — validations basées sur countrycode (7 continents)
library(countrycode)

data <- read.csv(
  "nasa_disaster_correction.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8"
)

# 1) Nombre de colonnes attendu (8)
if (ncol(data) != 8) {
  stop(sprintf("ECHEC: %d colonnes (attendu 8).", ncol(data)))
}

# 2) Colonnes essentielles
if (!("country" %in% names(data)))   stop("ECHEC: colonne 'country' absente.")
if (!("continent" %in% names(data))) stop("ECHEC: colonne 'continent' absente.")

# 3) Interdiction de NA/vides
if (any(is.na(data$country)   | data$country   == "")) stop("ECHEC: NA/vide dans 'country'.")
if (any(is.na(data$continent) | data$continent == "")) stop("ECHEC: NA/vide dans 'continent'.")

# 4) Liste officielle des continents selon countrycode
continents_ok <- sort(unique(na.omit(countrycode::codelist$continent)))
# par sécurité, s'assurer que North/South America sont bien inclus
continents_ok <- union(continents_ok, c("North America","South America"))

# 5) Refuser explicitement 'Unknown'
if (any(data$continent == "Unknown")) {
  stop("ECHEC: 'continent' contient 'Unknown'.")
}

# 6) Vérifier que toutes les valeurs appartiennent à la liste countrycode
bad <- setdiff(unique(data$continent), continents_ok)
if (length(bad)) {
  stop(sprintf("ECHEC: 'continent' contient des valeurs inattendues: %s",
               paste(bad, collapse=", ")))
}

cat("✅ OK: 8 colonnes, aucun NA/vide, continents valides selon countrycode (North/South America acceptés).\n")
