# --- Countrycode-continent-country.R (version recalcul complet) ---
# Recalcule entièrement la colonne 'continent' à partir de 'country'
# Corrige Kosovo et Micronesia, et harmonise les noms via countrycode

source("Packages.R")

entrée  <- "nasa_disaster_correction.csv"
sortie  <- "nasa_disaster_correction.csv"

# --- Lecture du fichier ---
données <- read.csv(entrée, check.names = FALSE, fileEncoding = "UTF-8")
names(données) <- trimws(names(données))

# --- Fonction pour nettoyer les espaces spéciaux ---
trim_nbsp <- function(valeur) {
  if (is.null(valeur)) return(valeur)
  valeur <- as.character(valeur)
  valeur <- gsub("\u00A0", " ", valeur, fixed = TRUE)  # enlève les espaces insécables
  trimws(valeur)
}

# --- Harmoniser la colonne 'country' ---
if (!is.element("country", names(données))) stop("Erreur : colonne 'country' absente du fichier.")
données$country <- trim_nbsp(données$country)

# Correction robuste pour Micronesia (ignore casse et espaces)
detecte_micronesia <- grepl("^\\s*micronesia\\s*$", données$country, ignore.case = TRUE)
données$country[detecte_micronesia] <- "Micronesia, Federated States of"

# Harmoniser les pays avec countrycode
pays_harmonisés <- countrycode(
  sourcevar    = données$country,
  origin       = "country.name",
  destination  = "country.name",
  warn         = FALSE
)
lignes_valide <- !is.na(pays_harmonisés) & nzchar(pays_harmonisés)
données$country[lignes_valide] <- pays_harmonisés[lignes_valide]

# --- Recalcul complet de la colonne 'continent' ---
correction_continent <- c(
  "Kosovo" = "Europe"
)
données$continent <- countrycode(
  sourcevar    = données$country,
  origin       = "country.name",
  destination  = "continent",
  custom_match = correction_continent,
  warn         = FALSE
)

# --- Vérification des pays non reconnus ---
country_na <- données$country[is.na(pays_harmonisés) | pays_harmonisés == ""]
if (length(country_na) > 0) {
  cat("⚠️  Certains pays n'ont pas été reconnus par countrycode :\n")
  print(unique(country_na))
  cat("\n")
}

# --- Sauvegarde du fichier corrigé ---
write.csv(données, sortie, row.names = FALSE, fileEncoding = "UTF-8")

# --- Bilan final ---
na_country   <- sum(is.na(données$country) | données$country == "")
na_continent <- sum(is.na(données$continent) | données$continent == "" | données$continent == "Unknown")
continents_detectes <- sort(unique(na.omit(données$continent)))

cat("OK : pays harmonisés et continents recalculés à 100% par countrycode.\n")
cat("Pays manquants :", na_country, "\n")
cat("Continents manquants :", na_continent, "\n")
cat("Continents détectés :", paste(continents_detectes, collapse = ", "), "\n")
