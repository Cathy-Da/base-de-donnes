# --- Countrycode-continent-country.R (version recalcul complet) ---
# Recalcule entièrement la colonne 'continent' à partir de 'country'
# Corrige Kosovo et Micronesia, et harmonise les noms via countrycode

source("Packages.R")

INFILE  <- "nasa_disaster_correction.csv"
OUTFILE <- "nasa_disaster_correction.csv"

# --- Lecture du fichier ---
x <- read.csv(INFILE, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
names(x) <- trimws(names(x))

# --- Fonction pour nettoyer les espaces spéciaux ---
trim_nbsp <- function(s) {
  if (is.null(s)) return(s)
  s <- as.character(s)
  s <- gsub("\u00A0", " ", s, fixed = TRUE)  # enlève les espaces insécables
  trimws(s)
}

# --- Harmoniser la colonne 'country' ---
if (!("country" %in% names(x))) stop("Erreur : colonne 'country' absente du fichier.")
x$country <- trim_nbsp(x$country)

# Correction robuste pour Micronesia (ignore casse et espaces)
mask_micro <- grepl("^\\s*micronesia\\s*$", x$country, ignore.case = TRUE)
x$country[mask_micro] <- "Micronesia, Federated States of"

# Harmoniser les pays avec countrycode
mapped_country <- countrycode(
  sourcevar    = x$country,
  origin       = "country.name",
  destination  = "country.name",
  warn         = FALSE
)
mask_ok <- !is.na(mapped_country) & nzchar(mapped_country)
x$country[mask_ok] <- mapped_country[mask_ok]

# --- Recalcul complet de la colonne 'continent' ---
custom_continent_match <- c(
  "Kosovo" = "Europe"
)
x$continent <- countrycode(
  sourcevar    = x$country,
  origin       = "country.name",
  destination  = "continent",
  custom_match = custom_continent_match,
  warn         = FALSE
)

# --- Vérification des pays non reconnus ---
country_na <- x$country[is.na(mapped_country) | mapped_country == ""]
if (length(country_na) > 0) {
  cat("⚠️  Certains pays n'ont pas été reconnus par countrycode :\n")
  print(unique(country_na))
  cat("\n")
}

# --- Sauvegarde du fichier corrigé ---
write.csv(x, OUTFILE, row.names = FALSE, fileEncoding = "UTF-8")

# --- Bilan final ---
na_country   <- sum(is.na(x$country) | x$country == "")
na_continent <- sum(is.na(x$continent) | x$continent == "" | x$continent == "Unknown")
continents_detectes <- sort(unique(na.omit(x$continent)))

cat("OK : pays harmonisés et continents recalculés à 100% par countrycode.\n")
cat("Pays manquants :", na_country, "\n")
cat("Continents manquants :", na_continent, "\n")
cat("Continents détectés :", paste(continents_detectes, collapse = ", "), "\n")
