# --- Countrycode-continent-country.R (robuste, sans pipe) ---

source("Packages.R")

entrée  <- "nasa_disaster_correction.csv"
sortie  <- "nasa_disaster_correction.csv"

# 1) Lire et mémoriser l'ordre d'origine
données <- read.csv(entrée, check.names = FALSE, fileEncoding = "UTF-8")
names(données) <- trimws(names(données))
liste_colonnes <- names(données)
nb_colonnes <- length(liste_colonnes)

# === Fonctions utilitaires ===
trim_nbsp <- function(valeur) {
  if (is.null(valeur)) return(valeur)
  valeur <- as.character(valeur)
  valeur <- gsub("\u00A0", " ", valeur, fixed = TRUE)
  trimws(valeur)
}

# 2) Corrections Pakistan (NWFP, FATA)
pakistan_correction <- function(texte) {
  if (!is.character(texte)) return(texte)
  texte <- gsub("(?i)\\bN\\.?\\s*W\\.?\\s*F\\.?\\s*P\\.?\\b", "Khyber Pakhtunkhwa", texte, perl = TRUE)
  texte <- gsub("(?i)\\bNorth\\s*[- ]?\\s*West\\s*Frontier\\s*Province\\b", "Khyber Pakhtunkhwa", texte, perl = TRUE)
  texte <- gsub("(?i)\\bF\\.?\\s*A\\.?\\s*T\\.?\\s*A\\.?\\b", "Khyber Pakhtunkhwa", texte, perl = TRUE)
  texte <- gsub("(?i)\\bFederally\\s+Administered\\s+Tribal\\s+Areas\\b", "Khyber Pakhtunkhwa", texte, perl = TRUE)
  trimws(texte)
}

if ("adm1" %in% names(données))     données$adm1     <- pakistan_correction(données$adm1)
if ("location" %in% names(données)) données$location <- pakistan_correction(données$location)

# 3) Harmoniser 'country'
if (!("country" %in% names(données))) stop("ECHEC : colonne 'country' absente.")
pays_nettoyer <- trim_nbsp(données$country)

pays_correction <- c(
  "Türkiye"            = "Turkey",
  "Timor Leste"        = "Timor-Leste",
  "Macedonia"          = "North Macedonia",
  "Cape Verde"         = "Cabo Verde",
  "Cote D'Ivoire"      = "Cote d'Ivoire",
  "Côte d'Ivoire"      = "Cote d'Ivoire",
  "Micronesia"         = "Micronesia, Federated States of",
  "Reunion"            = "Réunion",
  "Saint Helena"       = "Saint Helena, Ascension and Tristan da Cunha",
  "Congo (Kinshasa)"   = "Democratic Republic of the Congo",
  "Congo (Brazzaville)"= "Republic of the Congo",
  "Russian Federation" = "Russia",
  "Lao PDR"            = "Laos",
  "Viet Nam"           = "Vietnam",
  "Korea, South"       = "South Korea",
  "Korea, North"       = "North Korea",
  "Syrian Arab Republic" = "Syria",
  "Bolivia (Plurinational State of)" = "Bolivia",
  "Iran (Islamic Republic of)"       = "Iran",
  "United Republic of Tanzania"      = "Tanzania",
  "Czechia"                          = "Czech Republic"
)

pays_corrigés <- countrycode(
  sourcevar    = pays_nettoyer,
  origin       = "country.name",
  destination  = "country.name",
  custom_match = pays_correction,
  warn         = FALSE
)

mask_ok <- !is.na(pays_corrigés) & nzchar(pays_corrigés)
données$country[mask_ok] <- pays_corrigés[mask_ok]

# 4) Recalculer 'continent' si manquant
if (!("continent" %in% names(données))) données$continent <- NA_character_
données$continent <- trim_nbsp(données$continent)
continent_manquant <- is.na(données$continent) | données$continent == "" | données$continent == "Unknown"

if (any(continent_manquant)) {
  custom_continent_match <- c(
    "Turkey" = "Asia",
    "North Macedonia" = "Europe",
    "Timor-Leste" = "Asia",
    "Cabo Verde" = "Africa",
    "Kosovo" = "Europe",
    "Cote d'Ivoire" = "Africa",
    "Micronesia, Federated States of" = "Oceania",
    "Réunion" = "Africa",
    "Saint Helena, Ascension and Tristan da Cunha" = "Africa"
  )
  continent_corrigés <- countrycode(
    sourcevar    = données$country,
    origin       = "country.name",
    destination  = "continent",
    custom_match = custom_continent_match,
    warn         = FALSE
  )
  données$continent[continent_manquant] <- continent_corrigés[continent_manquant]
}

# Canonicaliser la casse
continents_valides <- c("Africa","Asia","Europe","North America","South America","Oceania")
correction_continent <- c(
  "africa"="Africa","asia"="Asia","europe"="Europe",
  "north america"="North America","south america"="South America","oceania"="Oceania"
)
minuscule_continent  <- tolower(trim_nbsp(données$continent))
correction <- !is.na(minuscule_continent) & nzchar(minuscule_continent) & (minuscule_continent %in% names(correction_continent))
données$continent[correction] <- unname(correction_continent[minuscule_continent[correction]])

# Réparer valeurs hors liste
données_non_valides <- is.na(données$continent) | données$continent == "" | !(données$continent %in% continents_valides)
if (any(données_non_valides)) {
  continent_arrangé <- countrycode(
    sourcevar    = données$country,
    origin       = "country.name",
    destination  = "continent",
    custom_match = custom_continent_match,
    warn         = FALSE
  )
  données$continent[données_non_valides] <- continent_arrangé[données_non_valides]
}

# Réordonnancement
nom <- names(données)
colonnes_manquantes  <- setdiff(liste_colonnes, nom)
nouvelles_colonnes    <- setdiff(nom, liste_colonnes)

if (length(colonnes_manquantes) > 0) {
  for (mc in colonnes_manquantes) données[[mc]] <- NA
}
index_colonnes <- match(liste_colonnes, names(données))
index_colonnes <- index_colonnes[!is.na(index_colonnes)]
données <- données[, index_colonnes, drop = FALSE]

if (ncol(données) > nb_colonnes) données <- données[, seq_len(nb_colonnes), drop = FALSE]

if (ncol(données) != nb_colonnes) {
  stop(sprintf("ECHEC: le fichier doit rester à %d colonnes (actuel: %d).", nb_colonnes, ncol(données)))
}
if (!identical(names(données), liste_colonnes)) {
  warning("Noms/ordre légèrement ajustés pour coller au schéma d'origine.")
}

# Sauvegarde
write.csv(données, sortie, row.names = FALSE, fileEncoding = "UTF-8")

# Bilan
na_pays   <- sum(is.na(données$country) | données$country == "")
continent_manquant    <- sum(is.na(données$continent) | données$continent == "" | données$continent == "Unknown")
données_non_valides_valeurs     <- setdiff(unique(données$continent), continents_valides)

cat("OK : pays harmonisés, correction PK (NWFP/FATA), continents corrigés si manquants + casse canonique, réordonnancement stable.\n")
cat(sprintf("- Country NA/vides : %d\n", na_pays))
cat(sprintf("- Continent NA/vides/Unknown : %d\n", continent_manquant))
if (length(données_non_valides_valeurs)) {
  cat("⚠️ Valeurs de 'continent' hors liste détectées : ", paste(données_non_valides_valeurs, collapse=", "), "\n", sep="")
}
if (length(colonnes_manquantes) > 0) {
  cat("ℹ️ Colonnes recréées (NA) : ", paste(colonnes_manquantes, collapse=", "), "\n", sep="")
}
if (length(nouvelles_colonnes) > 0) {
  cat("ℹ️ Colonnes en trop supprimées : ", paste(nouvelles_colonnes, collapse=", "), "\n", sep="")
}
