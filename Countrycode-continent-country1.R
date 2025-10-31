# --- Countrycode-continent-country.R (robuste, sans pipe) ---

source("Packages.R")

entrée  <- "nasa_disaster_correction.csv"
sortie  <- "nasa_disaster_correction.csv"

# 1) Lire et mémoriser l'ordre d'origine
données <- read.csv(entrée, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
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
fix_pk_strict <- function(texte) {
  if (!is.character(texte)) return(texte)
  texte <- gsub("(?i)\\bN\\.?\\s*W\\.?\\s*F\\.?\\s*P\\.?\\b", "Khyber Pakhtunkhwa", texte, perl = TRUE)
  texte <- gsub("(?i)\\bNorth\\s*[- ]?\\s*West\\s*Frontier\\s*Province\\b", "Khyber Pakhtunkhwa", texte, perl = TRUE)
  texte <- gsub("(?i)\\bF\\.?\\s*A\\.?\\s*T\\.?\\s*A\\.?\\b", "Khyber Pakhtunkhwa", texte, perl = TRUE)
  texte <- gsub("(?i)\\bFederally\\s+Administered\\s+Tribal\\s+Areas\\b", "Khyber Pakhtunkhwa", texte, perl = TRUE)
  trimws(texte)
}

if ("adm1" %in% names(données))     données$adm1     <- fix_pk_strict(données$adm1)
if ("location" %in% names(données)) données$location <- fix_pk_strict(données$location)

# 3) Harmoniser 'country'
if (!("country" %in% names(données))) stop("ECHEC : colonne 'country' absente.")
pays_nettoyer <- trim_nbsp(données$country)

custom_country_match <- c(
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

mapped_country <- countrycode(
  sourcevar    = pays_nettoyer,
  origin       = "country.name",
  destination  = "country.name",
  custom_match = custom_country_match,
  warn         = FALSE
)

mask_ok <- !is.na(mapped_country) & nzchar(mapped_country)
données$country[mask_ok] <- mapped_country[mask_ok]

# 4) Recalculer 'continent' si manquant
if (!("continent" %in% names(données))) données$continent <- NA_character_
données$continent <- trim_nbsp(données$continent)
mask_fix <- is.na(données$continent) | données$continent == "" | données$continent == "Unknown"

if (any(mask_fix)) {
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
  new_cont <- countrycode(
    sourcevar    = données$country,
    origin       = "country.name",
    destination  = "continent",
    custom_match = custom_continent_match,
    warn         = FALSE
  )
  données$continent[mask_fix] <- new_cont[mask_fix]
}

# Canonicaliser la casse
ok_vals <- c("Africa","Asia","Europe","North America","South America","Oceania")
canon_map <- c(
  "africa"="Africa","asia"="Asia","europe"="Europe",
  "north america"="North America","south america"="South America","oceania"="Oceania"
)
lk  <- tolower(trim_nbsp(données$continent))
hit <- !is.na(lk) & nzchar(lk) & (lk %in% names(canon_map))
données$continent[hit] <- unname(canon_map[lk[hit]])

# Réparer valeurs hors liste
bad <- is.na(données$continent) | données$continent == "" | !(données$continent %in% ok_vals)
if (any(bad)) {
  fix <- countrycode(
    sourcevar    = données$country,
    origin       = "country.name",
    destination  = "continent",
    custom_match = custom_continent_match,
    warn         = FALSE
  )
  données$continent[bad] <- fix[bad]
}

# Réordonnancement
current_names <- names(données)
missing_cols  <- setdiff(liste_colonnes, current_names)
extra_cols    <- setdiff(current_names, liste_colonnes)

if (length(missing_cols) > 0) {
  for (mc in missing_cols) données[[mc]] <- NA
}
idx <- match(liste_colonnes, names(données))
idx <- idx[!is.na(idx)]
données <- données[, idx, drop = FALSE]

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
na_country   <- sum(is.na(données$country) | données$country == "")
miss_cont    <- sum(is.na(données$continent) | données$continent == "" | données$continent == "Unknown")
bad_vals     <- setdiff(unique(données$continent), ok_vals)

cat("OK : pays harmonisés, correction PK (NWFP/FATA), continents corrigés si manquants + casse canonique, réordonnancement stable.\n")
cat(sprintf("- Country NA/vides : %d\n", na_country))
cat(sprintf("- Continent NA/vides/Unknown : %d\n", miss_cont))
if (length(bad_vals)) {
  cat("⚠️ Valeurs de 'continent' hors liste détectées : ", paste(bad_vals, collapse=", "), "\n", sep="")
}
if (length(missing_cols) > 0) {
  cat("ℹ️ Colonnes recréées (NA) : ", paste(missing_cols, collapse=", "), "\n", sep="")
}
if (length(extra_cols) > 0) {
  cat("ℹ️ Colonnes en trop supprimées : ", paste(extra_cols, collapse=", "), "\n", sep="")
}
