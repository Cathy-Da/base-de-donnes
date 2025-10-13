# --- Countrycode-continent-country.R (robuste, SANS pipe) ---

suppressWarnings(suppressMessages(library(countrycode)))

INFILE  <- "nasa_disaster_correction.csv"
OUTFILE <- "nasa_disaster_correction.csv"

# 1) Lire et mémoriser l'ordre d'origine
x <- read.csv(INFILE, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
names(x) <- trimws(names(x))
orig_cols <- names(x)             # ordre et intitulés d'ORIGINE (référence)
orig_ncol <- length(orig_cols)

# === Fonctions utilitaires ===
trim_nbsp <- function(s) {
  if (is.null(s)) return(s)
  s <- as.character(s)
  s <- gsub("\u00A0", " ", s, fixed = TRUE)  # enlève NBSP
  trimws(s)
}

# 2) Patches légers (mais robustes) — Pakistan
# couvre: N.W.F.P., NWFP, North West Frontier Province, F.A.T.A., FATA, Federally Administered Tribal Areas
fix_pk_strict <- function(v){
  if (!is.character(v)) return(v)
  v <- gsub("(?i)\\bN\\.?\\s*W\\.?\\s*F\\.?\\s*P\\.?\\b", "Khyber Pakhtunkhwa", v, perl = TRUE)
  v <- gsub("(?i)\\bNorth\\s*[- ]?\\s*West\\s*Frontier\\s*Province\\b", "Khyber Pakhtunkhwa", v, perl = TRUE)
  v <- gsub("(?i)\\bF\\.?\\s*A\\.?\\s*T\\.?\\s*A\\.?\\b", "Khyber Pakhtunkhwa", v, perl = TRUE)
  v <- gsub("(?i)\\bFederally\\s+Administered\\s+Tribal\\s+Areas\\b", "Khyber Pakhtunkhwa", v, perl = TRUE)
  trimws(v)
}
if ("adm1" %in% names(x))     x$adm1     <- fix_pk_strict(x$adm1)
if ("location" %in% names(x)) x$location <- fix_pk_strict(x$location)

# 3) Harmoniser country (sans tout écraser)
if (!("country" %in% names(x))) stop("ECHEC : colonne 'country' absente.")
raw <- trim_nbsp(x$country)

custom_country_match <- c(
  "Türkiye"            = "Turkey",
  "Timor Leste"        = "Timor-Leste",
  "Macedonia"          = "North Macedonia",
  "Cape Verde"         = "Cabo Verde",
  "Cote D'Ivoire"      = "Cote d'Ivoire",
  "Cote d’Ivoire"      = "Cote d'Ivoire",
  "Côte d’Ivoire"      = "Cote d'Ivoire",
  "Côte d'Ivoire"      = "Cote d'Ivoire",
  "Micronesia"         = "Micronesia, Federated States of",
  "Reunion"            = "Réunion",
  "Saint Helena"       = "Saint Helena, Ascension and Tristan da Cunha",
  # autres alias utiles
  "Congo (Kinshasa)"     = "Democratic Republic of the Congo",
  "Congo (Brazzaville)"  = "Republic of the Congo",
  "Russian Federation"   = "Russia",
  "Lao PDR"              = "Laos",
  "Viet Nam"             = "Vietnam",
  "Korea, South"         = "South Korea",
  "Korea, North"         = "North Korea",
  "Syrian Arab Republic" = "Syria",
  "Bolivia (Plurinational State of)" = "Bolivia",
  "Iran (Islamic Republic of)"       = "Iran",
  "United Republic of Tanzania"      = "Tanzania",
  "Czechia"                          = "Czech Republic"
)

mapped_country <- countrycode(
  sourcevar    = raw,
  origin       = "country.name",
  destination  = "country.name",
  custom_match = custom_country_match,
  warn         = FALSE
)

mask_ok <- !is.na(mapped_country) & nzchar(mapped_country)
x$country[mask_ok] <- mapped_country[mask_ok]

# 4) (Re)calculer continent UNIQUEMENT si NA / "" / "Unknown"
if (!("continent" %in% names(x))) x$continent <- NA_character_
x$continent <- trim_nbsp(x$continent)
mask_fix <- is.na(x$continent) | x$continent == "" | x$continent == "Unknown"

if (any(mask_fix)) {
  custom_continent_match <- c(
    "Turkey"                                   = "Asia",
    "North Macedonia"                          = "Europe",
    "Timor-Leste"                              = "Asia",
    "Cabo Verde"                               = "Africa",
    "Kosovo"                                   = "Europe",
    "Cote d'Ivoire"                            = "Africa",
    "Micronesia, Federated States of"          = "Oceania",
    "Réunion"                                  = "Africa",
    "Saint Helena, Ascension and Tristan da Cunha" = "Africa",
    "micronesia"                               = "Oceania"
  )
  
  new_cont <- countrycode(
    sourcevar    = x$country,
    origin       = "country.name",
    destination  = "continent",
    custom_match = custom_continent_match,
    warn         = FALSE
  )
  x$continent[mask_fix] <- new_cont[mask_fix]
}

# 4bis) --- Canonicaliser la casse des continents (TOUJOURS) ---
ok_vals <- c("Africa","Asia","Europe","North America","South America","Oceania")

canon_map <- c(
  "africa"        = "Africa",
  "asia"          = "Asia",
  "europe"        = "Europe",
  "north america" = "North America",
  "south america" = "South America",
  "oceania"       = "Oceania"
)

lk  <- tolower(trim_nbsp(x$continent))
hit <- !is.na(lk) & nzchar(lk) & (lk %in% names(canon_map))
x$continent[hit] <- unname(canon_map[lk[hit]])

# 4ter) --- Réparer toute valeur hors-liste en recalculant depuis 'country' ---
bad <- is.na(x$continent) | x$continent == "" | !(x$continent %in% ok_vals)
if (any(bad)) {
  # réutilise le même mapping custom que plus haut
  custom_continent_match <- c(
    "Turkey"                                   = "Asia",
    "North Macedonia"                          = "Europe",
    "Timor-Leste"                              = "Asia",
    "Cabo Verde"                               = "Africa",
    "Kosovo"                                   = "Europe",
    "Cote d'Ivoire"                            = "Africa",
    "Micronesia, Federated States of"          = "Oceania",
    "Réunion"                                  = "Africa",
    "Saint Helena, Ascension and Tristan da Cunha" = "Africa"
  )
  fix <- countrycode(
    sourcevar    = x$country,
    origin       = "country.name",
    destination  = "continent",
    custom_match = custom_continent_match,
    warn         = FALSE
  )
  x$continent[bad] <- fix[bad]
}

# 5) RÉORDONNANCEMENT INFAILLIBLE (jamais d'erreur)
current_names <- names(x)
missing_cols  <- setdiff(orig_cols, current_names)
extra_cols    <- setdiff(current_names, orig_cols)

if (length(missing_cols) > 0) {
  for (mc in missing_cols) x[[mc]] <- NA
}
# réordonner par indices (match) puis enlever l'excédent
idx <- match(orig_cols, names(x))      # positions dans l'objet courant
idx <- idx[!is.na(idx)]
x <- x[, idx, drop = FALSE]

# sécurité : si malgré tout il y a > 9 colonnes, on coupe à 9 dans l'ordre d'origine
if (ncol(x) > orig_ncol) x <- x[, seq_len(orig_ncol), drop = FALSE]

# 6) Gardes-fous finaux
if (ncol(x) != orig_ncol) {
  stop(sprintf("ECHEC: le fichier doit rester à %d colonnes (actuel: %d).", orig_ncol, ncol(x)))
}
if (!identical(names(x), orig_cols)) {
  warning("Noms/ordre légèrement ajustés pour coller au schéma d'origine.")
}

# 7) Écriture
write.csv(x, OUTFILE, row.names = FALSE, fileEncoding = "UTF-8")

# 8) Bilan explicite
na_country   <- sum(is.na(x$country)   | x$country   == "")
miss_cont    <- sum(is.na(x$continent) | x$continent == "" | x$continent == "Unknown")
bad_vals     <- setdiff(unique(x$continent), ok_vals)

cat("OK: pays harmonisés, correction PK (NWFP/FATA), continents corrigés si manquants + casse canonique, réordonnancement stable.\n")
cat(sprintf("- Country NA/vides : %d\n", na_country))
cat(sprintf("- Continent NA/vides/Unknown : %d\n", miss_cont))
if (length(bad_vals)) {
  cat("⚠️ Valeurs de 'continent' hors liste détectées (après normalisation): ",
      paste(bad_vals, collapse = ", "), "\n", sep = "")
}
if (length(missing_cols) > 0) {
  cat("ℹ️ Colonnes recréées (NA) pour préserver le schéma : ",
      paste(missing_cols, collapse = ", "), "\n", sep = "")
}
if (length(extra_cols) > 0) {
  cat("ℹ️ Colonnes en trop supprimées : ",
      paste(extra_cols, collapse = ", "), "\n", sep = "")
}
