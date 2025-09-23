# Verification_min.R — sorties compactes: OK/STOP uniquement

data <- read.csv(
  "nasa_disaster_correction.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8"   # <-- garantit un encodage correct
)

if (ncol(data) != 9) stop(sprintf("ECHEC: %d colonnes (attendu 9).", ncol(data)))

if (!("country" %in% names(data)))   stop("ECHEC: colonne 'country' absente.")
if (!("continent" %in% names(data))) stop("ECHEC: colonne 'continent' absente.")

if (any(is.na(data$country)))   stop("ECHEC: NA dans 'country'.")
if (any(is.na(data$continent))) stop("ECHEC: NA dans 'continent'.")

continents_ok <- c("Africa","Americas","Asia","Europe","Oceania")
if (!all(na.omit(unique(data$continent)) %in% continents_ok)) {
  bad <- setdiff(na.omit(unique(data$continent)), continents_ok)
  stop(sprintf("ECHEC: 'continent' contient des valeurs inattendues: %s", paste(bad, collapse=", ")))
}

cat("OK: base cohérente — 9 colonnes, aucun NA dans 'country'/'continent', continents valides.\n")
