# Lire avec encodage explicite
base_de_données <- read.csv(
  "nasa_disaster_correction.csv",
  check.names = FALSE,
  fileEncoding = "UTF-8"
)

problems <- FALSE

# Vérif NA
if (any(is.na(base_de_données$id))) {
  cat("NA dans id\n")
  problems <- TRUE
}

# Vérif doublons
if (anyDuplicated(base_de_données$id) > 0) {
  cat("Doublon id\n")
  problems <- TRUE
}

# Si tout est OK
if (!problems) {
  cat("✅ OK : 'id sans doublon et na \n")
}
