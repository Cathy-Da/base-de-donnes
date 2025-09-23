# Lire avec encodage explicite
data <- read.csv(
  "nasa_disaster_correction.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8"
)

problems <- FALSE

# Vérif NA
if (any(is.na(data$id))) {
  cat("⚠️ Attention : des NA dans 'id'\n")
  problems <- TRUE
}

# Vérif doublons
if (anyDuplicated(data$id) > 0) {
  cat("⚠️ Attention : 'id' contient des doublons\n")
  problems <- TRUE
}

# Si tout est OK
if (!problems) {
  cat("✅ OK : 'id' est unique et sans NA\n")
}
