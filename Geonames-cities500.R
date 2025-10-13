# --- geonames_download.R ---
# Télécharge et prépare GeoNames cities500.txt (version simplifiée)

dir.create("geonames_data", showWarnings = FALSE)

URL_CITIES  <- "https://download.geonames.org/export/dump/cities500.zip"
ZIP_CITIES  <- file.path("geonames_data", "cities500.zip")
TXT_CITIES  <- file.path("geonames_data", "cities500.txt")

# 1) Télécharger si non présent
if (!file.exists(ZIP_CITIES)) {
  cat("Téléchargement de cities500.zip...\n")
  download.file(URL_CITIES, ZIP_CITIES, mode = "wb")
}

# 2) Décompresser si nécessaire
if (!file.exists(TXT_CITIES)) {
  cat("Décompression...\n")
  unzip(ZIP_CITIES, exdir = "geonames_data")
}

# 3) Lire cities500.txt
cols <- c("geonameid","name","asciiname","alternatenames","lat","long",
          "feature_class","feature_code","country_code","cc2","admin1_code","admin2_code",
          "admin3_code","admin4_code","population","elevation","dem","timezone","modification")

cities <- read.delim(TXT_CITIES, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE)
names(cities) <- cols

# 4) Garder uniquement les colonnes utiles pour uniformiser location/adm1
cities_clean <- cities[, c("geonameid","asciiname","country_code","admin1_code")]
names(cities_clean)[2] <- "city"

# Sauvegarde version nettoyée
saveRDS(cities_clean, file = "geonames_data/cities500_clean.rds")

cat("✅ cities500_clean.rds sauvegardé avec", nrow(cities_clean), "lignes.\n")
