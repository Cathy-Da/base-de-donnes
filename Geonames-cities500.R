# GeoNames cities500.txt

dir.create("geonames_data")

URL  <- "https://download.geonames.org/export/dump/cities500.zip"
ZIP  <- file.path("geonames_data", "cities500.zip")
TXT_files  <- file.path("geonames_data", "cities500.txt")

# Télécharge et décompresse
download.file(URL, ZIP, mode = "wb")
unzip(ZIP, exdir = "geonames_data", overwrite = TRUE)

# Lire cities500.txt
colonnes <- c("geonameid","name","asciiname","alternatenames",
              "feature_class","feature_code","country_code","cc2",
              "admin1_code","admin2_code","admin3_code","admin4_code",
              "population","elevation","dem","timezone","modification")

cities <- read.delim(TXT_files, header = FALSE, sep = "\t", quote = "")
names(cities) <- colonnes

# 4) Garder uniquement les colonnes utiles pour uniformiser location/adm1
cities_uniformisation <- cities[, c("geonameid","asciiname","country_code","admin1_code")]
names(cities_uniformisation)[2] <- "city"

# 5) Sauvegarde version nettoyée (sans lat/long)
saveRDS(cities_uniformisation, file = "geonames_data/cities500_clean.rds")

cat("✅ cities500_clean.rds sauvegardé avec", nrow(cities_uniformisation), "lignes.\n")
