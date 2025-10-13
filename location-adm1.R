# --- LocationAdm1_from_cities500.R ---
# Mise à jour de 'location' et 'adm1' via GeoNames cities500_clean.rds
# ⚠️ On écrase les valeurs existantes (overwrite)

suppressWarnings(suppressMessages(library(dplyr)))

INFILE  <- "nasa_disaster_correction.csv"
OUTFILE <- "nasa_disaster_correction.csv"
CITIES  <- "geonames_data/cities500_clean.rds"

# 1) Charger la base corrigée
data <- read.csv(INFILE, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")

# 2) Charger cities500_clean
cities <- readRDS(CITIES)

# 3) Préparer clés de jointure
# Normaliser pour éviter majuscules/espaces gênants
normalize <- function(x) {
  tolower(trimws(x))
}

data$key_loc <- normalize(data$location)
data$key_cty <- normalize(data$country)

cities$key_loc <- normalize(cities$city)
cities$key_cty <- normalize(cities$country_code)

# 4) Jointure gauche (on garde toutes les lignes de data)
merged <- left_join(
  data,
  cities[, c("city","country_code","admin1_code","key_loc","key_cty")],
  by = c("key_loc","key_cty")
)

# 5) Écraser location et adm1 avec les valeurs de cities500 si trouvées
merged$location <- ifelse(!is.na(merged$city), merged$city, merged$location)
merged$adm1     <- ifelse(!is.na(merged$admin1_code), merged$admin1_code, merged$adm1)

# 6) Nettoyer colonnes techniques
merged <- merged[, !(names(merged) %in% c("key_loc","key_cty","city","country_code","admin1_code"))]

# 7) Réécriture (on écrase le fichier original)
write.csv(merged, OUTFILE, row.names = FALSE, fileEncoding = "UTF-8")

# 8) Bilan
nb_loc <- sum(!is.na(merged$location) & merged$location != "")
nb_adm1 <- sum(!is.na(merged$adm1) & merged$adm1 != "")
cat("✅ Mise à jour terminée.\n")
cat("- Lignes avec location non vide :", nb_loc, "\n")
cat("- Lignes avec adm1 non vide     :", nb_adm1, "\n")
