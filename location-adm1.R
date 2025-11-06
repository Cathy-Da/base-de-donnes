# --- LocationAdm1_from_villes500.R ---
# Mise à jour de 'location' et 'adm1' via GeoNames villes500_clean.rds
# ⚠️ On écrase les valeurs existantes (overwrite)

source("Packages.R")

entrée  <- "nasa_disaster_correction.csv"
sortie <- "nasa_disaster_correction.csv"
villes  <- "geonames_data/cities500_clean.rds"

# 1) Charger la base corrigée
base_de_données <- read.csv(entrée, check.names = FALSE, fileEncoding = "UTF-8")

# 2) Charger villes500_clean
villes <- readRDS(villes)

# 3) Préparer clés de jointure
# Normaliser pour éviter majuscules/espaces gênants
uniformisation <- function(x) {
  tolower(trimws(x))
}

base_de_données$key_loc <- uniformisation(base_de_données$location)
base_de_données$key_cty <- uniformisation(base_de_données$country)

villes$key_loc <- uniformisation(villes$city)
villes$key_cty <- uniformisation(villes$country_code)

# 4) Jointure gauche (on garde toutes les lignes de base_de_données)
fusion <- left_join(
  base_de_données,
  villes[, c("city","country_code","admin1_code","key_loc","key_cty")],
  by = c("key_loc","key_cty")
)

# 5) Écraser location et adm1 avec les valeurs de villes500 si trouvées
fusion$location <- ifelse(!is.na(fusion$city), fusion$city, fusion$location)
fusion$adm1     <- ifelse(!is.na(fusion$admin1_code), fusion$admin1_code, fusion$adm1)

# 6) Nettoyer colonnes techniques
fusion <- fusion[, !is.element(names(fusion), c("key_loc","key_cty","city","country_code","admin1_code"))]

# 7) Réécriture (on écrase le fichier original)
write.csv(fusion, sortie, row.names = FALSE, fileEncoding = "UTF-8")

# 8) Bilan
nb_location <- sum(!is.na(fusion$location) & fusion$location != "")
nb_adm1 <- sum(!is.na(fusion$adm1) & fusion$adm1 != "")
cat("✅ Mise à jour terminée.\n")
cat("- Lignes avec location non vide :", nb_location, "\n")
cat("- Lignes avec adm1 non vide     :", nb_adm1, "\n")
