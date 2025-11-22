source("Packages.R")

entrée  <- "nasa_disaster_correction.csv"
sortie  <- "nasa_disaster_correction.csv"
villes  <- "geonames_data/cities500_clean.rds"

base_de_données <- read.csv(entrée, check.names = FALSE, fileEncoding = "UTF-8")
villes <- readRDS(villes)

# Uniformiser les colonnes pour la jointure
uniformisation <- function(x) {tolower(trimws(x))}

# clés nasa disaster
base_de_données$key_loc <- uniformisation(base_de_données$location)
base_de_données$key_cty <- uniformisation(base_de_données$country)

# clés GeoNames
villes$key_loc <- uniformisation(villes$V3)   # V3 = ville
villes$key_cty <- uniformisation(villes$V9)   # V9 = country_code

# Jointure 
fusion <- left_join(
  base_de_données,
  villes[, c("V3", "V9", "V11", "key_loc", "key_cty")],
  by = c("key_loc", "key_cty")
)

# location et adm1
fusion$location <- ifelse(!is.na(fusion$V3), fusion$V3, fusion$location)
fusion$adm1     <- ifelse(!is.na(fusion$V11), fusion$V11, fusion$adm1)

# Supprime les colonnes temporaires
fusion <- fusion[, !is.element(names(fusion), c("key_loc", "key_cty", "V3", "V9", "V11"))]

write.csv(fusion, sortie, row.names = FALSE, fileEncoding = "UTF-8")

nb_location <- sum(!is.na(fusion$location) & fusion$location != "")
nb_adm1 <- sum(!is.na(fusion$adm1) & fusion$adm1 != "")

cat("- Lignes avec location non vide :", nb_location, "\n")
cat("- Lignes avec adm1 non vide     :", nb_adm1, "\n")
