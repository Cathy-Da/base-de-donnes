# Packages
# install.packages("countrycode")
library(countrycode)

# 1) Lire EN UTF-8 et sans altérer les noms de colonnes
data <- read.csv(
  "nasa_disaster_correction.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8"
)

# Garde-fous (9 colonnes et noms inchangés)
orig_cols <- names(data)
stopifnot(length(orig_cols) == 9)

# 2) Préparer la colonne country (trim + case-insensitive)
raw <- trimws(data$country)
u   <- toupper(raw)

# 3) Mapper vers les noms pays canoniques (countrycode)
#    (on part du nom de pays -> nom de pays standard)
country_name <- countrycode(raw, origin = "country.name", destination = "country.name", warn = FALSE)

# 3a) Corrections ponctuelles (insensibles à la casse)
fix_eq <- function(vec, key, val) {
  vec[is.na(vec) & toupper(raw) == toupper(key)] <- val
  vec
}
country_name <- fix_eq(country_name, "Kosovo",     "Kosovo")
country_name <- fix_eq(country_name, "Micronesia", "Micronesia")

# 4) Déduire le continent depuis le nom pays (avec correspondances custom)
continent <- countrycode(
  country_name,
  origin      = "country.name",
  destination = "continent",
  custom_match = c("Kosovo" = "Europe", "Micronesia" = "Oceania")
)

# 5) Injection EN PLACE (sans créer de nouvelles colonnes)
data$country   <- country_name
data$continent <- continent

# 6) Garde-fous finaux
stopifnot(identical(names(data), orig_cols))
if (ncol(data) != 9) stop("Le fichier doit avoir 9 colonnes.")
if (any(is.na(data$country)))   stop("NA dans 'country'.")
if (any(is.na(data$continent))) stop("NA dans 'continent'.")

# 7) Écrire EN UTF-8 (portable, évite le mojibake)
con <- file("nasa_disaster_correction.csv", open = "w", encoding = "UTF-8")
write.csv(data, con, row.names = FALSE)
close(con)

cat("OK: country = noms complets, continent rempli, 9 colonnes, zéro NA, UTF-8 garanti.\n")
