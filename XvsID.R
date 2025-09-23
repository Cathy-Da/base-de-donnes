# Charger la base en forçant UTF-8
data <- read.csv(
  "nasa_disaster_correction.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8"
)

# Vérification : il faut bien une colonne "X"
if (!"X" %in% names(data)) stop("ECHEC : colonne 'X' absente, rien à renommer.")

# Étape 1 : renommer X -> id_unique
names(data)[names(data) == "X"] <- "id_unique"

# Étape 2 : supprimer l'ancienne colonne id si elle existe
if ("id" %in% names(data)) {
  data$id <- NULL
}

# Étape 3 : renommer id_unique -> id
names(data)[names(data) == "id_unique"] <- "id"

# Vérification
print(names(data))

# Sauvegarde en UTF-8
con <- file("nasa_disaster_correction.csv", open = "w", encoding = "UTF-8")
write.csv(data, con, row.names = FALSE)
close(con)

cat("OK : colonne 'X' renommée en 'id', ancienne colonne 'id' supprimée si présente.\n")
