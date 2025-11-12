# --- Verrouillage_Colonnes.R ---
# Vérifie la validité, la cohérence et la complétude du fichier nasa_disaster_correction.csv

source("Packages.R")

# 1) Chargement du fichier
fichier <- "nasa_disaster_correction.csv"
base_de_données <- read.csv(fichier, check.names = FALSE, fileEncoding = "UTF-8")

cat("Vérification du fichier :", fichier, "\n\n")

# -------------------------------------------------------------------------
# 2) Vérification du nombre minimal de lignes et de colonnes
# -------------------------------------------------------------------------
nb_lignes_attendu <- 39953
nb_colonnes_min <- 8

if (nrow(base_de_données) < nb_lignes_attendu) {
  stop("ERREUR : le fichier contient moins de ", nb_lignes_attendu, " lignes (",
       nrow(base_de_données), " trouvées).")
} else {
  cat("✅ Nombre de lignes vérifié :", nrow(base_de_données), "(≥", nb_lignes_attendu, ")\n")
}

if (ncol(base_de_données) < nb_colonnes_min) {
  stop("ERREUR : le fichier contient moins de ", nb_colonnes_min, " colonnes (",
       ncol(base_de_données), " trouvées).")
} else {
  cat("✅ Nombre de colonnes vérifié :", ncol(base_de_données), "(≥", nb_colonnes_min, ")\n\n")
}

# -------------------------------------------------------------------------
# 3) Vérification générale : valeurs vides, NA ou invalides
# -------------------------------------------------------------------------
valeurs_invalides <- c("", "na", "n/a", "none", "null", "unknown", "undefined")

colonnes_critiques <- c("id", "country", "continent", "location",
                        "adm1", "disastertype", "level")

for (colonne in colonnes_critiques) {
  if (!colonne %in% names(base_de_données)) {
    stop("ERREUR : colonne manquante dans le fichier :", colonne)
  }
  
  valeurs <- base_de_données[[colonne]]
  if (is.character(valeurs)) {
    valeurs <- trimws(tolower(valeurs))
  }
  
  lignes_vides <- which(is.na(valeurs) | valeurs %in% valeurs_invalides)
  
  if (length(lignes_vides) > 0) {
    stop("ERREUR : la colonne '", colonne, "' contient ",
         length(lignes_vides), " valeur(s) vide(s) ou invalide(s).")
  } else {
    cat("✅ Colonne '", colonne, "' vérifiée : aucune valeur vide ou invalide.\n", sep = "")
  }
}
cat("\n")

# -------------------------------------------------------------------------
# 4) Vérification du type et de l’unicité des colonnes numériques
# -------------------------------------------------------------------------
# Colonne ID : numérique et unique
if (!is.numeric(base_de_données$id)) {
  stop("ERREUR : la colonne 'id' doit être de type numérique.")
}
if (any(duplicated(base_de_données$id))) {
  stop("ERREUR : la colonne 'id' contient des doublons.")
}
cat("✅ Colonne 'id' : numérique et sans doublon.\n")

# Colonne level : doit être 1, 2 ou 3
valeurs_level <- unique(base_de_données$level)
if (!all(valeurs_level %in% c(1, 2, 3))) {
  stop("ERREUR : la colonne 'level' contient des valeurs autres que 1, 2 ou 3.")
}
cat("✅ Colonne 'level' : valeurs autorisées = 1, 2, 3 uniquement.\n\n")

# -------------------------------------------------------------------------
# 5) Vérification des continents selon countrycode
# -------------------------------------------------------------------------
continents_autorises <- sort(unique(na.omit(countrycode::codelist$continent)))
continents_autorises <- union(continents_autorises, c("North America", "South America"))

valeurs_invalides_continent <- setdiff(unique(base_de_données$continent), continents_autorises)
if (length(valeurs_invalides_continent) > 0) {
  stop("ERREUR : colonne 'continent' contient des valeurs non reconnues : ",
       paste(valeurs_invalides_continent, collapse = ", "))
} else {
  cat("✅ Colonne 'continent' : toutes les valeurs sont valides selon countrycode.\n")
}

# -------------------------------------------------------------------------
# 6) Vérification des pays selon countrycode
# -------------------------------------------------------------------------
pays_valides <- unique(na.omit(countrycode::codelist$country.name.en))
valeurs_invalides_pays <- setdiff(unique(base_de_données$country), pays_valides)
if (length(valeurs_invalides_pays) > 0) {
  stop("ERREUR : colonne 'country' contient des valeurs non reconnues : ",
       paste(head(valeurs_invalides_pays, 5), collapse = ", "), " ...")
} else {
  cat("✅ Colonne 'country' : tous les pays sont valides selon countrycode.\n")
}

# -------------------------------------------------------------------------
# 7) Vérification du type de catastrophe
# -------------------------------------------------------------------------
types_autorises <- c("drought", "earthquake", "epidemic", "flood",
                     "impact", "insect infestation", "storm", "volcanic activity", "wildfire")

types_invalides <- setdiff(unique(tolower(trimws(base_de_données$disastertype))), types_autorises)
if (length(types_invalides) > 0) {
  stop("ERREUR : colonne 'disastertype' contient des valeurs non autorisées : ",
       paste(types_invalides, collapse = ", "))
} else {
  cat("✅ Colonne 'disastertype' : tous les types sont valides.\n")
}

# -------------------------------------------------------------------------
# 8) Résumé final
# -------------------------------------------------------------------------
cat("\n🎯 VALIDATION TERMINÉE AVEC SUCCÈS 🎯\n")
cat("Aucune erreur détectée.\n")
cat("Les colonnes et les valeurs sont toutes conformes.\n")
cat("Fichier :", fichier, "\n")
