# --- Hypotheses_Analyses.R ---
# Analyse descriptive pour tester deux hypothèses :
# H1 : L’Asie est le continent ayant le plus de catastrophes naturelles
# H2 : La catastrophe naturelle "storm" est celle la plus présente dans le monde

source("Packages.R")

# --- Chargement des données ---
DATAFILE <- "nasa_disaster_correction.csv"
data <- read_csv(DATAFILE, show_col_types = FALSE)

# --- Vérification de la structure ---
cat("Dimensions :", dim(data)[1], "lignes x", dim(data)[2], "colonnes\n")
cat("Colonnes disponibles :", names(data), "\n\n")

# --- Nettoyage léger ---
data <- data[!is.na(data$continent) & !is.na(data$disastertype), ]
data$continent <- trimws(tolower(data$continent))
data$disastertype <- trimws(tolower(data$disastertype))

# =====================================================================
# HYPOTHESE 1 : L’Asie est le continent ayant le plus de catastrophes
# =====================================================================

continent_counts <- aggregate(list(n_catastrophes = data$continent),
                              by = list(continent = data$continent),
                              FUN = length)
continent_counts <- continent_counts[order(-continent_counts$n_catastrophes), ]

cat("=== Nombre de catastrophes par continent ===\n")
print(continent_counts)
cat("\n")

continent_max <- continent_counts[1, ]
cat("Continent dominant :", continent_max$continent, 
    "avec", continent_max$n_catastrophes, "catastrophes.\n\n")

# Graphique H1
ggplot(continent_counts, aes(x = reorder(continent, -n_catastrophes), y = n_catastrophes)) +
  geom_col(fill = "blue") +
  geom_text(aes(label = n_catastrophes), vjust = -0.5, size = 4) +
  labs(
    title = "Nombre total de catastrophes naturelles par continent",
    x = "Continent",
    y = "Nombre de catastrophes"
  ) +
  theme_minimal(base_size = 14)

# =====================================================================
# HYPOTHESE 2 : Le type "storm" est la catastrophe la plus présente
# =====================================================================

type_counts <- aggregate(list(n_occurrences = data$disastertype),
                         by = list(disastertype = data$disastertype),
                         FUN = length)
type_counts <- type_counts[order(-type_counts$n_occurrences), ]

cat("=== Nombre d'occurrences par type de catastrophe ===\n")
print(type_counts)
cat("\n")

type_max <- type_counts[1, ]
cat("Type dominant :", type_max$disastertype,
    "avec", type_max$n_occurrences, "occurrences.\n\n")

# Graphique H2
top10 <- head(type_counts, 10)
ggplot(top10, aes(x = reorder(disastertype, -n_occurrences), y = n_occurrences)) +
  geom_col(fill = "green") +
  geom_text(aes(label = n_occurrences), vjust = -0.5, size = 4) +
  labs(
    title = "Top 10 des types de catastrophes naturelles dans le monde",
    x = "Type de catastrophe",
    y = "Nombre d'occurrences"
  ) +
  theme_minimal(base_size = 14)

# =====================================================================
# CONCLUSION AMÉLIORÉE
# =====================================================================

cat("=== CONCLUSION ===\n")

# H1 : Continent dominant
if (continent_max$continent == "asia") {
  cat("H1 : CONFIRMEE (L’Asie est bien le continent avec le plus de catastrophes)\n")
} else {
  cat("H1 : NON CONFIRMEE — Le continent avec le plus de catastrophes est : ",
      continent_max$continent, "\n")
}

# H2 : Type dominant
if (type_max$disastertype == "storm") {
  cat("H2 : CONFIRMEE (Le type 'storm' est bien le plus fréquent)\n")
} else {
  cat("H2 : NON CONFIRMEE — Le type de catastrophe le plus fréquent est : ",
      type_max$disastertype, "\n")
}
