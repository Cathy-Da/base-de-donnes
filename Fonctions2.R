# Créer dossier
creation_dossier <-function(chemin) {
  dir.create(chemin, recursive = TRUE, showWarnings = FALSE)
}
  
# Lecture et taille des fichiers
tailles_fichiers <-function(path) {
  files <-list.files(path, full.names = TRUE, recursive = TRUE)
  data.frame(
    fichier = basename(files),
    chemin = files,
    taille = file.info(files)$size
)
}

# Extraction des extensions
extraction_extensions <-function(name) {
  name <-str_remove(name, "\\.csv(\\.brotli|\\.gz|\\.bz2|\\.xz|\\.zip)?$")
  name <-str_remove(name, "\\.bin(\\.brotli|\\.gz|\\.bz2|\\.xz|\\.zip)?$")
  name <-str_remove(name, "_(ASCII|UTF-8|Base64|Windows-1251|ISO-8859-1|UTF-16LE|UTF-16BE|UTF-32LE|UTF-32BE|GB2312|Shift-JIS|Big5)$")
  return(name)
}

# Extraction de l'encodage
extraction_encodage <-function(name) {
  str_extract(name, "ASCII|UTF-8|Base64|Windows-1251|ISO-8859-1|UTF-16LE|UTF-16BE|UTF-32LE|UTF-32BE|GB2312|Shift-JIS|Big5")
}

# Extraction du type
extraction_type <-function(name) {
  str_extract(name, "(?<=textes_)[^_]+")
}

# Extraction de la langue
extraction_langue <-function(name) {
  str_extract(name, "Anglais|Français|Arabe|Chinois|Russe")
}

# Détection de la méthode de compression
detecter_compression <- function(noms_fichiers) {
  ifelse(str_detect(noms_fichiers, "\\.brotli$"), "brotli",
  ifelse(str_detect(noms_fichiers, "\\.gz$|\\.gzip$"), "gzip",
  ifelse(str_detect(noms_fichiers, "\\.bz2$|\\.bzip2$"), "bzip2",
  ifelse(str_detect(noms_fichiers, "\\.xz$"), "xz",
  ifelse(str_detect(noms_fichiers, "\\.lzma$"), "lzma",
  ifelse(str_detect(noms_fichiers, "\\.zip$"), "zip", NA))))))
}

# Résumé statistique
statistiques <- function(donnees, variable) {
  groupe_variable <- as.symbol(variable) 
  resultat <- summarise(
    group_by(donnees, !!groupe_variable),
    Moyenne = mean(diff_pct),
    Mediane = median(diff_pct),
    Min = min(diff_pct),
    Max = max(diff_pct),
    Nombre = n()
)
  return(resultat)
}

# graphique ggplot
graphique_ggplot <-function(donnees, variable, titre, fichier_resultat) {
  plot <- ggplot(donnees, aes(x = .data[[variable]], y = diff_pct)) +
  stat_summary(fun = mean, geom = "bar", fill = "blue") +
  labs(title = titre, x = variable, y = "Différence moyenne (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(fichier_resultat, plot = plot, width = 8, height = 5)
}
