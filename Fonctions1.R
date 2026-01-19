# Créer un dossier
creation_dossier <-function(chemin) {
  dir.create(chemin, recursive = TRUE, showWarnings = FALSE)
}

# Sauvegarde fichier csv texte originaux
sauvegarde_csv <-function(titre, contenu, fichier_resultat) {
txt_encodes <-data.frame(
             titre = titre,
             texte_contenu = contenu
)
write_csv(txt_encodes, fichier_resultat, na = "")
}


# Langues
langues <-data.frame(
         langue = c("Français", "Anglais", "Arabe", "Chinois", "Russe"),
         wiki_code = c("fr", "en", "ar", "zh", "ru")
)
