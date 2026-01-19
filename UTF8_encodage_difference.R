# Créer dossier
creation_dossier("resultats")

# Fichiers encodés
encodes <-tailles_fichiers("textes_encodes/")
nombres_encodes <-nrow(encodes)

# Initialiser les colonnes
encodes$jointure_nom <-character(nombres_encodes)
encodes$encodage <-character(nombres_encodes)
encodes$type <-character(nombres_encodes)
encodes$langue <-character(nombres_encodes)

for (i in 1:nombres_encodes) {
  fichier <-encodes$fichier[i]
  encodes$jointure_nom[i] <-extraction_extensions(fichier)
  encodes$encodage[i] <-extraction_encodage(fichier)
  encodes$type[i] <-extraction_type(fichier)
  encodes$langue[i] <-extraction_langue(fichier)
}

# UTF-8 ou autres encodages
utf8 <-encodes[encodes$encodage == "UTF-8", c("jointure_nom", "taille")]
colnames(utf8)[2] <-"taille_utf8"

autres <-encodes[encodes$encodage != "UTF-8", ]

# Fusion et calculs
comparaison <-merge(autres, utf8, by = "jointure_nom")
comparaison$diff_pct <-((comparaison$taille - comparaison$taille_utf8) / comparaison$taille_utf8 * 100)
comparaison <-comparaison[, c("jointure_nom", "encodage", "type", "langue", "taille", "taille_utf8", "diff_pct")]

widget <-datatable(comparaison,
caption = "Différences de taille entre UTF-8 et autres encodages")
saveWidget(widget, file = "resultats/tableau_encodage_utf8.html")
print("Résultat sauvegardé: resultats/tableau_encodage_utf8.html")

print("Résumé par encodage")
print(statistiques(comparaison, "encodage"))

print("Résumé par type")
print(statistiques(comparaison, "type"))

print("Résumé par langue")
print(statistiques(comparaison, "langue"))

# Graphiques
graphique_ggplot(comparaison, "encodage", "Différence (%) par encodage", "resultats/UTF8 - difference_par_encodage.png")
graphique_ggplot(comparaison, "type", "Différence (%) par type de texte", "resultats/UTF8 - difference_par_type.png")
graphique_ggplot(comparaison, "langue", "Différence (%) par langue", "resultats/UTF8 - diffence_par_langue.png")
