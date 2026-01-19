# Créer dossier
creation_dossier("resultats")

# fichiers encodés
encodes <-tailles_fichiers("textes_encodes/")
nom_encodes <-character(nrow(encodes))
encodages <-character(nrow(encodes))
types <-character(nrow(encodes))
langues <-character(nrow(encodes))

for (i in 1:nrow(encodes)) {
  fichier <-encodes$fichier[i]
  nom_encodes[i] <-extraction_extensions(fichier)
  encodages[i] <-extraction_encodage(fichier)
  types[i] <-extraction_type(fichier)
  langues[i] <-extraction_langue(fichier)
}

encodes$jointure_nom <-nom_encodes
encodes$encodage <-encodages
encodes$type <-types
encodes$langue <-langues
colnames(encodes)[colnames(encodes) == "taille"] <- "taille_encodé"

# fichiers décompressés
decompresses_csv <-tailles_fichiers("textes_decompresses")
decompresses_bin <-tailles_fichiers("textes_decompresses_bin")
decompresses <-rbind(decompresses_csv, decompresses_bin)

nombres_fichiers_decompresses <-character(nrow(decompresses))
for (i in 1:nrow(decompresses)) {
  nombres_fichiers_decompresses[i] <-extraction_extensions(decompresses$fichier[i])
}
decompresses$jointure_nom <-nombres_fichiers_decompresses
colnames(decompresses)[colnames(decompresses) == "taille"] <- "taille_decompressé"

# Fusion et calculs
comparaison <-merge(encodes, decompresses, by = "jointure_nom", suffixes = c("_encodé", "_decompressé"))
comparaison$diff_pct <-(comparaison$taille_decompressé - comparaison$taille_encodé) / comparaison$taille_encodé * 100

comparaisons <-list(
  "encodage" = "Comparaison décompression vs encodage",
  "type"     = "Comparaison décompression vs type de texte",
  "langue"   = "Comparaison décompression vs langue"
)

for (var in names(comparaisons)) {
  print(comparaisons[[var]])
  resultat <-statistiques(comparaison, var)
  
  print(resultat)
}

# Graphiques
graphique_ggplot(comparaison, "encodage", "Difference (%) entre encodage et décompression", "resultats/diff_encodage_vs_decompression_encodage.png")
graphique_ggplot(comparaison, "type", "Difference (%) entre encodage et décompression par type de texte", "resultats/diff_encodage_vs_decompression_type.png")
graphique_ggplot(comparaison, "langue", "Difference (%) entre encodage et décompression par langue", "resultats/diff_encodage_vs_decompression_langue.png")

print("terminé")
