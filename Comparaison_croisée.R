# Créer dossier
creation_dossier("resultats")

# Fichiers originaux
originaux <-tailles_fichiers("textes_originaux/")
originaux$nom_fichiers <-extraction_extensions(originaux$fichier)
originaux$taille_original <-originaux$taille
originaux <-originaux[, c("nom_fichiers", "taille_original")]

# Fichiers encodés
encodes <-tailles_fichiers("textes_encodes")
nombres_encodes <-nrow(encodes)

nom_fichier <-character(nombres_encodes)
encodages <-character(nombres_encodes)
types <-character(nombres_encodes)
langues <-character(nombres_encodes)

for (i in 1:nombres_encodes) {
  fichier <-encodes$fichier[i]
  nom_fichier[i] <-extraction_extensions(fichier)
  encodages[i] <-extraction_encodage(fichier)
  types[i] <-extraction_type(fichier)
  langues[i] <-extraction_langue(fichier)
}

encodes$nom_fichiers <-nom_fichier
encodes$encodage <-encodages
encodes$type <-types
encodes$langue <-langues
encodes$jointure_nom <-paste(encodes$nom_fichiers, "_", encodes$encodage, sep = "")
encodes$taille_encodé <-encodes$taille
encodes <-encodes[, c("jointure_nom", "nom_fichiers", "encodage", "type", "langue", "taille_encodé")]

# Fichiers compressés
compresses_csv <-tailles_fichiers("textes_compresses/")
compresses_bin <-tailles_fichiers("textes_compresses_bin/")
compresses <-rbind(compresses_csv, compresses_bin)

compresses$fichier_compressé <-compresses$fichier
compression_types <-character(nrow(compresses))

for (i in 1:nrow(compresses)) {
  compression_types[i] <-detecter_compression(compresses$fichier_compressé[i])
}
compresses$compression <-compression_types
noms_sans_extensions <-gsub("\\.(brotli|gz|gzip|bz2|bzip2|xz|lzma|zip)$", "", compresses$fichier_compressé)
compresses$nom_fichiers <-extraction_extensions(basename(noms_sans_extensions))
compresses$encodage <-extraction_encodage(basename(noms_sans_extensions))
compresses$jointure_nom <-paste(compresses$nom_fichiers, "_", compresses$encodage, sep = "")
compresses <-compresses[, c("jointure_nom", "compression", "taille")]

# Fusion des tableaux
comparaison <-merge(encodes, originaux, by = "nom_fichiers")
print(c("Fusion encodes vs originaux: ", nrow(comparaison), " lignes"))

comparaison <-merge(comparaison, compresses, by = "jointure_nom")
print(c("Fusion encodes vs compresses: ", nrow(comparaison), " lignes"))

# Calculs
comparaison$gain_encodage_pct <-(comparaison$taille_encodé - comparaison$taille_original) / comparaison$taille_original * 100
comparaison$gain_compression_pct <-(comparaison$taille - comparaison$taille_encodé) / comparaison$taille_encodé * 100
comparaison$gain_total_pct <-(comparaison$taille - comparaison$taille_original) / comparaison$taille_original * 100

# Tableau
sauvegarder_html <-datatable(comparaison, caption = "Comparaison croisée entre encodage, compression, type et langue")
saveWidget(sauvegarder_html, file = "resultats/tableau_comparaison_croisee.html")
print("resultats/tableau_comparaison_croisee.html")

# Gains par langue
print("Gain d'encodage par langue:")
donnees_groupées <-group_by(comparaison, langue)
résumé <-summarise(donnees_groupées, moyenne_gain_encodage = mean(gain_encodage_pct))
print(résumé)

print("Gain de compression par langue:")
groupé <-group_by(comparaison, langue)
résumé <-summarise(groupé, moyenne_gain_compression = mean(gain_compression_pct))
print(résumé)

print("Gain d'encodage par type de texte :")
groupe <-group_by(comparaison, type)
résumé <-summarise(groupe, moyenne_gain_encodage = mean(gain_encodage_pct))
print(résumé)

print("Gain de compression par type de texte :")
groupe <-group_by(comparaison, type)
résumé <-summarise(groupe, moyenne_gain_compression = mean(gain_compression_pct))
print(résumé)

# Graphiques
comparaison_graphiques <-comparaison
comparaison_graphiques$diff_pct <-comparaison_graphiques$gain_encodage_pct
graphique_ggplot(comparaison_graphiques, "langue", "Gain moyen d'encodage (%) par langue", "resultats/gain_encodage_par_langue.png")

comparaison_graphiques$diff_pct <-comparaison_graphiques$gain_compression_pct
graphique_ggplot(comparaison_graphiques, "langue", "Gain moyen de compression (%) par langue", "resultats/gain_compression_par_langue.png")

comparaison_graphiques$diff_pct <-comparaison_graphiques$gain_encodage_pct
graphique_ggplot(comparaison_graphiques, "type", "Gain moyen d'encodage (%) par type de texte", "resultats/gain_encodage_par_type.png")

comparaison_graphiques$diff_pct <-comparaison_graphiques$gain_compression_pct
graphique_ggplot(comparaison_graphiques, "type", "Gain moyen de compression (%) par type de texte", "resultats/gain_compression_par_type.png")

comparaison_graphiques$diff_pct <-comparaison_graphiques$gain_encodage_pct
graphique_ggplot(comparaison_graphiques, "encodage", "Gain moyen d'encodage (%) par encodage", "resultats/gain_encodage_par_encodage.png")

comparaison_graphiques$diff_pct <-comparaison_graphiques$gain_compression_pct
graphique_ggplot(comparaison_graphiques, "encodage", "Gain moyen de compression (%) par encodage", "resultats/gain_compression_par_encodage.png")

comparaison_graphiques$diff_pct <-comparaison_graphiques$gain_compression_pct
graphique_ggplot(comparaison_graphiques, "compression", "Gain moyen de compression (%) par compression", "resultats/gain_compression_par_compression.png")

print("Graphiques enregistrés dans resultats")  
