# test pour valeurs aberrantes
tukey_valeurs_aberrantes <-function(données, colonne, label = NULL) {
  valeurs <-données[[colonne]]
  quartiles <-quantile(valeurs, probs = c(0.25, 0.75))
  Q1 <-quartiles[1]
  Q3 <-quartiles[2]
  IQR <-Q3 - Q1
  borne_inferieure <- Q1 - 1.5 * IQR
  borne_superieure <- Q3 + 1.5 * IQR
  
  print(c("Q1:", Q1, "Q3:", Q3))
  
  valeurs_aberrantes <-valeurs < borne_inferieure | valeurs > borne_superieure
  return(valeurs_aberrantes)
}

encodes <-tailles_fichiers("textes_compresses/")
decompresses <-tailles_fichiers("textes_decompresses/")

encodes$nom_fichiers <-extraction_extensions(encodes$fichier)
decompresses$nom_fichiers <-extraction_extensions(decompresses$fichier)

# Fusion
comparaison_encodage_decompression <-merge(encodes, decompresses, by = "nom_fichiers", suffixes = c("_encodé", "_decompressé"))
comparaison_encodage_decompression$type <-"Encodé vers Décompressé"

# Pourcentage de différence
comparaison_encodage_decompression$difference_pourcentage <-(
  (comparaison_encodage_decompression$taille_decompressé - comparaison_encodage_decompression$taille_encodé) /
  comparaison_encodage_decompression$taille_encodé * 100)

# Valeurs aberrantes
comparaison_encodage_decompression$valeurs_aberrantes <-tukey_valeurs_aberrantes(
  comparaison_encodage_decompression,
  "difference_pourcentage", "Encodé versus Décompressé"
)

valeurs_aberrante_encodage_decompression <-comparaison_encodage_decompression[
  comparaison_encodage_decompression$valeurs_aberrantes,
  c("nom_fichiers", "fichier_encodé", "fichier_decompressé", "difference_pourcentage")]

# extraction des données
valeurs_aberrante_encodage_decompression$encodage <-extraction_encodage(valeurs_aberrante_encodage_decompression$fichier_encodé)
valeurs_aberrante_encodage_decompression$type <-extraction_type(valeurs_aberrante_encodage_decompression$fichier_encodé)
valeurs_aberrante_encodage_decompression$langue <-extraction_langue(valeurs_aberrante_encodage_decompression$fichier_encodé)
valeurs_aberrante_encodage_decompression$compression <-detect_compression(valeurs_aberrante_encodage_decompression$fichier_encodé)

nombre_total <-nrow(valeurs_aberrante_encodage_decompression)

# tableau
table_valeurs_aberrantes <-datatable(valeurs_aberrante_encodage_decompression,
                           caption = "Valeurs aberrantes"
)

creation_dossier("resultats")
saveWidget(table_valeurs_aberrantes,
           file = "resultats/valeurs_aberrantes.html")

print("Resultats/resultats_valeurs_aberrantes.html")
nombre_aberrant_total <-nrow(valeurs_aberrante_encodage_decompression)
pourcentage_aberrant_total <-(nombre_aberrant_total / nrow(comparaison_encodage_decompression) * 100)
print(c("Nombre total des valeurs aberrantes:", nombre_aberrant_total))
print(c("Pourcentage des valeurs aberrantes:", pourcentage_aberrant_total, "%"))