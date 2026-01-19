# Créer dossier
creation_dossier("resultats")

# Fichiers encodés
encodes <-tailles_fichiers("textes_encodes/")
nbr_fichiers <-nrow(encodes)
nom_fichier_encodes <-character(nbr_fichiers)

if (nbr_fichiers > 0) {
for (i in 1:nbr_fichiers) {
    nom_fichier_encodes[i] <-extraction_extensions(encodes$fichier[i])
}
}

encodes$id <-nom_fichier_encodes
colnames(encodes)[colnames(encodes) == "taille"] <-"taille_encodé"

# Fichiers compressés
compresses_csv <-tailles_fichiers("textes_compresses/")
compresses_bin <-tailles_fichiers("textes_compresses_bin/")
compresses <-rbind(compresses_csv, compresses_bin)

for (i in 1:nrow(compresses)) {
  identifiants_compresses[i] <-extraction_extensions(compresses$fichier[i])
}

compresses$id <-identifiants_compresses
colnames(compresses)[colnames(compresses) == "taille"] <-"taille_compressé"

# Fusion encodées vs compressées
comparaison <-merge(encodes, compresses, by = "id")
comparaison$diff_pct <-(comparaison$taille_compressé - comparaison$taille_encodé) / comparaison$taille_encodé * 100

# Test de Shapiro-Wilk
set.seed(10)
echantillon <-sample(comparaison$diff_pct, min(5000, nrow(comparaison)))
resultat <-shapiro.test(echantillon)

# Affichage résultat
print(c("Test de Shapiro-Wilk global, nombre d'échantillon:", length(echantillon)))
print(c("W =", resultat$statistic))
print(c("p-value =", resultat$p.value))

if (resultat$p.value < 0.05) {
print("❌ N'est pas une distribution normale (p < 0.05).")
} else {
  print("✅ Est une distribution normale (p ≥ 0.05).")
}
