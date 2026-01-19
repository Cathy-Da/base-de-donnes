# textes informatiques
textes_informatiques <-data.frame(
      titre = c("C++", "C++", "سي++", "C++", "C++"),
      langue = langues$langue,
      wiki_code = langues$wiki_code
)

creation_dossier("textes_originaux")

# sauvegarde des textes
for (i in 1:nrow(textes_informatiques)) {
    contenu <-wikipedia(textes_informatiques$wiki_code[i], textes_informatiques$titre[i])
  
  if (!is.na(contenu) && contenu != "") {
      fichier_resultat <-file.path("textes_originaux", paste("textes_informatique_", textes_informatiques$langue[i], ".csv", sep = ""))
    
    sauvegarde_csv(textes_informatiques$titre[i], contenu, fichier_resultat)
    
    print(c("✅ Fichier sauvegardé: ", fichier_resultat))
} 
    else {
    print(c("❌ Erreur pour:", textes_informatiques$titre[i]))
}
}

print("Textes informatiques terminés")
