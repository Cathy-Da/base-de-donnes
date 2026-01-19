# textes philosophiques
textes_philosophiques <-data.frame(
             titre = c("Sens de la vie",
            "Meaning of life",
            "معنى الحياة",
            "生命的意義",
            "Смысл жизни"),
            langue = langues$langue,
            wiki_code = langues$wiki_code
)

creation_dossier("textes_originaux")

# sauvegarde des textes
for (i in 1:nrow(textes_philosophiques)) {
    contenu <-wikipedia(textes_philosophiques$wiki_code[i], textes_philosophiques$titre[i])
  
  if (!is.na(contenu) && contenu != "") {
      fichier_resultat <-file.path("textes_originaux", paste("textes_philosophique_", textes_philosophiques$langue[i], ".csv", sep = ""))
    
    sauvegarde_csv(textes_philosophiques$titre[i], contenu, fichier_resultat)
    
    print(c("✅ Fichier sauvegardé: ", fichier_resultat))
} 
    else {
    print(c("❌ Erreur pour:", textes_philosophiques$titre[i]))
}
}

print("Textes philosophiques terminés")
