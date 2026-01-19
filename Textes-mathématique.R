# textes mathématiques
textes_mathematiques <-data.frame(
            titre = c("Formule sommatoire d'Abel",
            "Abel's summation formula",
            "صيغة جمع أبيل",
            "阿贝尔求和公式",
            "Формула суммирования Абеля"),
            langue = langues$langue,
            wiki_code = langues$wiki_code
)

creation_dossier("textes_originaux")

# sauvegarde des textes
for (i in 1:nrow(textes_mathematiques)) {
    contenu <-wikipedia(textes_mathematiques$wiki_code[i], textes_mathematiques$titre[i])
  
  if (!is.na(contenu) && contenu != "") {
      fichier_resultat <-file.path("textes_originaux", paste("textes_mathematique_", textes_mathematiques$langue[i], ".csv", sep = ""))
  
    sauvegarde_csv(textes_mathematiques$titre[i], contenu, fichier_resultat)
    
    print(c("✅ Fichier sauvegardé: ", fichier_resultat))
} 
    else {
    print(c("❌ Erreur pour:", textes_mathematiques$titre[i]))
}
}

print("Textes mathématiques terminés")
