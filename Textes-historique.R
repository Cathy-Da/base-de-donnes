# textes historiques
textes_historiques <-data.frame(
            titre = c("Déclaration des droits de l'homme et du citoyen de 1789",
            "Declaration of the Rights of Man and of the Citizen",
            "إعلان حقوق الإنسان والمواطن",
            "人权和公民权宣言",
            "Декларация прав человека и гражданина"),
            langue = langues$langue,
            wiki_code = langues$wiki_code
)

creation_dossier("textes_originaux")

# sauvegarde des textes
for (i in 1:nrow(textes_historiques)) {
    contenu <-wikipedia(textes_historiques$wiki_code[i], textes_historiques$titre[i])
  
  if (!is.na(contenu) && contenu != "") {
      fichier_resultat <-file.path("textes_originaux", paste("textes_historique_", textes_historiques$langue[i], ".csv", sep = ""))
    
      sauvegarde_csv(textes_historiques$titre[i], contenu, fichier_resultat)
    
      print(c("✅ Fichier sauvegardé: ", fichier_resultat))
} 
    else {
    print(c("❌ Erreur pour: ", textes_historiques$titre[i]))
}
}
print("Textes historiques terminés")
