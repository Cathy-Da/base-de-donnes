# Textes tableaux
textes_tableaux <-data.frame(
             titre = c("Aide:Formules_TeX",
            "Help:Displaying_a_formula",
            "مساعدة:عرض صيغة رياضية",
            "Help:数学公式",
            "Википедия:Формулы"),
            langue = langues$langue,
            wiki_code = langues$wiki_code
)

creation_dossier("textes_originaux")

# sauvegarde des textes
for (i in 1:nrow(textes_tableaux)) {
    contenu <-wikipedia(textes_tableaux$wiki_code[i], textes_tableaux$titre[i])
  
  if (!is.na(contenu) && contenu != "") {
      fichier_resultat <-file.path("textes_originaux", paste("textes_tableau_", textes_tableaux$langue[i], ".csv", sep = ""))
    
    sauvegarde_csv(textes_tableaux$titre[i], contenu, fichier_resultat)
    
    print(c("✅ Fichier sauvegardé: ", fichier_resultat))
} 
    else {
    print(c("❌ Erreur pour:", textes_tableaux$titre[i]))
}
}

print("Textes tableaux terminés")
