# Dossiers
dossier_originaux <-"textes_originaux"
dossier_encodes <-"textes_encodes"

# Encodages texte
encodages_text <-c("UTF-8", "ISO-8859-1", "Windows-1251", "Shift-JIS",
                    "GB2312", "Big5", "Base64", "ASCII")

encoder_texte <-function(texte, encodage) {
              texte_utf8 <-iconv(texte, from = "", to = "UTF-8", sub = "byte")
  
  if (encodage == "Base64") {
  return(base64encode(charToRaw(texte_utf8)))
}
  
  return(iconv(texte_utf8, from = "UTF-8", to = encodage, sub = "byte"))
}

# Lecture des fichiers originaux
fichiers_originaux <-list.files(dossier_originaux, full.names = TRUE)

for (fichier in fichiers_originaux) {
  donnees <-read.csv(fichier)
  
  nom_fichier <-file_path_sans_ext(basename(fichier))
  
  for (encodage in encodages_text) {
  texte_encodes <-character(nrow(donnees))
    
    for (i in 1:nrow(donnees)) {
    texte_encodes[i] <-encoder_texte(donnees$texte_contenu[i], encodage)
}
    
donnees$texte_contenu <-texte_encodes
    
dossier_encodage <-file.path(dossier_encodes, encodage)
creation_dossier(dossier_encodage)
    
chemin_fichier <-file.path(dossier_encodage, paste(nom_fichier, "_", encodage, ".csv", sep = ""))
    
write.csv(donnees, chemin_fichier, row.names = FALSE)
    
print(c("Fichier encodé sauvegardé: ", nom_fichier, " [", encodage, "]"))
}
}

print("Encodage terminé.")
