# Dossiers
dossier_originaux <-"textes_originaux"
dossier_encodes <-"textes_encodes"

# Encodages binaires
encodages_binaires <-c("UTF-16LE", "UTF-16BE", "UTF-32LE", "UTF-32BE")

encodages_bin <-function(texte, encodage) {
              texte_utf8 <-iconv(texte, from = "", to = "UTF-8", sub = "byte")
              resultat <-iconv(texte_utf8, from = "UTF-8", to = encodage, toRaw = TRUE)
              return(resultat)
}

# fichiers originaux à traiter
fichiers_originaux <-list.files(dossier_originaux, full.names = TRUE)

for (fichier in fichiers_originaux) {
  print(c("Fichier: ", fichier))
  
  donnees <-read.csv(fichier)
  
  nom_fichier <-file_path_sans_ext(basename(fichier))
  Nbr <-nrow(donnees)
  
  for (enc in encodages_binaires) {
    dossier_encodage <-file.path(dossier_encodes, enc)
    creation_dossier(dossier_encodage)
    
    if (Nbr > 0) {
    for (i in 1:N) {
        texte_bin[[i]] <- encodages_bin(donnees$texte_contenu[i], encodage = enc)
}
      
donnees_bin <-unlist(texte_bin, recursive = TRUE, use.names = FALSE)

} 
    
    else {
    donnees_bin <-raw(0)
}
    
    
chemin_fichier_binaire <- file.path(dossier_encodage, paste(nom_fichier, "_", enc, ".bin", sep = ""))
writeBin(donnees_bin, chemin_fichier_binaire)
    
print(c("Fichier encodé binaire sauvegardé: ", nom_fichier, " [", enc, "]"))
}
}

print("Encodage binaire terminé.")