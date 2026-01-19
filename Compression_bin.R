# dossiers
dossier_encodage <-"textes_encodes"
dossier_compresses_bin <-"textes_compresses_bin"

# Encodages binaires
encodages_binaires <-c("UTF-16LE", "UTF-16BE", "UTF-32LE", "UTF-32BE")
compressions_bin <-c("GZIP", "BZIP2", "LZMA", "ZIP", "Brotli")
compresser_fichier_bin <-function(fichier_debut, fichier_fin, methode) {
  
  donnees <-readBin(fichier_debut, what = "raw", n = file.info(fichier_debut)$size)
  
# compression
  if (methode == "GZIP") {
    connexion <-gzfile(fichier_fin, "wb")
    writeBin(donnees, connexion)
    close(connexion)
    
} 
  else if (methode == "BZIP2") {
    connexion <-bzfile(fichier_fin, "wb")
    writeBin(donnees, connexion)
    close(connexion)
    
} 
  else if (methode == "LZMA") {
    connexion <-xzfile(fichier_fin, "wb")
    writeBin(donnees, connexion)
    close(connexion)
    
} 
  else if (methode == "ZIP") {
    zipr(fichier_fin, files = fichier_debut)
    
} 
  else if (methode == "Brotli") {
    donnees_compresses <- brotli_compress(donnees)
    writeBin(donnees_compresses, fichier_fin)
}
  
print(c("Fichier binaire compressé avec", methode, ":", fichier_fin))
}

# dossiers
dossiers_encodage_bin <-list.dirs(dossier_encodage)

# compression
for (comp in compressions_bin) {
  print(c("Fichier binaire compressé avec:", comp))
  
  dossier_compression_bin <-file.path(dossier_compresses_bin, comp)
  creation_dossier(dossier_compression_bin)
  
  for (dossier_encodage in dossiers_encodage_bin) {
    encodage <-basename(dossier_encodage)
    
    chemin_fichier_bin <-file.path(dossier_compression_bin, encodage)
    creation_dossier(chemin_fichier_bin)
    
    fichiers_bin <-list.files(dossier_encodage, pattern = "\\.bin$", full.names = TRUE)
    
    for (fichier in fichiers_bin) {
      fichier_fin <-file.path(chemin_fichier_bin, paste(basename(fichier), ".", tolower(comp), sep = ""))
      compresser_fichier_bin(fichier, fichier_fin, comp)
}
}
}

print("Compression fichiers binaires terminée") 