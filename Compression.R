# Dossiers
dossier_encodage <-"textes_encodes"
dossier_compression <-"textes_compresses"

# compressions
compressions <-c("GZIP", "BZIP2", "LZMA", "ZIP", "Brotli")

# Fonction de compression
compression_fichier <-function(fichier_debut, fichier_fin, methode) {
            fichier_temporaire <-paste(fichier_fin, "_tmp", sep = "")
            file.copy(fichier_debut, fichier_temporaire, overwrite = TRUE)
  
  if (methode == "GZIP") {
    gzip(fichier_temporaire, destname = fichier_fin, overwrite = TRUE, remove = FALSE)
} 
    else if (methode == "BZIP2") {
    bzip2(fichier_temporaire, destname = fichier_fin, overwrite = TRUE)
} 
    else if (methode == "LZMA") {
    lignes <-readLines(fichier_temporaire, encoding = "UTF-8", warn = FALSE)
    connexion <-xzfile(fichier_fin)
    writeLines(lignes, connexion, useBytes = TRUE)
    close(connexion)
} 
    else if (methode == "ZIP") {
    zipr(fichier_fin, files = fichier_temporaire)
} 
    else if (methode == "Brotli") {
    donnees <-readBin(fichier_temporaire, what = "raw", n = file.info(fichier_temporaire)$size)
    donnees_compresses <-brotli_compress(donnees)
    writeBin(donnees_compresses, fichier_fin)
  }
  
file.remove(fichier_temporaire)
print(c("Fichier compressé avec", methode, ":", fichier_fin))
}

dossiers_encodage <-list.dirs(dossier_encodage)

# Compression
for (comp in compressions) {
  print(c("Compression avec:", comp))
  
  dossier_compression_methode <-file.path(dossier_compression, comp)
  creation_dossier(dossier_compression_methode)
  
  for (dossier_encodage in dossiers_encodage) {
    encodage <-basename(dossier_encodage)
    chemin_fichier <-file.path(dossier_compression_methode, encodage)
    creation_dossier(chemin_fichier)
    
    fichiers_encodes <-list.files(dossier_encodage, pattern = "\\.csv$", full.names = TRUE)
    
    for (fichier in fichiers_encodes) {
      fichier_fin <-file.path(chemin_fichier, paste(basename(fichier), ".", tolower(comp), sep = ""))
      compression_fichier(fichier, fichier_fin, comp)
}
}
}

print("Compression terminée")
