# dossiers
dossier_compresses_bin <-"textes_compresses_bin"
dossier_decompresses_bin <-"textes_decompresses_bin"

# Encodages binaires
encodages_binaires <-c("UTF-16LE", "UTF-16BE", "UTF-32LE", "UTF-32BE")

# compressions
compressions_bin <-c("GZIP", "BZIP2", "LZMA", "ZIP", "Brotli")

# décompresser un fichier binaire
decompresser_fichier_bin <-function(fichier_compresse, fichier_resultat, methode) {
  fichier_temporaire <-paste(fichier_resultat, "_tmp", sep = "")
  
  if (methode == "GZIP") {
    lecture <-gzfile(fichier_compresse, "rb")
    donnees <-readBin(lecture, what = "raw", n = file.info(fichier_compresse)$size)
    close(lecture)
    writeBin(donnees, fichier_temporaire)
    
} 
  else if (methode == "BZIP2") {
    lecture <-bzfile(fichier_compresse, "rb")
    donnees <-readBin(lecture, what = "raw", n = file.info(fichier_compresse)$size)
    close(lecture)
    writeBin(donnees, fichier_temporaire)
    
} 
  else if (methode == "LZMA") {
    lecture <-xzfile(fichier_compresse, "rb")
    donnees <-readBin(lecture, what = "raw", n = file.info(fichier_compresse)$size)
    close(lecture)
    writeBin(donnees, fichier_temporaire)
    
} 
  else if (methode == "ZIP") {
    unzip(fichier_compresse, exdir = dirname(fichier_resultat))
    return()
    
} 
  else if (methode == "Brotli") {
    donnees <-readBin(fichier_compresse, what = "raw", n = file.info(fichier_compresse)$size)
    decompressed_donnees <-brotli_decompress(donnees)
    writeBin(decompressed_donnees, fichier_temporaire)
}
  
file.rename(fichier_temporaire, fichier_resultat)
print(c("Fichier binaire décompressé avec ", methode, " : ", fichier_resultat))
}

# Décompressions
for (comp in compressions_bin) {
  print(c("Fichier binaire décompressé avec: ", comp))
  
  dossier_comp_bin <-file.path(dossier_compresses_bin, comp)
  dossiers_encodage_bin <-list.dirs(dossier_comp_bin)
  
  for (dossier_enc in dossiers_encodage_bin) {
    encodage <-basename(dossier_enc)
    
    dossier_sortie <-file.path(dossier_decompresses_bin, comp, encodage)
    creation_dossier(dossier_sortie)
    
    fichiers_bin_compresse <-list.files( dossier_enc,pattern = paste("\\.bin\\.", tolower(comp), "$", sep = ""),
                                         full.names = TRUE
)
    
    for (fichier in fichiers_bin_compresse) {
      fichier_resultat <-file.path(
        dossier_sortie,
        file_path_sans_ext(file_path_sans_ext(basename(fichier)))
)
      decompresser_fichier_bin(fichier, fichier_resultat, comp)
}
}
}

print("Décompression terminée")