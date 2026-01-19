# Dossiers
dossier_compresses <-"textes_compresses"
dossier_decompresses <-"textes_decompresses"

# compressions
compressions <-c("GZIP", "BZIP2", "LZMA", "ZIP", "Brotli")

# Saut de ligne nécessaire
saut_de_ligne <-function(fichier) {
        lignes <-readLines(fichier, warn = FALSE, encoding = "UTF-8")
        writeLines(c(lignes, ""), fichier, sep = "\n")
}

# Décompression fichier
decompresser_fichier <-function(fichier_compresse, fichier_resultat, methode) {
  fichier_temporaire <-paste(fichier_resultat, "_tmp", sep = "")
  
  if (methode == "GZIP") {
    gunzip(fichier_compresse, destname = fichier_temporaire, overwrite = TRUE, remove = FALSE)
    
} 
  else if (methode == "BZIP2") {
    bunzip2(fichier_compresse, destname = fichier_temporaire, overwrite = TRUE, remove = FALSE)
    
} 
  else if (methode == "LZMA") {
    lecture <-xzfile(fichier_compresse, open = "rb")
    donnees <-readBin(lecture, what = "raw", n = file.info(fichier_compresse)$size)
    close(lecture)
    writeBin(donnees, fichier_temporaire)
    
} 
  else if (methode == "ZIP" && grepl("\\.zip$", fichier_compresse, ignore.case = TRUE)) {
    fichiers_zip <-unzip(fichier_compresse, exdir = dirname(fichier_resultat))
    
    for (fichier in fichiers_zip) {
    saut_de_ligne(fichier)
}
    return()
    
} 
  else if (methode == "Brotli" && grepl("\\.brotli$", fichier_compresse, ignore.case = TRUE)) {
    donnees <-readBin(fichier_compresse, what = "raw", n = file.info(fichier_compresse)$size)
    donnees_decompresses <-brotli_decompress(donnees)
    writeBin(donnees_decompresses, fichier_temporaire)
    
} 
  else {
    print(c("❌ Le fichier ", fichier_compresse, "non valide", methode))
    return()
}
  
  saut_de_ligne(fichier_temporaire)
  if (file.exists(fichier_resultat)) file.remove(fichier_resultat)
  file.rename(fichier_temporaire, fichier_resultat)
  
  print(c("Fichier décompressé avec ", methode, ": ", fichier_resultat))
}

# Compression
for (comp in compressions) {
  print(c("Décompression des fichiers avec méthode: ", comp))
  
  dossier_comp <-file.path(dossier_compresses, comp)
  dossiers_encodage <-list.dirs(dossier_comp)
  
  for (dossier_enc in dossiers_encodage) {
    encodage <-basename(dossier_enc)
    dossier_sortie <-file.path(dossier_decompresses, comp, encodage)
    creation_dossier(dossier_sortie)
    
    fichiers_encodes <-list.files(
      dossier_enc,
      pattern = paste("\\.csv\\.", tolower(comp), "$", sep = ""),
      full.names = TRUE
)
    
    for (fichier in fichiers_encodes) {
      fichier_resultat <- file.path(
        dossier_sortie,
        file_path_sans_ext(file_path_sans_ext(basename(fichier)))
)
      decompresser_fichier(fichier, fichier_resultat, comp)
}
}
}

print("Décompression terminée") 