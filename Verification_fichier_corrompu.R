# Créer dossier
dossiers <-c("textes_decompresses", "textes_decompresses_bin")

# Compteurs
nb_valide <-0
nb_corrompu <-0

# Vérification
verifier_fichier <-function(fichier, dossier_parent) {
extension <-file_ext(fichier)
  
# Fichier sans extension
  if (extension == "") {
  if (grepl("textes_decompresses_bin", dossier_parent)) {
      taille <-file.info(fichier)$size
  if (is.na(taille) || taille == 0) {
        print(c("❌ invalide:", fichier))
        return(FALSE)
}
      donnees_bin <-try(readBin(fichier, what = "raw", n = taille), silent = TRUE)
  if (inherits(donnees_bin, "try-error") || length(donnees_bin) == 0) {
        print(c("❌ invalide:", fichier))
        return(FALSE)
} 
      else {
        print(c("✅ Valide:", fichier))
        return(TRUE)
}
      
}
     else if (grepl("textes_decompresses", dossier_parent)) {
        donnees_csv <-try(read.csv(fichier), silent = TRUE)
  
  if (inherits(donnees_csv, "try-error")) {
        print(c("❌ invalide:", fichier))
        return(FALSE)
}
  if (nrow(donnees_csv) == 0 && ncol(donnees_csv) == 0) {
        print(c("❌ invalide:", fichier))
        return(FALSE)
} 
        else if (anyNA(names(donnees_csv))) {
        print(c("❌ invalide:", fichier))
        return(FALSE)
} 
        else {
        print(c("✅ Valide:", fichier))
        return(TRUE)
}
}
}
  
# cSV avec extension
  if (extension == "csv") {
    donnees_bin <-try(read.csv(fichier), silent = TRUE)
  if (inherits(donnees_bin, "try-error")) {
      print(c("❌ invalide:", fichier))
      return(FALSE)
}
  if (nrow(donnees_bin) == 0 && ncol(donnees_bin) == 0) {
      print(c("❌ invalide:", fichier))
      return(FALSE)
} 
    else if (anyNA(names(donnees_bin))) {
      print(c("❌ invalide:", fichier))
      return(FALSE)
} 
    else {
      print(c("✅ Valide:", fichier))
      return(TRUE)
}
}
  
# BIN avec extension
  if (extension == "bin") {
      taille <-file.info(fichier)$size
  if (is.na(taille) || taille == 0) {
      print(c("❌ invalide:", fichier))
      return(FALSE)
}
      donnees_bin <-try(readBin(fichier, what = "raw", n = taille), silent = TRUE)
  if (inherits(donnees_bin, "try-error") || length(donnees_bin) == 0) {
      print(c("❌ invalide:", fichier))
      return(FALSE)
} 
      else {
      print(c("✅ Valide:", fichier))
      return(TRUE)
}
}
  
  return(FALSE)
}

# Liste des fichiers
for (dossier in dossiers) {
  print(c("Dossier:", dossier))
  fichiers_tous <-list.files(dossier, full.names = TRUE, recursive = TRUE)
  
  if (length(fichiers_tous) == 0) {
    print("Aucun fichier trouvé.")
} 
  else {
    for (fichier in fichiers_tous) {
      resultat <-verifier_fichier(fichier, dossier)
      if (identical(resultat, TRUE)) nb_valide <- nb_valide + 1
      if (identical(resultat, FALSE)) nb_corrompu <- nb_corrompu + 1
}
}
}

print("Vérification terminée !")
print(c("✅ Fichiers valides:", nb_valide))
print(c("❌ Fichiers corrompus:", nb_corrompu))
