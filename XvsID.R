source("Packages.R")

entrée <-"nasa_disaster_correction.csv"
sortie <-"nasa_disaster_correction.csv"
données <-read.csv(entrée,check.names= FALSE,fileEncoding= "UTF-8")

# Renommer colonne 1 en id et supprimer la colonne 2
names(données)[1] <-"id"
données <-données[,-2]

# Sauvegarde
write.csv(données,sortie,row.names= FALSE,fileEncoding= "UTF-8")

# Vérifier doublons et NA dans id
na_id <-any(is.na(données$id))
doublon_id <-anyDuplicated(données$id) > 0

if (na_id || doublon_id) 
{cat("Problèmes dans id:\n")
if (na_id) cat("Valeurs NA dans id\n")
if (doublon_id) cat("Doublons dans id\n")} else 
{cat("Colonne id sans doublon et sans NA\n")}