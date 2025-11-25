source("Packages.R")

entrée <-"nasa_disaster_correction.csv"
sortie <-"nasa_disaster_correction.csv"
villes <-"geonames_data/cities500_clean.rds"

base_de_données <-read.csv(entrée,check.names= FALSE,fileEncoding= "UTF-8")
villes <-readRDS(villes)

# Uniformiser les colonnes pour la jointure
uniformisation <-function(x) {tolower(trimws(x))}

# Fusions
fusion <-left_join(base_de_données,data.frame
(V3=uniformisation(villes$V3),V11=villes$V11, V9=uniformisation(villes$V9), stringsAsFactors = FALSE),
by=c("location"= "V3","country"= "V9"
)
)

# location et adm1
fusion$location <- ifelse(!is.na(fusion$V11), fusion$V3, fusion$location)
fusion$adm1 <- ifelse(!is.na(fusion$V11), fusion$V11, fusion$adm1)

# Supprimer les colonnes de geonames
fusion <- fusion[, !is.element(names(fusion), c("V3", "V9", "V11"))]

write.csv(fusion,sortie,row.names= FALSE,fileEncoding="UTF-8")

nb_location <-sum(!is.na(fusion$location) & fusion$location !="")
nb_adm1 <-sum(!is.na(fusion$adm1) & fusion$adm1 !="")

cat("Location non vide:",nb_location,"\n")
cat("Adm1 non vide:",nb_adm1,"\n")
