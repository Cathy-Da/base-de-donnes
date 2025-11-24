# Download de GeoNames cities500 pour pouvoir faire script location-adm1

dir.create("geonames_data")

URL <-"https://download.geonames.org/export/dump/cities500.zip"
ZIP <-file.path("geonames_data","cities500.zip")
TXT <-file.path("geonames_data","cities500.txt")

# Télécharger, décompresser, lecture fichier geoname
download.file(URL,ZIP,mode ="wb")
unzip(ZIP,exdir ="geonames_data",overwrite =TRUE)
cities <-read.delim(TXT,header =FALSE,sep ="\t",quote ="")

# Colonne nécessaire à la base de données après vérification
cities_uniformisation <-cities[,c(1, 3, 9, 11)]
names(cities_uniformisation) <-c("V1","V3","V9","V11")

# Sauvegarde
saveRDS(cities_uniformisation,file ="geonames_data/cities500_clean.rds")
cat("OK: cities500_clean.rds créé.\n")
