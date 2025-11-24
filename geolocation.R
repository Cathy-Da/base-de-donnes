source("Packages.R")

entrée <-"nasa_disaster_correction.csv"
sortie <-"nasa_disaster_correction.csv"

Taille_lot <-300
Nb_tentatives <-3
Pause_erreur <-2
Pause_lots<-1

coordonnée_geolocation <-function(lat, long) 
{ifelse(is.na(lat) | is.na(long),NA,sprintf("%.6f, %.6f",lat,long))}

geolocation_arcgis <-function(vecteur_adresses,Nb_tentatives =3,pause_base =2) 
{for (tentative in seq_len(Nb_tentatives)) 
{resultat <-tryCatch(geo(address =vecteur_adresses,method ="arcgis",limit =1, quiet =TRUE),
error = function(e) 
{NULL}
)
if (!is.null(resultat) && all(is.element(c("address","lat","long"),names(resultat))))
{return(resultat)}
Sys.sleep(2)
}
data.frame(address =vecteur_adresses,lat =NA_real_,long =NA_real_)
}

base_de_données <-read.csv(entrée,check.names =FALSE,fileEncoding ="UTF-8")
noms_origine <-names(base_de_données)
colonnes_requises <-c("location","adm1","country","geolocation")
colonnes_manquantes <-setdiff(colonnes_requises,names(base_de_données))
if (length(colonnes_manquantes)) 
{stop("Colonnes manquantes: ",paste(colonnes_manquantes,collapse =","))}

corriger_regions <-function(valeur) 
  # Pakistan
{
valeur <-gsub("(?i)\\bN\\.?\\s*W\\.?\\s*F\\.?\\s*P\\.?\\b","Khyber Pakhtunkhwa",valeur,perl =TRUE)
valeur <-gsub("(?i)\\bF\\.?\\s*A\\.?\\s*T\\.?\\s*A\\.?\\b","Khyber Pakhtunkhwa",valeur,perl =TRUE)
  
# Philippines
valeur <-gsub("(?i)^viii$","Eastern Visayas",valeur)
valeur <-gsub("(?i)^occ mindoro$","Occidental Mindoro",valeur)
valeur <-gsub("(?i)^and bislig$","bislig",valeur)
valeur
}

# corriger
base_de_données$adm1 <-corriger_regions(base_de_données$adm1)
base_de_données$location <-corriger_regions(base_de_données$location)

adresses_completes <-nettoyer_adresse(
paste(base_de_données$location,base_de_données$adm1,base_de_données$country,sep =","))

adresses_uniques <- unique(adresses_completes)

nb_total <- length(adresses_uniques)
nb_lots <- ceiling(nb_total / Taille_lot)
resultats_lots <- vector("list", nb_lots)

ancien_timeout <- getOption("timeout")
options(timeout = max(500, ancien_timeout))

for (i in seq_len(nb_lots)) {
  debut <- (i - 1) * Taille_lot + 1
  fin   <- min(i * Taille_lot, nb_total)
  cat(sprintf("Lot %d/%d : lignes %d -> %d\n", i, nb_lots, debut, fin))
  vecteur <- adresses_uniques[debut:fin]
  resultats_lots[[i]] <- geolocation_arcgis(vecteur)
  Sys.sleep(Pause_lots)
}

options(timeout = ancien_timeout)

geo_resultats <- bind_rows(resultats_lots)

latitude  <- geo_resultats$lat[match(adresses_completes, geo_resultats$address)]
longitude <- geo_resultats$long[match(adresses_completes, geo_resultats$address)]
base_de_données$geolocation <- coordonnée_geolocation(latitude, longitude)

write.csv(base_de_données, sortie, row.names = FALSE, fileEncoding = "UTF-8")

nb_coordonnees <- sum(!is.na(base_de_données$geolocation))
nb_vide <- sum(is.na(base_de_données$geolocation))

cat("Coordonnées trouvées :", nb_coordonnees, "\n")
cat("Coordonnées manquantes :", nb_vide, "\n")
