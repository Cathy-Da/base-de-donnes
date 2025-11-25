source("Packages.R")

fichier_entrée <-"nasa_disaster_correction.csv"

# Fonction
extraire_lat_long <-function(geoloc) 
{
lat <-numeric(length(geoloc))
lon <-numeric(length(geoloc))
  
for (i in seq_along(geoloc)) {
valeurs <-strsplit(geoloc[i], ",")[[1]]
lat[i] <-as.numeric(trimws(valeurs[1]))
lon[i] <-as.numeric(trimws(valeurs[2]))
}
data.frame(lat=lat, lon=lon)
}

# Lecture
base_de_donnees <-read.csv(fichier_entrée,check.names= FALSE,fileEncoding= "UTF-8")

# Extraction des coordonnées
coordonnees <-extraire_lat_long(base_de_donnees$geolocation)
base_de_donnees$latitude <-coordonnees$lat
base_de_donnees$longitude <-coordonnees$lon

# carte
monde <-map_data("world")
theme_carte <-theme_void() +
theme (legend.position= "bottom",legend.title= element_text(face="bold"),
plot.title= element_text(face ="bold",size=8,hjust=0.5)
)

# Carte globale
carte_globale <-ggplot() +
geom_polygon(data = monde,aes(x=long, y=lat, group=group),fill ="white",colour="black",linewidth =0.2) +
geom_point(data=base_de_donnees,aes(x=longitude, y=latitude,colour= disastertype)) +
scale_color_brewer(palette= "Paired") +
labs(title="Carte mondiale des catastrophes naturelles",
caption=paste("Source: nasa_disaster_correction.csv\nTotal d’événements:",nrow(base_de_donnees))) +
theme_carte

# Carte par type
types_disponibles <-sort(unique(base_de_donnees$disastertype))
catastrophes_filtre <-base_de_donnees[is.element(base_de_donnees$disastertype,types_disponibles), ]

carte_types <-ggplot() +
geom_polygon(data=monde,aes(x=long, y=lat, group=group),fill="white",colour="black",linewidth=0.2) +
geom_point(data=catastrophes_filtre,aes(x=longitude, y=latitude, colour=disastertype)) +
scale_color_brewer(palette= "Paired", name= "Type de catastrophe") +
labs(title="Catastrophes naturelles par type",caption=paste("Source: nasa_disaster_correction.csv\nTotal d’événements:",
nrow(base_de_donnees))) +
facet_wrap(~ disastertype) +
theme_carte

ggsave("Carte_Catastrophes_Global.png",carte_globale,width= 11,height= 8,units="in",bg="white")
ggsave("Carte_Catastrophes_ParType.png",carte_types,width= 11,height= 8,units ="in",bg="white")
cat("Nombre total d’événements tracés:", nrow(base_de_donnees),"\n")
