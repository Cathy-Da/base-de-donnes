source("Packages.R")

fichier_entrée <-"nasa_disaster_correction.csv"

# Fonction
extraire_coordonnees_simple<- function(geoloc) 
{
morceaux <-strsplit(geoloc,",")
latitude <-as.numeric(sapply(morceaux,function(x) trimws(x[1])))
longitude <-as.numeric(sapply(morceaux,function(x) trimws(x[2])))
data.frame(lat = latitude,lon =longitude)
}

# Lecture
base_de_donnees <-read.csv(fichier_entrée,check.names =FALSE,fileEncoding ="UTF-8")

# Extraction des coordonnées
coordonnees <-extraire_coordonnees_simple(base_de_donnees$geolocation)
base_de_donnees$latitude <-coordonnees$lat
base_de_donnees$longitude <-coordonnees$lon

# carte
monde <-map_data("world")
theme_carte <-theme_void() +
theme (legend.position = "bottom",legend.title = element_text(face = "bold"),
legend.background =element_rect(fill ="white",colour =NA),
plot.title =element_text(face ="bold",size =16,hjust =0.5),
plot.caption =element_text(size =10))

# Carte globale
carte_globale <-ggplot() +
geom_polygon(data = monde,aes(long, lat, group = group),fill ="white",colour ="black",linewidth =0.2) +
geom_point(data = base_de_donnees,aes(x = longitude, y = latitude, colour = disastertype)) +
scale_color_brewer(palette = "Set1",name = "Type de catastrophe") +
labs(title = "Carte mondiale des catastrophes naturelles",
caption = paste("Source : nasa_disaster_correction.csv\nTotal d’événements :", nrow(base_de_donnees))) +
theme_carte

ggsave("Carte_Catastrophes_Global.png",carte_globale,bg ="white")

# Carte par type
types_disponibles <-sort(unique(base_de_donnees$disastertype))
catastrophes_filtre <-base_de_donnees[is.element(base_de_donnees$disastertype,types_disponibles), ]

carte_types <-ggplot() +
geom_polygon(data = monde,aes(long,lat,group =group),fill ="white",colour ="black",linewidth = 0.2) +
geom_point(data = catastrophes_filtre,aes(x = longitude, y = latitude, colour = disastertype)) +
scale_color_brewer(palette = "Set1", name = "Type de catastrophe") +
labs(title = "Catastrophes naturelles par type",caption = paste("Source : nasa_disaster_correction.csv\nTotal d’événements :",
nrow(base_de_donnees))) +
facet_wrap(~ disastertype) +
theme_carte

ggsave("Carte_Catastrophes_ParType.png",carte_types,bg ="white")
cat("Carte_Catastrophes_Global.png\n")
cat("Carte_Catastrophes_ParType.png\n")
cat("Nombre total d’événements tracés:", nrow(base_de_donnees),"\n")
