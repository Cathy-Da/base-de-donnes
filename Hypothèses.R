source("Packages.R")

# HYPOTHESE 1: L’Asie est le continent ayant le plus de catastrophes

nombre_continents <-aggregate(
list(n_catastrophes =base_de_données$continent),
by= list(continent =base_de_données$continent),
FUN= length
)

nombre_continents <-nombre_continents[order(-nombre_continents$n_catastrophes), ]

cat("Total de catastrophes par continent\n")
print(nombre_continents)

continent_max <- nombre_continents[1, ]
cat("Continent avec le plus de catastrophe:",continent_max$continent,"\n")

# HYPOTHESE 2: Storm est la catastrophe la plus présente

nombre_type <-aggregate(
list(n_occurrences=base_de_données$disastertype),
by= list(disastertype= base_de_données$disastertype),
FUN= length
)

nombre_type <-nombre_type[order(nombre_type$n_occurrences), ]

cat("Total par catastrophe\n")
print(nombre_type)

type_max <-nombre_type[nrow(nombre_type), ]
cat("Type de catastrophe la plus présente:",type_max$disastertype,"\n")

# H1: L’Asie est le continent ayant le plus de catastrophes
if (continent_max$continent =="Asia") {
cat("H1: CONFIRMEE. L’Asie est le continent avec le plus de catastrophes\n")
} else {
cat("H1: NON CONFIRMEE. Le continent avec le plus de catastrophes est: ",continent_max$continent, "\n")
}

# H2: Storm est la catastrophe la plus présente
if (type_max$disastertype == "storm") {
cat("H2: CONFIRMEE. Le type de catastrophe storm est bien le plus fréquent\n")
} else {
cat("H2: NON CONFIRMEE. Le type de catastrophe le plus fréquent est: ",type_max$disastertype, "\n")
}
