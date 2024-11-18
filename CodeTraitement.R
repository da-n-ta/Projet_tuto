# install.packages("sf")
# install.packages("leaflet")
# install.packages("readr")
library(sf)
library(leaflet)


data <- read.csv(file = file.choose(), sep = ";")
attach(data)

# Traitement de la variable année
annee <- substr(AAAAMMJJ, 1,4)
data <- cbind(annee,data)

# split 
data_station <- split(data, NOM_USUEL)
# Liste traitée comme un dataframe
data_station$`AIGLETON-NIVOSE`

# Boucle création jeu de donnée par station et par année :
# Intitalisation
station <- unique(NOM_USUEL)
periode <- sort(unique(annee), decreasing = FALSE)
N <- length(station) * length(periode)
compteur <- 1
Mn <- matrix(NA, nrow = N, ncol = 6)
colnames(Mn) <- c("Station", "Annee", "RR - Precipitation", "Lat", "Long", "Alti")

# Boucle (Attention 10 min du traitement)
for (st in station){
  for (an in periode){
    Mn[compteur, 1] <- st
    Mn[compteur, 2] <- an
    Mn[compteur, 3] <- max(na.omit(data$RR[which(annee == an & NOM_USUEL == st)]))
    # lat <- unique(na.omit(data$LAT[which(NOM_USUEL == st)]))[1]
    # Mn[compteur, 4] <- lat
    # long <- unique(na.omit(data$LON[which(NOM_USUEL == st)]))[1]
    # Mn[compteur, 5] <- long
    # alti <- unique(na.omit(data$ALTI[which(NOM_USUEL == st)]))[1]
    # Mn[compteur, 6] <- alti
    compteur <- compteur + 1
  }
}

Mn2 <- read.csv(file = file.choose(), header = TRUE)
Mn2[,3] <- Mn[,3]

# Export des données en CSV et en GeoJSON
write.csv(Mn2, file = "dataStations.csv", row.names = FALSE)
dataconvert <- read.csv(file = file.choose(), header = TRUE)
geo_data <- st_as_sf(dataconvert, coords = c("Long", "Lat"), crs = 4326)
st_write(geo_data, "dataStations.geojson", driver = "GeoJSON")



# Première création de carte avec les données Geojson
# txtconvert <- read.csv(file = file.choose(), header = FALSE, sep = ",", dec = ".")
# txtconvert <- txtconvert[,-3]
# frontIsere <- st_as_sf(txtconvert, coords = c("V1", "V2"), crs = 4326)
# st_write(frontIsere, "isere.shp")
isere <- st_read("C:/Users/Serge C/Documents/isere.shp")
isere <- isere[,-1]

isere_ligne <- st_union(isere) %>%
  st_cast("LINESTRING") 

carte_station <- leaflet(data = geo_data) %>%
  addTiles() %>%
  # addPolygons(data = isere, color = "black", weight = 2, opacity = 0.7) %>%
  addCircleMarkers(
    data = isere,
    radius = 0.5,
    color = "black",
    stroke = TRUE,
    fillOpacity = 0.7
  ) %>%
  addPolylines(
    data = isere_ligne,
    color = "black",
    weight = 2,
    opacity = 0.8
  ) %>%
  addCircleMarkers(
    radius = 5,
    color = "blue",
    stroke = FALSE,
    fillOpacity = 0.7,
    popup = ~Station
  ) %>%
  setView(lng = mean(dataconvert$Long), lat = mean(dataconvert$Lat), zoom = 10)
                
carte_station

