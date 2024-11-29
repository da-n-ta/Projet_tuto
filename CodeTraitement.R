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

isere_ligne <- st_union(isere[5:380,1]) %>%
  st_cast("LINESTRING") 

geo_data <- st_read("C:/Users/Serge C/Documents/dataStations.geojson")

carte_station <- leaflet(data = geo_data[which(geo_data$rep.NA..dim.geo_data..1.. == 0),]) %>%
  addTiles() %>%
  addCircleMarkers(
    data = isere,
    radius = 0.1,
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
  addCircleMarkers(
    data = geo_data[which(geo_data$rep.NA..dim.geo_data..1.. == 1),],
    radius = 5,
    color = "red",
    stroke = FALSE,
    fillOpacity = 0.7,
    popup = ~Station
  ) %>%
  setView(lng = mean(dataconvert$Long), lat = mean(dataconvert$Lat), zoom = 10)
                
carte_station

# Visualisation des enregistrmeents différés
Prapout_pipay <-geo_data[which(geo_data$Station == "Prapoutel Pipay" ),]
Prapout <-geo_data[which(geo_data$Station == "Prapoutel" ),]
PRAPOUT <-geo_data[which(geo_data$Station == "PRAPOUTEL" ),]
# Table globale
comparaison <- cbind(PRAPOUT$Annee, PRAPOUT$RR...Precipitation, Prapout$RR...Precipitation, Prapout_pipay$RR...Precipitation)
colnames(comparaison) <- c("Annee", "PRAPOUTEL" , "Prapoutel", "Prapoutel Pipay")
View(comparaison)
View(geo_data[which(geo_data$Station == "LE VERSOUD"),])
View(geo_data[which(geo_data$Station == "GRENOBLE - LVD"),])


isol <- c(109, 14, 95, 131, 19, 90, 147, 47, 16, 123, 22, 145, 163, 87, 86, 108, 8, 9, 48, 29,180,83, 152, 21, 120,70, 111)
nom_isol <- station[isol]


isol2 <- c(109, 14, 95, 131, 19, 47, 22, 145, 163, 108, 8, 48, 83, 21, 70, 111, 10,105, 179, 24, 68, 69, 67, 41, 20, 132, 158, 96, 97, 123, 32, 23,39,122,133,134,139,128,129,126,127,178,177,42,12,27,102,103,28,135,168,181,73,55,107,30,33,137,64,85,43,153,84,165,38,92,91,104,191,192,193,194,66,81,78,80,79,77,176,15,10,11,18,17,88,62,59,60,61,63,5,1,2,3,4,157)
nom_isol2 <- station[isol2]

geo_data <- cbind(geo_data,rep(NA,dim(geo_data)[1]))
geo_data[,5] <- ifelse(geo_data$Station %in% nom_isol2, 1, 0)

df <- matrix(-1, nrow = 73, ncol = length(isol))
i <- 1
for (nom in nom_isol){
  df[,i] <- geo_data$RR...Precipitation[which(geo_data$Station == nom)]
  i <- i + 1
}
colnames(df) <- nom_isol
df <- cbind(geo_data$Annee,df)


isol_data <- geo_data[geo_data$Station == nom_isol2, ]


isol <- c(138, 51)
nom_isol <- station[isol]
df <- matrix(-1, nrow = 73, ncol = length(isol))
i <- 1
for (nom in nom_isol){
  df[,i] <- geo_data$RR...Precipitation[which(geo_data$Station == nom)]
  i <- i + 1
}
colnames(df) <- nom_isol
df <- cbind(geo_data$Annee,df)
View(df)
  
