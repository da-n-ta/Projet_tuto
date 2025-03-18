library(extRemes)
library(sf)
library(leaflet)
library(dplyr)
library(RColorBrewer)

# Import des données
# carte de ksi

# carte avec les premières stations sélectionnées
#traitement des données
data = st_read("dataStationsGroupeFINAL.geojson")
isere <- st_read("isere.shp")
isere <- isere[,-1]
isere_ligne <- st_union(isere[5:380,1]) %>%
  st_cast("LINESTRING") 
cantons <- st_read("limites-des-cantons-de-lisere.geojson")

data_ksi = data.frame(unique(data$NOM))
data_ksi = as.data.frame(data_ksi)

# # Correction Chamrousse
# # pour 1977 - 78 - 79 et 86
data$PLUIE[which(data$ANNEE == 1977 & data$NOM == "CHAMROUSSE")] <- NA
data$PLUIE[which(data$ANNEE == 1978 & data$NOM == "CHAMROUSSE")] <- NA
data$PLUIE[which(data$ANNEE == 1979 & data$NOM == "CHAMROUSSE")] <- NA
data$PLUIE[which(data$ANNEE == 1986 & data$NOM == "CHAMROUSSE")] <- NA
# 
data$PLUIE[which(data$ANNEE == 1999 & data$NOM == "REVETIN")] <- NA
data$PLUIE[which(data$ANNEE == 1999 & data$NOM == "LA COTE ST ANDRE")] <- NA
data$PLUIE[which(data$ANNEE == 1999 & data$NOM == "CORPS")] <- NA
data$PLUIE[which(data$ANNEE == 1975 & data$NOM == "Les 2 ALpes Jandri")] <- NA
data$PLUIE[which(data$ANNEE == 2017 & data$NOM == "Les 2 ALpes Jandri")] <- NA
data$PLUIE[which(data$ANNEE == 1976 & data$NOM == "Les 2 ALpes Jandri")] <- NA
data$PLUIE[which(data$ANNEE == 1975 & data$NOM == "L Alpe d Huez SATA 1860")] <- NA
data$PLUIE[which(data$ANNEE == 1984 & data$NOM == "L Alpe d Huez SATA 1860")] <- NA
data$PLUIE[which(data$ANNEE == 1985 & data$NOM == "L Alpe d Huez SATA 1860")] <- NA
data$PLUIE[which(data$ANNEE == 1977 & data$NOM == "Prapoutel Pipay")] <- NA
data$PLUIE[which(data$ANNEE == 1984 & data$NOM == "Prapoutel Pipay")] <- NA
data$PLUIE[which(data$ANNEE == 1991 & data$NOM == "Prapoutel Pipay")] <- NA
data$PLUIE[which(data$ANNEE == 2003 & data$NOM == "Prapoutel Pipay")] <- NA
data$PLUIE[which(data$ANNEE == 1976 & data$NOM == "BEAUREPAIRE")] <- NA


# ------------------------------------------------------------------------------
# carte avec les premières stations sélectionnées
#traitement des données

# Calcul des ksi par GEV avec une méthode MLE
ksi=c()
lower =c()
upper =c()
nom = unique(data$NOM)
for(i in 1:length(nom)){
  test = fevd(na.omit(data$PLUIE[which(data$NOM == nom[i])]), type="GEV", method = "MLE")
  interval = ci(test, type="parameter")
  ksi[i]= interval[3,2]
  lower[i]=interval[3,1]
  upper[i]=interval[3,3]
}
#CHAMROUSSE ksi= 1233.484, ci incalculable, que faire?

# On enlève chamrousse pour la visualisation
data_ksi <- as.data.frame(cbind(data_ksi,ksi,lower,upper))

# Trouver la loi associée à l'intervalle de confiance du Ksi
for(i in 1:dim(data_ksi)[1]){
  if (data_ksi$upper[i] < 0 ){
    data_ksi$loi[i]= -1
  }else if(data_ksi$lower[i] < 0 & data_ksi$upper[i] > 0){
    data_ksi$loi[i]= 0
  }else{
    data_ksi$loi[i]= 1
  }
}

# Test de récupérer les coordonées des points
geom <- rep(NA,dim(data_ksi)[1])
data_ksi <- cbind(data_ksi, geom)
for(i in 1:dim(data_ksi)[1]){
  data_ksi$geom[i]= data$geometry[which(data$NOM == data_ksi$unique.data.NOM.[i])]
}

names(data_ksi) <- c("NOM","KSI", "LOWER", "UPPER", "LOI", "GEOMETRY")

# Association de la Loi sur le jeu de données de base
for (i in 1:dim(data)[1]){
  data$LOI[i]  <- data_ksi$LOI[which(data_ksi$NOM == data$NOM[i])]
}


# ------------------------------------------------------------------------------
# carte avec code couleur des ksi
carte_station_ksi <- leaflet(data = data[which(data$LOI == -1),]) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    data = isere,
    radius = 0.1,
    color = "black",
    stroke = TRUE,
    fillOpacity = 0.7
  ) %>%
  addPolygons(
    data = cantons[,6],
    color = "lightgrey",
    fillOpacity = 0.7
  ) %>%
  addCircleMarkers(
    radius = 5,
    color = "blue",
    stroke = FALSE,
    fillOpacity = 0.7,
    popup = ~NOM
  ) %>%
  addCircleMarkers(
    data = data[which(data$LOI == 0),],
    radius = 5,
    color = "red",
    stroke = FALSE,
    fillOpacity = 0.7,
    popup = ~NOM
  ) %>%
  addCircleMarkers(
    data = data[which(data$LOI == 1),],
    radius = 5,
    color = "green",
    stroke = FALSE,
    fillOpacity = 0.7,
    popup = ~NOM
  ) %>%
  setView(lng = 5.9293476, lat = 44.9957745, zoom = 8)

carte_station_ksi
# ------------------------------------------------------------------------------
# méthode des moments
data_ksi_moments = data.frame(unique(data$NOM))
data_ksi_moments = as.data.frame(data_ksi_moments)
ksi=c()
lower =c()
upper =c()
nom = unique(data$NOM)
for(i in 1:length(nom)){
  test = fevd(na.omit(data$PLUIE[which(data$NOM == nom[i])]), type="GEV", method = "Lmoments")
  interval = ci(test, type="parameter")
  ksi[i]= interval[3,2]
  lower[i]=interval[3,1]
  upper[i]=interval[3,3]
}
data_ksi_moments <- as.data.frame(cbind(data_ksi_moments,ksi,lower,upper))

# Trouver la loi associée à l'intervalle de confiance du Ksi
for(i in 1:dim(data_ksi_moments)[1]){
  if (data_ksi_moments$upper[i] < 0 ){
    data_ksi_moments$loi[i]= -1
  }else if(data_ksi_moments$lower[i] < 0 & data_ksi_moments$upper[i] > 0){
    data_ksi_moments$loi[i]= 0
  }else{
    data_ksi_moments$loi[i]= 1
  }
}

# Test de récupérer les coordonées des points
geom <- rep(NA,dim(data_ksi_moments)[1])
data_ksi_moments <- cbind(data_ksi_moments, geom)
for(i in 1:dim(data_ksi_moments)[1]){
  data_ksi_moments$geom[i]= data$geometry[which(data$NOM == data_ksi_moments$unique.data.NOM.[i])]
}

names(data_ksi_moments) <- c("NOM","KSI", "LOWER", "UPPER", "LOI", "GEOMETRY")

# sur jeu de données
for (i in 1:dim(data)[1]){
  data$LOI_moments[i]  <- data_ksi_moments$LOI[which(data_ksi_moments$NOM == data$NOM[i])]
}

# Carte
# carte avec code couleur des ksi
carte_station_ksi <- leaflet(data = data[which(data$LOI_moments == -1),]) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    data = isere,
    radius = 0.1,
    color = "black",
    stroke = TRUE,
    fillOpacity = 0.7
  ) %>%
  addCircleMarkers(
    radius = 5,
    color = "blue",
    stroke = FALSE,
    fillOpacity = 0.7,
    popup = ~NOM
  ) %>%
  addCircleMarkers(
    data = data[which(data$LOI_moments == 0),],
    radius = 5,
    color = "red",
    stroke = FALSE,
    fillOpacity = 0.7,
    popup = ~NOM
  ) %>%
  addCircleMarkers(
    data = data[which(data$LOI_moments == 1),],
    radius = 5,
    color = "green",
    stroke = FALSE,
    fillOpacity = 0.7,
    popup = ~NOM
  ) %>%
  setView(lng = 5.9293476, lat = 44.9957745, zoom = 8)

carte_station_ksi
# ------------------------------------------------------------------------------
# comparaison des modèles

df <- matrix(NA,nrow = length(nom), ncol = 4)
df[,1] <- nom
for(i in 1:length(nom)){
  fit1 <- fevd(na.omit(data$PLUIE[which(data$NOM == nom[i])]), type = "GEV")
  fit2 <- fevd(na.omit(data$PLUIE[which(data$NOM == nom[i])]), type = "Gumbel")
  # fit1
  k_GEV <- length(fit1$results$par)
  log_GEV <- -fit1$results$value
  AIC_GEV <- -2 * log_GEV + 2 * k_GEV
  # fit2
  k_Gumbel <- length(fit1$results$par)
  log_Gumbel <- -fit2$results$value
  AIC_Gumbel <- -2 * log_Gumbel + 2 * k_Gumbel
  # matrice résults
  df[i,2] <- AIC_GEV
  df[i,3] <- AIC_Gumbel
  if(AIC_GEV > AIC_Gumbel){
    df[i,4] <- "Gumbel"
  } else {
    df[i,4] <- "GEV"
  }
}



# ------------------------------------------------------------------------------
# quantile prévisions
n <- 200

# Initialisation
nom <- unique(data$NOM)
loi <- df[,4]
res <- matrix(NA, nrow = length(nom), ncol = 2)
res[,1] <- nom
# Boucle
for (i in 1:length(nom)){
  if (loi[i] == "GEV"){
    fit <- fevd(na.omit(data$PLUIE[which(data$NOM == nom[i])]), type = "GEV")
  } else {
    fit <- fevd(na.omit(data$PLUIE[which(data$NOM == nom[i])]), type = "Gumbel")
  }
  res[i,2] <- as.numeric(round(return.level(fit, return.period = n),2))
}


 # ------------------------------------------------------------------------------
# Zone cart choroplèthe
# rajouter nom de la station plsssss
stations <- data$geometry
CANTONS <- cantons$geometry

print(st_geometry_type(stations))  # Devrait afficher "POINT"
print(st_geometry_type(CANTONS))   # Devrait afficher "POLYGON" ou "MULTIPOLYGON"

# Vérifier que les systèmes de projection sont identiques
print(st_crs(stations))
print(st_crs(CANTONS))

sf_stations <- st_as_sf(data.frame(id = 1 : length(stations)), geometry = stations)
sf_stations <- sf_stations[,-1]
print(class(sf_stations))

sf_cantons <- st_as_sf(data.frame(id = 1 : length(CANTONS)), geometry = CANTONS)
print(class(sf_cantons))

station_dans_cantons <- st_join(sf_stations, sf_cantons, left = FALSE)
poly <- st_intersects(sf_stations, sf_cantons)

sf_stations$canton <- sapply(poly, function(x) if (length(x) > 0) sf_cantons$geometry[x] else NA)

# -------predictions--------
  
  n <- 10

# Initialisation
nom <- unique(data$NOM)
loi <- df[,4]
res <- matrix(NA, nrow = length(nom), ncol = 2)
res[,1] <- nom
# Boucle
for (i in 1:length(nom)){
  if (loi[i] == "GEV"){
    fit <- fevd(na.omit(data$PLUIE[which(data$NOM == nom[i])]), type = "GEV")
  } else {
    fit <- fevd(na.omit(data$PLUIE[which(data$NOM == nom[i])]), type = "Gumbel")
  }
  res[i,2] <- round(return.level(fit, return.period = n),2)
}

# ------------------------------------------------------------------------------
# palette de couleur delon les valeurs predict


CANTONS_sf <- st_as_sf(CANTONS)

res=as.data.frame(res)
names(res)=c('NOM', 'PRED')

data_summarized <- data %>%
  group_by(NOM) %>%
  summarize(geometry = first(geometry)) 

resultat <- res %>%
  left_join(data_summarized, by = "NOM")

sf_stations_summarized <- sf_stations %>%
  group_by(geometry) %>%
  summarize(canton= st_geometry(first(canton))) 

resultat_cantons = resultat %>% 
  left_join(sf_stations_summarized, by = "geometry")

for (i in 1:length(resultat_cantons$canton)) {
  resultat_cantons[i,5] = 
    max(resultat_cantons$PRED[which(resultat_cantons$canton == resultat_cantons$canton[i])])
}

colnames(resultat_cantons)[5] = "pred_canton"
colnames(sf_cantons)[2] = "canton"
colnames(resultat_cantons)[4] = "canton"
sf_cantons$st_canton = st_geometry(sf_cantons$canton)


for (i in 1:nrow(sf_cantons)) {
  sf_cantons[i,4] = max(resultat_cantons$pred_canton[which(resultat_cantons$canton ==sf_cantons$canton[i])])
}

colnames(sf_cantons)[4] = "pred_canton"

valeurs <- as.numeric(sf_cantons$pred_canton)
palette <- colorNumeric(palette = "Blues", domain = valeurs)

leaflet(data = CANTONS_sf) %>%
  addTiles() %>%  # Fond de carte OpenStreetMap
  addPolygons(
    fillColor = ~palette(valeurs),  # Couleur selon la variable
    weight = 1,                     # Bordure fine
    color = "white",                 # Bordure blanche
    fillOpacity = 0.7,               # Opacité des polygones
    label = ~paste0("Prédiction : ", valeurs),  # Infobulle avec la valeur
    highlight = highlightOptions(weight = 2, color = "red", bringToFront = TRUE) # Effet survol
  ) %>%
  addLegend(
    pal = palette, 
    values = valeurs, 
    title = "Prédiction", 
    position = "bottomright"
  )

