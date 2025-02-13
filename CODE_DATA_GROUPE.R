# install.packages("sf")
# install.packages("leaflet")
# install.packages("readr")
# install.packages("extRremes")
library(extRemes)
library(sf)
library(leaflet)
library(dplyr)

################################
##"cleaning du jeu de données"##
  # DataSet
  data <- read.csv(file = "Q_38_previous-1950-2022_RR-T-Vent.csv", sep = ";")
  attach(data)
  # Traitement de la variable année
  annee <- substr(AAAAMMJJ, 1,4)
  data <- cbind(annee,data)
  # split en fonction des stations
  data_station <- split(data, NOM_USUEL)
  # Liste traitée comme un dataframe
  data_station$`AIGLETON-NIVOSE`
  ## Boucle création jeu de donnée par station et par année :
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
    
      # Export des données en CSV et en GeoJSON
      write.csv(Mn, file = "dataStations.csv", row.names = FALSE)
      dataconvert <- read.csv(file ="dataStations.csv", header = TRUE)
      geo_data <- st_as_sf(dataconvert, coords = c("Long", "Lat"), crs = 4326)
      st_write(geo_data, "dataStations.geojson", driver = "GeoJSON")
      
  
#######################################
##"Création d'une carte interractive"##
  
          # Première création de carte avec les données Geojson
          # txtconvert <- read.csv(file = file.choose(), header = FALSE, sep = ",", dec = ".")
          # txtconvert <- txtconvert[,-3]
          # frontIsere <- st_as_sf(txtconvert, coords = c("V1", "V2"), crs = 4326)
          # st_write(frontIsere, "isere.shp")
  ## Lecture des données traitées ci-dessus
  dataconvert <- read.csv(file ="dataStations.csv", header = TRUE)
  isere <- st_read("isere.shp")
  isere <- isere[,-1]
  isere_ligne <- st_union(isere[5:380,1]) %>%
    st_cast("LINESTRING") 
  geo_data <- st_read("dataStations.geojson")
  
  # Traitelent des stations qui possèdent des données
  isol2 <- c(109, 14, 95, 131, 19, 47, 22, 145, 163, 108, 8, 48, 83, 21, 70, 111)
  nom_isol2 <- station[isol2]
  isol3 <- c(109, 14, 95, 131, 19, 47, 22, 145, 163, 108, 8, 48, 83, 21, 70, 111, 10,105,  68, 67, 20, 96, 123,  23, 39, 122, 133, 139, 128, 178, 42, 12, 102, 28, 168, 181, 73, 55, 107, 30, 33, 137, 85, 153, 165, 38, 92, 91, 104, 191, 66, 81, 15, 10, 88, 62, 5, 1)
  nom_isol3 <- station[isol3]
  # différentiation des données que nous retenons
  #Isoler
  geo_data2 <- cbind(geo_data,rep(NA,dim(geo_data)[1]))
  geo_data2[,5] <- ifelse(geo_data$Station %in% nom_isol2, 1, 0)
  #Groupement
  geo_data3 <- cbind(geo_data,rep(NA,dim(geo_data)[1]))
  geo_data3[,5] <- ifelse(geo_data$Station %in% nom_isol3, 1, 0)
  #prapoutel
  isol4 <- c(1, 3, 4)
  nom_isol4 <- station[isol4]
  geo_data4 <- cbind(geo_data,rep(NA,dim(geo_data)[1]))
  geo_data4[,5] <- ifelse(geo_data4$Station %in% nom_isol4, 1, 0)

  # Programmation de la carte de l'isère avec l'ajout des stations
  # Carte des stations
  carte_station_1 <- leaflet(data = geo_data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(
      data = isere,
      radius = 0.1,
      color = "black",
      stroke = TRUE,
      fillOpacity = 0.7
    ) %>%
    # addPolylines(
    #   data = isere_ligne,
    #   color = "black",
    #   weight = 2,
    #   opacity = 0.8
    # ) %>%
    addCircleMarkers(
      radius = 5,
      color = "blue",
      stroke = FALSE,
      fillOpacity = 0.7,
      popup = ~Station
    ) %>%
    setView(lng = mean(dataconvert$Long), lat = mean(dataconvert$Lat), zoom = 10)
  
  # carte avec les premières stations sélectionnées
  carte_station_2 <- leaflet(data = geo_data2[which(geo_data2$rep.NA..dim.geo_data..1.. == 0),]) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(
      data = isere,
      radius = 0.1,
      color = "black",
      stroke = TRUE,
      fillOpacity = 0.7
    ) %>%
    # addPolylines(
    #   data = isere_ligne,
    #   color = "black",
    #   weight = 2,
    #   opacity = 0.8
    # ) %>%
    addCircleMarkers(
      radius = 5,
      color = "blue",
      stroke = FALSE,
      fillOpacity = 0.7,
      popup = ~Station
    ) %>%
    addCircleMarkers(
      data = geo_data2[which(geo_data2$rep.NA..dim.geo_data..1.. == 1),],
      radius = 5,
      color = "red",
      stroke = FALSE,
      fillOpacity = 0.7,
      popup = ~Station
    ) %>%
    setView(lng = mean(dataconvert$Long), lat = mean(dataconvert$Lat), zoom = 10)
  
  
  # Carte complete
  carte_station_3 <- leaflet(data = geo_data3[which(geo_data3$rep.NA..dim.geo_data..1.. == 0),]) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(
      data = isere,
      radius = 0.1,
      color = "black",
      stroke = TRUE,
      fillOpacity = 0.7
    ) %>%
    # addPolylines(
    #   data = isere_ligne,
    #   color = "black",
    #   weight = 2,
    #   opacity = 0.8
    # ) %>%
    # addCircleMarkers(
    #   radius = 5,
    #   color = "blue",
    #   stroke = FALSE,
    #   fillOpacity = 0.7,
    #   popup = ~Station
    # ) %>%
    addCircleMarkers(
      data = geo_data3[which(geo_data3$rep.NA..dim.geo_data..1.. == 1),],
      radius = 5,
      color = "red",
      stroke = FALSE,
      fillOpacity = 0.7,
      popup = ~Station
    ) %>%
    setView(lng = mean(dataconvert$Long), lat = mean(dataconvert$Lat), zoom = 10)
  
  
  # Carte Prapout
  carte_station_4 <- leaflet(data = geo_data4[which(geo_data4$rep.NA..dim.geo_data..1.. == 0),]) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(
      data = isere,
      radius = 0.1,
      color = "black",
      stroke = TRUE,
      fillOpacity = 0.7
    ) %>%
    # addPolylines(
    #   data = isere_ligne,
    #   color = "black",
    #   weight = 2,
    #   opacity = 0.8
    # ) %>%
    addCircleMarkers(
      radius = 5,
      color = "blue",
      stroke = FALSE,
      fillOpacity = 0.7,
      popup = ~Station
    ) %>%
    addCircleMarkers(
      data = geo_data4[which(geo_data4$rep.NA..dim.geo_data..1.. == 1),],
      radius = 5,
      color = "red",
      stroke = FALSE,
      fillOpacity = 0.7,
      popup = ~Station
    ) %>%
    setView(lng = mean(dataconvert$Long), lat = mean(dataconvert$Lat), zoom = 10)
                  
  carte_station_1
  carte_station_2
  carte_station_3
  carte_station_4
  
# ----------------------------------------------------------------------------------------------
  # Fonction Merge
  # LA FONCTION
  merge_utlime <- function(num_station, nb_station){
    if (nb_station == 6){
      # Recup Station
      stat1 <- geo_data3$RR...Precipitation[which(geo_data3$Station == station[num_station[1]])]
      stat2 <- geo_data3$RR...Precipitation[which(geo_data3$Station == station[num_station[2]])]
      stat3 <- geo_data3$RR...Precipitation[which(geo_data3$Station == station[num_station[3]])]
      stat4 <- geo_data3$RR...Precipitation[which(geo_data3$Station == station[num_station[4]])]
      stat5 <- geo_data3$RR...Precipitation[which(geo_data3$Station == station[num_station[5]])]
      stat6 <- geo_data3$RR...Precipitation[which(geo_data3$Station == station[num_station[6]])]
      # Merge
      stat_merge <- do.call(pmax, c(list(stat1, stat2, stat3, stat4, stat5, stat6), na.rm = TRUE))
      
    } else if (nb_station == 5){
      # Recup Station
      stat1 <- geo_data3$RR...Precipitation[which(geo_data3$Station == station[num_station[1]])]
      stat2 <- geo_data3$RR...Precipitation[which(geo_data3$Station == station[num_station[2]])]
      stat3 <- geo_data3$RR...Precipitation[which(geo_data3$Station == station[num_station[3]])]
      stat4 <- geo_data3$RR...Precipitation[which(geo_data3$Station == station[num_station[4]])]
      stat5 <- geo_data3$RR...Precipitation[which(geo_data3$Station == station[num_station[5]])]
      # Merge
      stat_merge <- do.call(pmax, c(list(stat1, stat2, stat3, stat4, stat5), na.rm = TRUE))
      
    } else if (nb_station == 4){
      # Recup Station
      stat1 <- geo_data3$RR...Precipitation[which(geo_data3$Station == station[num_station[1]])]
      stat2 <- geo_data3$RR...Precipitation[which(geo_data3$Station == station[num_station[2]])]
      stat3 <- geo_data3$RR...Precipitation[which(geo_data3$Station == station[num_station[3]])]
      stat4 <- geo_data3$RR...Precipitation[which(geo_data3$Station == station[num_station[4]])]
      # Merge
      stat_merge <- do.call(pmax, c(list(stat1, stat2, stat3, stat4), na.rm = TRUE))
    } else if (nb_station == 3){
      # Recup Station
      stat1 <- geo_data3$RR...Precipitation[which(geo_data3$Station == station[num_station[1]])]
      stat2 <- geo_data3$RR...Precipitation[which(geo_data3$Station == station[num_station[2]])]
      stat3 <- geo_data3$RR...Precipitation[which(geo_data3$Station == station[num_station[3]])]
      # Merge
      stat_merge <- do.call(pmax, c(list(stat1, stat2, stat3), na.rm = TRUE))
      
    } else if (nb_station == 2){
      # Recup Station
      stat1 <- geo_data3$RR...Precipitation[which(geo_data3$Station == station[num_station[1]])]
      stat2 <- geo_data3$RR...Precipitation[which(geo_data3$Station == station[num_station[2]])]
      # Merge
      stat_merge <- do.call(pmax, c(list(stat1, stat2), na.rm = TRUE))
      
    } else {
      # Recup Station
      stat1 <- geo_data3$RR...Precipitation[which(geo_data3$Station == station[num_station[1]])]
      # Merge
      stat_merge <- stat1
    }
    return(stat_merge)
  }
  
  # Initialisation des vecteurs de récupération des données
  NOM <- c()
  ANNEE <- c()
  PLUIE <- c()
  GEOMETRY <- c()

  
  # Manipulation avec les stations
  num_station <- c(8)
  n <- length(num_station)
  stat_merge <- merge_utlime(num_station,n)
  
  # Création du jeu de données
  NOM <- c(NOM, rep(station[num_station[1]], length(stat_merge)))
  ANNEE <- c(ANNEE, periode)
  PLUIE <- c(PLUIE, stat_merge)
  GEOMETRY <- c(GEOMETRY, rep(geo_data3$geometry[which(geo_data3$Station == station[num_station[1]])][1], length(stat_merge)))
 
  # Traitement des dataframes
  df_ult <- cbind(NOM,ANNEE,PLUIE,GEOMETRY)
  df <- as.data.frame(df_ult)
  # Pas de doublons
  df2 <- df %>% distinct(NOM,ANNEE, .keep_all = TRUE)
  df2 %>% count(NOM,ANNEE) %>% filter(n>1)
  # vréation d'un fichier csv
  df3 <- as.data.frame(df2[,(1:3)])
  df3$NOM <- as.character(df3$NOM)
  df3$ANNEE <- as.integer(df3$ANNEE)
  df3$PLUIE <- as.numeric(df3$PLUIE)
  write.csv(df3, file = "dataStationsgroupe.csv", row.names = FALSE)
  
  # création du fichier geojson
  for (i in 1:dim(df3)[1]){
    df3$LONG[i] <- unique(dataconvert$Long[which(dataconvert$Station == df3$NOM[i])])
    df3$LAT[i] <- unique(dataconvert$Lat[which(dataconvert$Station == df3$NOM[i])])
  }
  # Ecriture du fichier
  df3_geo <- st_as_sf(df3, coords = c("LONG", "LAT"), crs = 4326)
  st_write(df3_geo, "dataStationsGroupeFINAL.geojson", driver = "GeoJSON")
  # Lecture du fichier
  geo_data_groupe <- st_read("dataStationsGroupeFINAL.geojson")

# ------------------------------------------------------------------------------------------------
  # carte de ksi
  
  # carte avec les premières stations sélectionnées
  #traitement des données
  data = st_read("dataStationsGroupeFINAL.geojson")
  data_ksi = data.frame(unique(data$NOM))
  data_ksi = as.data.frame(data_ksi)
  # Calcul des ksi par GEV avec une méthode MLE
  ksi=c()
  lower =c()
  upper =c()
  nom = unique(data$NOM)[-33]
  for(i in 1:length(nom)){
    test = fevd(na.omit(data$PLUIE[which(data$NOM == nom[i])]), type="GEV", method = "MLE")
    interval = ci(test, type="parameter")
    ksi[i]= interval[3,2]
    lower[i]=interval[3,1]
    upper[i]=interval[3,3]
  }
  #CHAMROUSSE ksi= 1233.484, ci incalculable, que faire?
  
  # On enlève chamrousse pour la visualisation
  data_wcham = data_ksi[-33,]
  data_wcham= as.data.frame(cbind(data_wcham,ksi,lower,upper))
  # Test de récupérer les coordonées des points
  for(i in 1:dim(data_wcham)[1]){
    data_wcham$GEOMETRY[i]= data$geometry[which(data$NOM == data_wcham$data_wcham[i])]
  }
  # Trouver la loi associée à l'intervalle de confiance du Ksi
  for(i in 1:dim(data_wcham)[1]){
    if (data_wcham$upper[i] < 0 ){
      data_wcham$loi[i]= -1
    }else if(data_wcham$lower[i] < 0 & data_wcham$upper[i] > 0){
      data_wcham$loi[i]= 0
    }else{
      data_wcham$loi[i]= 1
    }
  }
  names(data_wcham) <- c("NOM","KSI", "LOWER", "UPPER", "GEOMETRY", "LOI")
  # Association de la Loi sur le jeu de données de base
  for (i in 1:dim(data)[1]){
    data$LOI[i] <- data_wcham$LOI[which(data_wcham$NOM == data$NOM[i])]
  }
  
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
    setView(lng = mean(dataconvert$Long), lat = mean(dataconvert$Lat), zoom = 10)
 
  
  
  
  
  
  
   
# ----------------------------------------------------------------------------------------------
# Essais extRemes
  autrans <- geo_data3$RR...Precipitation[geo_data3$Station == "AUTRANS"]
  fit <- fevd(autrans[1:20], type = "GEV", method = "MLE")
  # method = "Lmoment"
  
  
  fit <- fevd(autrans[1:20], type = "GEV", method = "Lmoments")
  plot(fit)
  # Intervalle Confiance
  ci(fit, type = "parameter")
  # changement de signe, approx par loi weibull et pas frécher, nous allons tester par loi gamble
  
  # calculer un qunatil extrem
  return.level(fit, return.period = 100)
  # Une fois tout les 100 ans on peut s'attendree à une valeur de max 119 mm de pluie
  
  # deuxieme approche avec interpolation.
  # pour l'instant ne pas prendre trop compte de la valeur du ksi
  
  # carte des Ksi, avec légende en fonction de la loi approchée
  
  # loi gamble
  fit1 <- fevd(autrans[1:20], type = "GEV")
  fit2 <- fevd(autrans[1:20], type = "Gumbel")
  plot(fit2)
  lr.test(fit1,fit2)
  
  #fonction récupère AIC des deux modèles, on garde le modèle avec le AIC le plus faible
  # on omet les valeurs NA
  

  #####################################
##"Traitements et anlyses partiels"##
  
  # Visualisation des enregistrements différés
  Prapout_pipay <-geo_data[which(geo_data$Station == "CHASSE" ),]
  Prapout <-geo_data[which(geo_data$Station == "LUZINAY BOURG" ),]
  PRAPOUT <-geo_data[which(geo_data$Station == "LUZINAY" ),]
  # Table globale
  comparaison <- cbind(PRAPOUT$Annee, PRAPOUT$RR...Precipitation, Prapout$RR...Precipitation, Prapout_pipay$RR...Precipitation)
  colnames(comparaison) <- c("Annee" , "LUZINAY", "LUZINAY BOURG", "CHASSE")
  View(comparaison)
  View(geo_data[which(geo_data$Station == "LE VERSOUD"),])
  View(geo_data[which(geo_data$Station == "GRENOBLE - LVD"),])
  
  
  isol <- c(109, 14, 95, 131, 19, 90, 147, 47, 16, 123, 22, 145, 163, 87, 86, 108, 8, 9, 48, 29,180,83, 152, 21, 120,70, 111)
  nom_isol <- station[isol]
  
  
  isol2 <- c(109, 14, 95, 131, 19, 47, 22, 145, 163, 108, 8, 48, 83, 21, 70, 111, 10,105, 179, 24, 68, 69, 67, 41, 20, 132, 158, 96, 97, 123, 32, 23,39,122,133,134,139,128,129,126,127,178,177,42,12,27,102,103,28,135,168,181,73,55,107,30,33,137,64,85,43,153,84,165,38,92,91,104,191,192,193,194,66,81,78,80,79,77,176,15,10,11,18,17,88,62,59,60,61,63,5,1,2,3,4,157)
  nom_isol2 <- station[isol2]
  
  isol4 <- c(1, 3, 4)
  nom_isol4 <- station[isol4]
  geo_data4 <- cbind(geo_data4,rep(NA,dim(geo_data4)[1]))
  geo_data4[,5] <- ifelse(geo_data4$Station %in% nom_isol4, 1, 0)
  
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
    