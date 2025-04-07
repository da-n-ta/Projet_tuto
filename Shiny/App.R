# Librairies
library(shiny) 
library(shinyjs)
library(bslib)
library(extRemes)
library(sf)
library(leaflet)
library(dplyr)
library(RColorBrewer)
library(ggplot2)



#-------------------------------------------------------------------------------
# Datasets
# Traitement des données
data = st_read("dataStationsGroupeFINAL.geojson")
isere <- st_read("isere.shp")
isere <- isere[,-1]
isere_ligne <- st_union(isere[5:380,1]) %>%
  st_cast("LINESTRING") 
cantons <- st_read("limites-des-cantons-de-lisere.geojson")

# Initialisation
data_ksi = data.frame(unique(data$NOM))
data_ksi = as.data.frame(data_ksi)

# # Correction Stations
data$PLUIE[which(data$ANNEE == 1977 & data$NOM == "CHAMROUSSE")] <- NA
data$PLUIE[which(data$ANNEE == 1978 & data$NOM == "CHAMROUSSE")] <- NA
data$PLUIE[which(data$ANNEE == 1979 & data$NOM == "CHAMROUSSE")] <- NA
data$PLUIE[which(data$ANNEE == 1986 & data$NOM == "CHAMROUSSE")] <- NA
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

#-------------------------------------------------------------------------------
# Traitement des Xi
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

# Test de récupération des coordonées des points
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

# comparaison des modèles (avec l'AIC)
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
# Zone carte choroplèthe - Vérification des types de données
stations <- data$geometry
CANTONS <- cantons$geometry
# print(st_geometry_type(stations))  # Devrait afficher "POINT"
# print(st_geometry_type(CANTONS))   # Devrait afficher "POLYGON" ou "MULTIPOLYGON"

# Vérifier que les systèmes de projection sont identiques
# print(st_crs(stations))
# print(st_crs(CANTONS))
sf_stations <- st_as_sf(data.frame(id = 1 : length(stations)), geometry = stations)
sf_stations <- sf_stations[,-1]
# print(class(sf_stations))

sf_cantons <- st_as_sf(data.frame(id = 1 : length(CANTONS)), geometry = CANTONS)
# print(class(sf_cantons))
station_dans_cantons <- st_join(sf_stations, sf_cantons, left = FALSE)
poly <- st_intersects(sf_stations, sf_cantons)
sf_stations$canton <- sapply(poly, function(x) if (length(x) > 0) sf_cantons$geometry[x] else NA)


# ------------------------------------------------------------------------------
# Cartes Heatmaps : Création de 3 cartes pour la visualisation
choix_alpha = c(50,100,200)
# Fonction d'interpolation KNN
knn_interpolation <- function(point, stations, k = 3) {
  distances <- st_distance(point, stations)  # Matrice de distances
  nearest_indices <- order(as.vector(distances))[1:k]  # Convertir distances en vecteur avant d'ordonner
  mean_pred <- mean(stations$PRED[nearest_indices])  # Moyenne des prédictions des k plus proches voisins
  return(mean_pred)
}

# ------------------------------------------------------------------------------
# alpha = 50 #
# annee_pred <- choix_alpha[1]
# # Initialisation
# nom <- unique(data$NOM)
# loi <- df[,4]
# res <- matrix(NA, nrow = length(nom), ncol = 2)
# res[,1] <- nom
# # Boucle
# for (i in 1:length(nom)){
#   if (loi[i] == "GEV"){
#     fit <- fevd(na.omit(data$PLUIE[which(data$NOM == nom[i])]), type = "GEV")
#   } else {
#     fit <- fevd(na.omit(data$PLUIE[which(data$NOM == nom[i])]), type = "Gumbel")
#   }
#   res[i,2] <- round(return.level(fit, return.period = annee_pred,2))
# }
# # Traitement des prédictions
# res=as.data.frame(res)
# names(res)=c('NOM', 'PRED')
# data_summarized <- data %>%
#   group_by(NOM) %>%
#   summarize(geometry = first(geometry)) 
# resultat <- res %>%
#   left_join(data_summarized, by = "NOM")
# # Transformer les points des bordures en polugone
# isere_polygon <- st_polygon(list(st_coordinates(isere))) %>% 
#   st_sfc(crs = st_crs(isere)) %>% 
#   st_sf()
# # Convertir en sf
# resultat <- st_as_sf(resultat)
# # Extraire la longitude et la latitude
# coords <- st_coordinates(resultat)
# resultat <- resultat %>%
#   mutate(longitude = coords[,1],
#          latitude = coords[,2])
# resultat$PRED <- as.numeric(resultat$PRED)
# #Convertir en sf
# resultat_sf <- st_as_sf(resultat, coords = c("longitude", "latitude"), crs = 4326)
# # Créer la grille et la croiser avec les bordures
# grid_spacing <- 0.01  # Taille des carrés (~1 km)
# grid <- st_make_grid(isere, cellsize = grid_spacing, square = TRUE)
# grid_sf <- st_as_sf(data.frame(geometry = grid), crs = st_crs(isere))
# grid_sf <- st_filter(grid_sf, isere_polygon)
# # Appliquer KNN sur chaque point de la grille (long traitement)
# grid_sf$PRED_INTERP <- sapply(1:nrow(grid_sf), function(i) {
#   point <- st_sf(geometry = st_sfc(grid_sf$geometry[i]), crs = 4326)  # Point sf
#   knn_interpolation(point, resultat_sf, k = 3)
# })
# 
# # Visualisation des prédictions interpolées
# min_couleur <- min(grid_sf$PRED_INTERP) - 10
# carte50 <- ggplot() +
#   geom_sf(data = isere, fill = NA, color = "lightgray") +
#   geom_sf(data = grid_sf, aes(fill = PRED_INTERP), size = 0.3) +
#   scale_fill_gradient2(low = "red", mid = "white", high = "darkblue", 
#                        midpoint = median(125, na.rm = TRUE), 
#                        name = "Prédiction", limits = c(min_couleur, 200), oob = scales::squish) +  
#   theme_minimal()

# Solution dans le dur
grid_sf2 <- st_read("carte50.shp")
min_couleur2 <- min(grid_sf2$PRED_IN) - 10
carte50 <- ggplot() +
  geom_sf(data = isere, fill = NA, color = "lightgray") +
  geom_sf(data = grid_sf2, aes(fill = PRED_IN), size = 0.3) +
  scale_fill_gradient2(low = "red", mid = "white", high = "darkblue",
                       midpoint = median(125, na.rm = TRUE),
                       name = "Prédiction", limits = c(min_couleur2, 200), oob = scales::squish) +
  theme_minimal()

# ------------------------------------------------------------------------------
# # alpha = 100 #
# annee_pred <- choix_alpha[2]
# # Initialisation
# nom <- unique(data$NOM)
# loi <- df[,4]
# res <- matrix(NA, nrow = length(nom), ncol = 2)
# res[,1] <- nom
# # Boucle
# for (i in 1:length(nom)){
#   if (loi[i] == "GEV"){
#     fit <- fevd(na.omit(data$PLUIE[which(data$NOM == nom[i])]), type = "GEV")
#   } else {
#     fit <- fevd(na.omit(data$PLUIE[which(data$NOM == nom[i])]), type = "Gumbel")
#   }
#   res[i,2] <- round(return.level(fit, return.period = annee_pred,2))
# }
# # Traitement des prédictions
# res=as.data.frame(res)
# names(res)=c('NOM', 'PRED')
# data_summarized <- data %>%
#   group_by(NOM) %>%
#   summarize(geometry = first(geometry)) 
# resultat <- res %>%
#   left_join(data_summarized, by = "NOM")
# # Transformer les points des bordures en polugone
# isere_polygon <- st_polygon(list(st_coordinates(isere))) %>% 
#   st_sfc(crs = st_crs(isere)) %>% 
#   st_sf()
# # Convertir en sf
# resultat <- st_as_sf(resultat)
# # Extraire la longitude et la latitude
# coords <- st_coordinates(resultat)
# resultat <- resultat %>%
#   mutate(longitude = coords[,1],
#          latitude = coords[,2])
# resultat$PRED <- as.numeric(resultat$PRED)
# #Convertir en sf
# resultat_sf <- st_as_sf(resultat, coords = c("longitude", "latitude"), crs = 4326)
# # Créer la grille et la croiser avec les bordures
# grid_spacing <- 0.01  # Taille des carrés (~1 km)
# grid <- st_make_grid(isere, cellsize = grid_spacing, square = TRUE)
# grid_sf <- st_as_sf(data.frame(geometry = grid), crs = st_crs(isere))
# grid_sf <- st_filter(grid_sf, isere_polygon)
# # Appliquer KNN sur chaque point de la grille (long traitement)
# grid_sf$PRED_INTERP <- sapply(1:nrow(grid_sf), function(i) {
#   point <- st_sf(geometry = st_sfc(grid_sf$geometry[i]), crs = 4326)  # Point sf
#   knn_interpolation(point, resultat_sf, k = 3)
# })

# # Visualisation des prédictions interpolées
# min_couleur <- min(grid_sf$PRED_INTERP) - 10
# carte100 <- ggplot() +
#   geom_sf(data = isere, fill = NA, color = "lightgray") +
#   geom_sf(data = grid_sf, aes(fill = PRED_INTERP), size = 0.3) +
#   scale_fill_gradient2(low = "red", mid = "white", high = "darkblue", 
#                        midpoint = median(125, na.rm = TRUE), 
#                        name = "Prédiction", limits = c(min_couleur, 200), oob = scales::squish) +  
#   theme_minimal()

# Solution dans le dur
grid_sf2 <- st_read("carte100.shp")
min_couleur2 <- min(grid_sf2$PRED_IN) - 10
carte100 <- ggplot() +
  geom_sf(data = isere, fill = NA, color = "lightgray") +
  geom_sf(data = grid_sf2, aes(fill = PRED_IN), size = 0.3) +
  scale_fill_gradient2(low = "red", mid = "white", high = "darkblue",
                       midpoint = median(125, na.rm = TRUE),
                       name = "Prédiction", limits = c(min_couleur2, 200), oob = scales::squish) +
  theme_minimal()

# ------------------------------------------------------------------------------
# alpha = 200 #
# annee_pred <- choix_alpha[3]
# # Initialisation
# nom <- unique(data$NOM)
# loi <- df[,4]
# res <- matrix(NA, nrow = length(nom), ncol = 2)
# res[,1] <- nom
# # Boucle
# for (i in 1:length(nom)){
#   if (loi[i] == "GEV"){
#     fit <- fevd(na.omit(data$PLUIE[which(data$NOM == nom[i])]), type = "GEV")
#   } else {
#     fit <- fevd(na.omit(data$PLUIE[which(data$NOM == nom[i])]), type = "Gumbel")
#   }
#   res[i,2] <- round(return.level(fit, return.period = annee_pred,2))
# }
# # Traitement des prédictions
# res=as.data.frame(res)
# names(res)=c('NOM', 'PRED')
# data_summarized <- data %>%
#   group_by(NOM) %>%
#   summarize(geometry = first(geometry)) 
# resultat <- res %>%
#   left_join(data_summarized, by = "NOM")
# # Transformer les points des bordures en polugone
# isere_polygon <- st_polygon(list(st_coordinates(isere))) %>% 
#   st_sfc(crs = st_crs(isere)) %>% 
#   st_sf()
# # Convertir en sf
# resultat <- st_as_sf(resultat)
# # Extraire la longitude et la latitude
# coords <- st_coordinates(resultat)
# resultat <- resultat %>%
#   mutate(longitude = coords[,1],
#          latitude = coords[,2])
# resultat$PRED <- as.numeric(resultat$PRED)
# #Convertir en sf
# resultat_sf <- st_as_sf(resultat, coords = c("longitude", "latitude"), crs = 4326)
# # Créer la grille et la croiser avec les bordures
# grid_spacing <- 0.01  # Taille des carrés (~1 km)
# grid <- st_make_grid(isere, cellsize = grid_spacing, square = TRUE)
# grid_sf <- st_as_sf(data.frame(geometry = grid), crs = st_crs(isere))
# grid_sf <- st_filter(grid_sf, isere_polygon)
# # Appliquer KNN sur chaque point de la grille (long traitement)
# grid_sf$PRED_INTERP <- sapply(1:nrow(grid_sf), function(i) {
#   point <- st_sf(geometry = st_sfc(grid_sf$geometry[i]), crs = 4326)  # Point sf
#   knn_interpolation(point, resultat_sf, k = 3)
# })

# # Visualisation des prédictions interpolées
# min_couleur <- min(grid_sf$PRED_INTERP) - 10
# carte200 <- ggplot() +
#   geom_sf(data = isere, fill = NA, color = "lightgray") +
#   geom_sf(data = grid_sf, aes(fill = PRED_INTERP), size = 0.3) +
#   scale_fill_gradient2(low = "red", mid = "white", high = "darkblue", 
#                        midpoint = median(125, na.rm = TRUE), 
#                        name = "Prédiction", limits = c(min_couleur, 200), oob = scales::squish) +  
#   theme_minimal()

# Solution dans le dur
grid_sf2 <- st_read("carte200.shp")
min_couleur2 <- min(grid_sf2$PRED_IN) - 10
carte200 <- ggplot() +
  geom_sf(data = isere, fill = NA, color = "lightgray") +
  geom_sf(data = grid_sf2, aes(fill = PRED_IN), size = 0.3) +
  scale_fill_gradient2(low = "red", mid = "white", high = "darkblue",
                       midpoint = median(125, na.rm = TRUE),
                       name = "Prédiction", limits = c(min_couleur2, 200), oob = scales::squish) +
  theme_minimal()

# ------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# SHINY
ui <- fluidPage(
  shinyjs::useShinyjs(),
  navbarPage(id = "navbar", "Accueil",
             
             # 1. Page "Prévisions de précipitations extrêmes en Isère"**
             tabPanel("Prévisions de précipitations extrêmes en Isère",
                      fluidRow(id = "Présentation",
                               column(3,
                                      card(
                                        full_screen = TRUE,
                                        card_header("Carte de l'Isère avec les stations météo"),
                                        card_body(leafletOutput("carte_station", height = "600px"))
                                      )
                               ),
                               column(9,
                                      h3("Explication du projet"),
                                      p("Groupe de 4 étudiants en M1 MIASHS SSD"),
                                      p("Théorie des valeurs extrêmes (livre)"),
                                      p("Données météo issues : ", 
                                        a("météoFrance", href = "https://meteo.data.gouv.fr/datasets/donnees-climatologiques-de-base-quotidiennes/", target = "_blank"), ".")
                               )
                      )
             ),
             
             # 2. Carte Choroplèthe avec un meilleur positionnement du slider**
             tabPanel("Carte Choroplèthe",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("annee", "Choisissez une année", 
                                      min = 5, max = 200, value = 5, step = 5)
                        ),
                        mainPanel(
                          leafletOutput("carte_choroplèthe", height = "500px")
                        )
                      )
             ),
             
             # 3. Heatmap avec sidebar**
             tabPanel("Heatmap",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("choix_graph", "Choisissez un quantile de prédiction :", 
                                      choices = choix_alpha, selected = "50")
                        ),
                        mainPanel(
                          plotOutput("heatmap", width = "800px", height = "600px")
                        )
                      )
             )
  )
)

server <- function(output, input,session){
  # Carte Station
  output$carte_station <- renderLeaflet({
    leaflet(data = data) %>%
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
      setView(lng = 5.47, lat = 45.2, zoom = 8)
  })
  
  observeEvent(input$annee, {
    
    # Si annee est NULL ou vide, on arrête ici
    if (is.null(input$annee) || input$annee == "") {
      return(NULL)
    }
    
    # prédiction chorplèthe
    annee_pred <- as.numeric(input$annee)
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
      res[i,2] <- round(return.level(fit, return.period = annee_pred,2))
    }
    
    # COuleur par prédiction dans les cantons
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
    palette <- colorNumeric(palette = c("red", "white", "blue"), domain = c(50, 200))
    
    # Carte Choroplèthe
      output$carte_choroplèthe <- renderLeaflet({
        leaflet(data = CANTONS_sf) %>%
          addTiles() %>%  
          addPolygons(
            fillColor = ~palette(valeurs),  
            weight = 1,                     
            color = "white",                 
            fillOpacity = 0.7,               
            label = ~paste0("Prédiction : ", valeurs),  
            highlight = highlightOptions(weight = 2, color = "red", bringToFront = TRUE) 
          ) %>%
          addLegend(
            pal = palette, 
            values = valeurs, 
            title = "Prédiction", 
            position = "bottomright"
          )%>%
          setView(lng = 5.9, lat = 45.2, zoom = 8)
    })
  })
  
  # Carte Heatmap
  observeEvent(input$choix_graph, {
    if (input$choix_graph == 50){
      output$heatmap <- renderPlot({ carte50 })
    }else if (input$choix_graph == 100) {
      output$heatmap <- renderPlot({ carte100 })
    } else {
      output$heatmap <- renderPlot({ carte200 })
    } 
  })
}

shinyApp(ui = ui, server = server)