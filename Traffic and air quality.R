
########################        VINCULACION DE CONDICIONES DE TRAFICO CON CALIDAD DEL AIRE         ##############################

library(googleway) # vinculacion al API de google maps
library(mapdeck) # Visualizacio con Mapbox , recomendable para visualizacion de ALTAS CANTIDADES DE DATOS (centenares de miles o millones de puntos)
library(leaflet) # visualizaciones espaciales web
library(readxl) # lectura de archivos excel
library(sf)  # Lectura y tratamiento de arhivos espaciales 
library(dplyr) # para el tratamiento de datos (similar a pandas en Python)
library(sp)
library(geosphere) # contiene la funcion "midPoint" que genera puntos georreferenciados entre puntos 
library(tmap) # Visualizaciones web con Tmap (no es recomendable para altas cantidades de datos (pero permite la inspeccion de elementos)
library(writexl) # Para exportar a xlsx 
library(osmdata)  # libreria de OSM para obtecion y tratamieno de datos de vialidades y lugares 


rm(list = ls(all=TRUE))
getwd()


setwd("C:/Users/orlan/Documents/ITESO/Metodologia trafico y contaminacion atmosferica/R/data")


#####################                   1) AUTENTIFICACION DE GOOGLE           #############################
keyg <- "YOUR_KEY"   # Tu llave de google maps 
set_key(key = keyg)
google_keys()



################        2) VISUALIZACION EN TIEMPO REAL DE CONDICIONE DE TRAFICO Y ZONA DE INTERES      ######################
google_map(key = keyg, location = c(20.696582, -103.377983), zoom = 12,   event_return_type = c("json")) %>%
  add_traffic()  


zona_interes <- read_sf("Americas.shp")
# cuando lees con read_sf (simple features) el objeto espacial que se lee pasa directamente a un SF que basicamente es un DF con una columna de geomtria interna que contiene el trazo  espacial del archivo 

plot(zona_interes$geometry)

# Plotear con mapbox
MAPBOX= 'YOUR_KEY'  # LLave de Mapbox
set_token(Sys.getenv("MAPBOX"))
key=MAPBOX

mapdeck(token = key, style = "mapbox://styles/orlandoandradeb/cjwrsrdko09mi1cpm91ptszfs", pitch = 45) %>%
  add_scatterplot(
    data = zona_interes
    , legend = FALSE
    , fill_colour = "#C93111"
    , radius = 10 ) 

# codigos HHTML de colores: https://htmlcolorcodes.com/es/






#####################        3) GENERACION  Y TRATAMIENTO DE LA MATRIZ DE TIEMPOS DE DESPLAZAMIENTO      ##########################

#### 3.1) Lectura de las zonas de interes (Prueba Americas N-S y Americas S-N)
Americas_NS <- read_excel("coordenadas_americas.xlsx", sheet = "NS")
Americas_SN <- read_excel("coordenadas_americas.xlsx", sheet = "SN")



# 3.2) Obtencion de la matriz de tiempos y disntancia de desplazamiento dentro de un loop 
distancia <- data.frame(matrix(NA, nrow = 1, ncol = 2))
duracion <- data.frame(matrix(NA, nrow = 1, ncol = 2))


for (i in 1:nrow(Americas_NS) - 1) {
  lectura <- google_distance(origins = Americas_NS[i, c("lat", "lon")] ,
                               destinations = Americas_NS[i+1, c("lat", "lon")],
                               simplify = TRUE, # es el tipo de formato en el cual se generar? el ouput, cuando es TRUE es un listado
                               units="metric",
                               departure_time ="now",
                               traffic_model = "best_guess")
  lectura <- lectura$rows
  lectura <- lectura$elements 
  lectura <- dplyr::bind_rows(lectura)

  distancia[i,] <- lectura$distance # en ambos DF ira almacenando los resultados de cada ciclo del loop 
  duracion[i,] <- lectura$duration_in_traffic 
                                                  }
  


### 3.3) Tratamiento de los DF generados 

datos_NS <- cbind(distancia, duracion) # union de los datos distancia y duracion 

datos_NS$X1 <- NULL # eliminacion de columnas incesarias 
datos_NS$X1 <- NULL

colnames(datos_NS)[1] <- "distance" # cambio de nombre de las columnas 
colnames(datos_NS)[2] <- "duration"

# velocidad media  en km/h
datos_NS$velocidad <- (datos_NS$distance / datos_NS$duration) * 3.6 

# pasamos la duracion a minutos 
datos_NS$duration <- datos_NS$duration/60 




### SE REPITE EL PROCESO para  S-N 
distancia <- data.frame(matrix(NA, nrow = 1, ncol = 2))
duracion <- data.frame(matrix(NA, nrow = 1, ncol = 2))


for (i in 1:nrow(Americas_SN) - 1) {
  lectura <- google_distance(origins = Americas_SN[i, c("lat", "lon")] ,
                             destinations = Americas_SN[i+1, c("lat", "lon")],
                             simplify = TRUE, # es el tipo de formato en el cual se generar? el ouput, cuando es TRUE es un listado
                             units="metric",
                             departure_time ="now",
                             traffic_model = "best_guess")
  lectura <- lectura$rows
  lectura <- lectura$elements 
  lectura <- dplyr::bind_rows(lectura)
  
  distancia[i,] <- lectura$distance
  duracion[i,] <- lectura$duration_in_traffic 
                                                }


# tratamiento de los datos geenrados 
datos_SN <- cbind(distancia, duracion) # union de los datos distancia y duracion 

datos_SN$X1 <- NULL # eliminacion de columnas incesarias 
datos_SN$X1 <- NULL

colnames(datos_SN)[1] <- "distance" # cambio de nombre de las columnas 
colnames(datos_SN)[2] <- "duration"

# Generacion de la velocidad media 
datos_SN$velocidad <- (datos_SN$distance / datos_SN$duration) * 3.6 

# pasamos las lecturas de duracion a minutos 
datos_SN$duration <- datos_SN$duration/60



##### 3.4) Vinculacion de  los datos de velocidad  media con los ID y coordenadas de sus origenes y destinos 

# generamos un ID a los datos de velocidad para AMBAS DIRECCIONES 
datos_NS$ID <- seq.int(nrow(datos_NS))
datos_SN$ID <- seq.int(nrow(datos_SN))


# generamos un ID a los datos de coordenadas e idnetificadores de los puntos , PARA AMBAS DIRECCIONES 
Americas_NS$ID <- seq.int(nrow(Americas_NS))
Americas_SN$ID <- seq.int(nrow(Americas_SN))


# Vinclacion de los DF y cambio de nombre  (los origenes)

datos_NS <- merge(datos_NS, Americas_NS, by ="ID") # vinculacion con los origenes 
names(datos_NS)[names(datos_NS) == "lon"] <- "lon_o" 
names(datos_NS)[names(datos_NS) == "lat"] <- "lat_o" 

# Vinculacion de los destinos
Americas_NS$ID <- Americas_NS$ID-1

datos_NS <- merge(datos_NS, Americas_NS, by="ID")
names(datos_NS)[names(datos_NS) == "lon"] <- "lon_d" 
names(datos_NS)[names(datos_NS) == "lat"] <- "lat_d" 
names(datos_NS)[names(datos_NS) == "id.x"] <- "id_o" 
names(datos_NS)[names(datos_NS) == "id.y"] <- "id_d" 



######      REPETIMOS EL PROCESO PARA S-N

# Vinclacion de los DF y cambio de nombre  (los origenes)

datos_SN <- merge(datos_SN, Americas_SN, by ="ID") # vinculacion con los origenes 
names(datos_SN)[names(datos_SN) == "lon"] <- "lon_o" 
names(datos_SN)[names(datos_SN) == "lat"] <- "lat_o" 

# Vinculacion de los destinos
Americas_SN$ID <- Americas_SN$ID-1

datos_SN <- merge(datos_SN, Americas_SN, by="ID")
names(datos_SN)[names(datos_SN) == "lon"] <- "lon_d" 
names(datos_SN)[names(datos_SN) == "lat"] <- "lat_d" 
names(datos_SN)[names(datos_SN) == "id.x"] <- "id_o" 
names(datos_SN)[names(datos_SN) == "id.y"] <- "id_d" 


#### Union en un DF del total de OD 
Americas_OD <- rbind(datos_NS,datos_SN)


# velocidades medias 
mean(datos_NS$velocidad) # N-S
mean(datos_SN$velocidad) # S-N
mean(Americas_OD$velocidad)  # de toda la avenida 






###########     4) VISUALIZACION DE RESULTADOS 

# Convertirlo en un  SF 
Americas_OD = st_as_sf(Americas_OD, coords = c("lon_o", "lat_o"), crs = 4326)

tmap_mode("view") # para ver en formato web el mapa generado 
tmaptools::palette_explorer()   # para ver paletas de colores 

tm_shape(Americas_OD) + tm_dots(col = "velocidad", palette= "Spectral") + tm_basemap(leaflet::providers$CartoDB.Positron)  + 
  tm_view(set.view = 12)


####  4.1) Segunda visualizacion, en puntos medios entre los pares OD (esto es opcional)

#  Generacion de centroides para cada par  O-D  (direccion N-S)
p1 <- matrix(c(datos_NS$lon_o , datos_NS$lat_o), ncol=2  ) # de preferencia lo pasamos a una matriz  ORIGENES
p2 <- matrix(c(datos_NS$lon_d , datos_NS$lat_d), ncol=2  ) # DESTINOS


midpoint <- as.data.frame(midPoint(p1,p2)) # Fuente: https://stat.ethz.ch/pipermail/r-help/2010-July/246788.html
# Nota: Mipoint ahora contiene las coordenadas de los putos ENTRE MEDIO de los origenes y destinos, ESTE Es el punto que se usara para vincularlo con el segmento vial correspondiente 


#union de DF 
datos_NS <- cbind(datos_NS, midpoint)


#####  Generacion de centroides para cada par  O-D  (direccion S-N)
p1 <- matrix(c(datos_SN$lon_o , datos_SN$lat_o), ncol=2  ) # de preferencia lo pasamos a una matriz  ORIGENES
p2 <- matrix(c(datos_SN$lon_d , datos_SN$lat_d), ncol=2  ) # DESTINOS

midpoint <- as.data.frame(midPoint(p1,p2)) # Fuente: https://stat.ethz.ch/pipermail/r-help/2010-July/246788.html

# Union de DF 
datos_SN <- cbind(datos_SN, midpoint)


# Nuevamente generacion del DF con el total de OD por ambas direcciones 
Americas_OD_2 <- rbind(datos_NS,datos_SN)

# convertirlo a un SF 
Americas_OD_2 = st_as_sf(Americas_OD_2, coords = c("lon", "lat"), crs = 4326)
plot(Americas_OD_2$geometry)


# Visualizacion final 
tm_shape(Americas_OD_2) + tm_dots(col = "velocidad", palette= "Spectral") + tm_basemap(leaflet::providers$CartoDB.Positron)  + 
  tm_view(set.view = 12)




##########      5)      EXPORTACION DE DATOS 
Americas_OD <- as_Spatial(Americas_OD)
Americas_OD <- as.data.frame(Americas_OD)

names(Americas_OD)[names(Americas_OD) == "coords.x1"] <- "lon_o" 
names(Americas_OD)[names(Americas_OD) == "coords.x2"] <- "lat_o" 

write_xlsx(Americas_OD, path =  "Americas_OD_4pm.xlsx") 







#######################    VINCULACION CON VIALIDADES 

# 4.1)  Descarga de datos viales via OSM 
# Primer paso) Dada de alta de el bounding box , LOS SHAPES viales aqui no sirven (mostrar la raz?n)
# fuente: https://boundingbox.klokantech.com/     debe de ponerlo en formato CSV 
library(osmdata)
call <- opq(bbox = c(-103.38132,20.666567,-103.36733,20.71362)) 

# descarga de los featurees de inter?s 
call <- add_osm_feature(call, key = "highway",value=c("motorway",
                                                      "primary","secondary", 
                                                      "unclassified", "residential")) %>% 
  osmdata_sf() #osmdata_sf nos generara SF de todos los datos

call    # https://rpubs.com/JesperHybel/673301  referencia 

# componentes de la red vial (nodos y links)
puntos <- call$osm_points 
lineas <- call$osm_lines

# listado de features, se categorizan con un "key" y "values" internos , hay muchos otros datos aparte de los viales dispoinibles como lugares (restaunrantes, oficinas, red de TP etc.)
# https://wiki.openstreetmap.org/wiki/Map_Features

#listado de features 
available_features()  
call <- opq(bbox = c(12.4,55.5,12.8,55.9)) 
call <- add_osm_feature(call, key = "highway",value=c("motorway",
                                                      "primary","secondary"))
mydata <- osmdata_sf(call)
mydata



# 4.2) Visualizacion de la red vial 
mapdeck(token = key, style = "mapbox://styles/orlandoandradeb/cjwrsrdko09mi1cpm91ptszfs", pitch = 45) %>%
  add_scatterplot(
    data = puntos # en este caso 3 mil puntos
    , legend = FALSE
    , fill_colour = "#C93111"
    , radius = 12 ) %>%
  
  add_path(
    data = lineas
    , stroke_colour = "RIGHT_LOC"
    , layer_id = "path_layer"
    , tooltip = "ROAD_NAME"
    , auto_highlight = TRUE
    , legend = FALSE)


