########################################################
#### Modelos de distribucion de especies con sdm #######
#### Script por Regino Cavia para Ecologia Regional ####
#### Ultima actualizacion 7/10/2021  ###################
########################################################
### Adaptado por Nico Lois y Rori Lovera 12/10/2021 ####
########################################################

# Species distribution modeling, Robert J. Hijmans and Jane Elith
# https://rspatial.org/raster/sdm/index.html
# https://www.youtube.com/watch?v=wLGanYOLzV8
# https://www.youtube.com/watch?v=83dMS3bcjJM&t=1241s

## Resetear environment ##
rm(list = ls(all.names = TRUE)) # borra todos los objetos
gc() # limpia memoria y reporta uso.
graphics.off() # borra graficos.

# install.packages("sdm")
# La primera vez intalar sdm y luego ejecutar
# installAll()

setwd("~/Documents/EcoRegional/TP6")


### Obtener datos de ocurrencia del águila coronada ####


# Opción via paquete rgbif
# install.packages("rgbif")
# library(rgbif)

# 1 ## Realizamos una busqueda en GBIF usando la libreria dismo ####
###### Primero importamos como "spatial data point" ##### 
# install.packages("dismo")
library(dismo)
gbif()
gbif(genus = "Harpyhaliaetus", species = "Harpyhaliaetus coronatus")
Ocurrencias <- gbif(genus = "Buteogallus", species = "Buteogallus coronatus")
head(Ocurrencias)
names(Ocurrencias)

Ocurrencias <- gbif("Harpyhaliaetus", "coronatus", sp=T) 
# sp=T los importa como spatial data point 
head(Ocurrencias)
names(Ocurrencias)


# 2 ##### Repetimos, pero ahora importamos como data.frame ####

Ocu_Data <-gbif("Harpyhaliaetus", "coronatus", geo=T,sp=F) # entonces, sp=FALSE!!!
head(Ocu_Data)

## graficamos con ggplot a ver como se ven los datos bajados! ##
# Sigo lo que nos mostró Pao Corrales

# install.packages("rnaturalearth")
library(rnaturalearth)
ne_countries(continent = "south america", returnclass = "sf")
# install.packages("ggplot2")
library(ggplot2)

ggplot(Ocu_Data, aes(lon,lat))+
  geom_point()+
  geom_sf(data = ne_countries(continent = "south america", returnclass = "sf"), inherit.aes = FALSE, fill = NA, color = 'black', size=0.3)
## Si no les anda, seguramente es porque Ocurrencias no es un dataframe --> Carguen Ocurrencias sin el sp=T!!

# 2.1 ## Exploramos NAs ####
Ocu_Data$lat   # observe que existen datos faltantes

# 2.2 ## Quitamos los NAs ####
W<-which(is.na(Ocu_Data$lat))
Ocu_Data<-Ocu_Data[-W,] # quito los que no tienen coordenadas
Ocu_1<-Ocu_Data[,c("lon","lat")] # Extraigo solo las coordenadas de Harpyhaliaetus coronatus
head(Ocu_1)

# 2.3 ## Repetimos la busqueda con el nombre aniguo de la especie ####
Ocu_Data <-gbif("Buteogallus", "coronatus", download = T, geo=T,sp=F)

W<-which(is.na(Ocu_Data$lat))
Ocu_Data<-Ocu_Data[-W,] # quito los que no tienen coordenadas
Ocu_2<-Ocu_Data[,c("lon","lat")] # Extraigo solo las coordenadas de Buteogallus coronatus
head(Ocu_2)

# 3 ## Uno ambas bases de datos ####
Ocu_12 <- rbind(Ocu_1, Ocu_2) # de "row bind" unir filas

# 4 ## Exploro los datos en busca de sitios duplicados (con las mismas coordenadas) ####
Dups_12 <- duplicated(Ocu_12[, c("lon","lat")]) # Saco los duplicados
Dups_12 # Devoluci?n l?gica

# 5 ## Elimino los datos duplicados ####
Ocu_unicas <- Ocu_12[!Dups_12, ] # ?Preguntar a los docentes si no entiende! 

# 6 ## Asignamos una nueva columna que llamamos especie y le ponemos un "1" por presencias  
Ocu_unicas$especie <- 1

head(Ocu_unicas)
nrow(Ocu_unicas)


Ocu_unicas <- Ocu_unicas[-613, ]
Ocu_unicas <- Ocu_unicas[-609, ]
Ocu_unicas <- Ocu_unicas[-608, ]
Ocu_unicas <- Ocu_unicas[-435, ]
max(Ocu_unicas$lon)

# 7 ## Ufff, seria mejor guardar ?no? Elija directorio y nombre del archivo ####
write.table(Ocu_unicas, file = "c:/basesgis/Argentina/Harpyhaliaetus_coronatus.txt", sep="\t", col.names = TRUE)

# 8 ## Convertimos el data.frame en un "SpatialPointsDataFrame" de sp
coordinates(Ocu_unicas) <- ~ lon + lat
class(Ocu_unicas)
plot(Ocu_unicas)

Ocu_unicas_sf <- st_as_sf(Ocu_unicas) # transformamos de sp a sf

library(tmap)

tmap_mode("view") # Activamoa la vista interactiva de tmap
tm_shape(Ocu_unicas_sf) + tm_dots()

# 9 ## Cargamos las capas rasters #### 

### Desde bioclim (repo online) ###
bioclim  <-  raster::getData('worldclim', var='bio', res=10)
plot(bioclim)

bio1 <- subset(bioclim,'bio1')

library(raster)

### Desde su carpeta ###
Bio1 <-raster("~/Documents/EcoRegional/DatosTP6/Grillas/Datos actuales/bio1.tif")
Bio4 <-raster("")
Bio12 <-raster("")
Bio15 <-raster("")
Elevacion <-raster("")

# 10 ## Las apilamos ##

Ambiente <- stack(bio1, bio4, bio12, bio15, elev, bosque)
# Ambiente <- bioclim    # por si quieren correr con toooodas las variables de bioclime (OJO TARDA MUCHO MAS)


names(Ambiente) # aca me dice los nombres de cada raster stackeado.
plot(Ambiente) # plot de los 5 / 6 raster apilados

# 11 ## Ahora a modelar!!! ####
library(sdm) 
getmethodNames() ### Modelos disponibles en sdm #####

# 12 ## generamos los datos de entrenamiento sdmData(), bg es el numero de sitios de "valores background" ####

d <- sdmData(especie ~., train= Ocu_unicas, predictors= Ambiente, bg=list(n=1800)) 
d
# bg = numero de valores tomados al azar , se puede usar = cantidad de ausencia que de presencias
# mucha discusión al respecto... Pensar y REPORTAR !
# Si no anda, lo mas seguro es que no hicieron el paso de "InstallAll" al ppio!! Si no, consulten!



# 13 ## Ajuste de modelos candidatos sdm() ####
# Esto puede tardar, pueden primero probar uno solo ####

m <- sdm(especie ~ . , d, methods=c('glm','gam','domain.dismo',"maxlike"),
         replication='sub', test.p=20) # 'gam''glm','domain.dismo',,'maxlike'

# 14 ## Evaluar el desempeño de los distintos modelos
m # o print(m) seria lo mismo

gui(m) # para una opcion visual. Si no anda, pedirnos capturas de pantalla.


# 15 ## Podemos redefinir el directorio para indicar donde guardo los resultados ####

dir = "~/Documents/EcoRegional/DatosTP6/Grillas/Datos actuales/"

# 16 ## Modelos predictivos ####
# GAM
m1 <- sdm(especie ~ . , d, methods=c('gam'),
         replication='sub', test.p=20)
# test.p = cuantos datos me guardo para probar el ajuste del modelo
AguilaPredGAM <- predict(m1, Ambiente, filename = str(dir,'AguilaPredGAM.img'), overwrite=TRUE)
windows(5.5,7)
plot(AguilaPredGAM)
points(Ocu_unicas, pch=21, cex=0.3)

# GLM
m2 <- sdm(especie ~ . , d, methods=c('glm'),
          replication='sub', test.p=5)
AguilaPredGLM <- predict(m2, Ambiente, filename = 'AguilaPredGLM.img', overwrite=TRUE)
windows(5.5,7)
plot(AguilaPredGLM)
points(Ocu_unicas, pch=21, cex=0.3)

# bioclim
m3 <- sdm(especie ~ . , d, methods=c('bioclim.dismo'),
          replication='sub', test.p=5)

AguilaPredBIOCLIM <- predict(m3, Ambiente, filename = 'AguilaPredIOCLIM.img', overwrite=TRUE)
windows(5.5,7)
plot(AguilaPredBIOCLIM)
points(Ocu_unicas, pch=21, cex=0.3)

# Domain
m4 <- sdm(especie ~ . , d, methods=c('domain.dismo'),
          replication='sub', test.p=5)

AguilaPredDOMAIN <- predict(m4, Ambiente, filename = 'AguilaPredDOMAIN.img', overwrite=TRUE)
windows(5.5,7)
plot(AguilaPredDOMAIN)
points(Ocu_unicas, pch=21, cex=0.3)

# Pueden probar otros #
# getmethodNames()     # Modelos disponibles en sdm 


# 17 ## Apilamos para graficarlos todos juntos ###
Aguila <- stack(AguilaPredGAM, AguilaPredGLM, AguilaPredBIOCLIM, AguilaPredDOMAIN)
windows()
plot(Aguila)


# 18 ## Modelo promedio ###

en <- ensemble(m, Ambiente, filename = 'ens_presente.img', overwrite=TRUE, setting=list(method='weighted',stat='AUC'))
windows()
plot(en)
points(Ocu_unicas, pch=21, cex=0.5)


## Reclasificar para obtener un corte ##
M1 <- c(0, 0.5, 0,  0.5, 1, 1)
M1
MatRec1 <- matrix(M1, ncol=3, byrow=TRUE)
en_reclas <- reclassify(en, MatRec1)
plot(en_reclas)