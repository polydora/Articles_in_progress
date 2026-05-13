# Создание карты

library(sp)
library(dplyr)
library(reshape2)
library(ggplot2)
library(ggmap)
library(mapproj)
library(maps)

# library(rgeos) #этот пакет содержит какую-то хрень, которая позволяет обойти проблему пр чтении фалов средсвами maptools. 
# #Att! Этот пакет должен быть загружен до maptools

library(mapdata)
# library(maptools) # Rgshhs
library(PBSmapping)
library(readxl)


library(sf) #Это теперь основной пакет для ГИС!

# Read the shapefile
karel_shape <- st_read("Maps/Karelia_new/data/coastlines.gpkg")

# karel_shape <- st_read("Maps/Karelia/boundary-polygon-land-lvl4.shp")

# arkhangel_shape <- st_read("Maps/Arkhangelskaya_obl/data/boundary-polygon-land-lvl2.gpkg")






kem_points <- read_excel("Data/Онежский. Острова_координаты.xlsx")

Lon_limits <- c(min(kem_points$Lon), max(kem_points$Lon))
Lat_limits <- c(min(kem_points$Lat), max(kem_points$Lat))


karel_shape_cropped <- st_crop(karel_shape, 
                               xmin = 30, 
                               xmax = 38, 
                               ymin = 64, 
                               ymax = 66)



Pl_Kem_area <-
  ggplot(data = karel_shape_cropped) +
  geom_sf(fill = "lightgreen", color = "black") +
  theme_bw() 


library(ggrepel)


Pl_Kem_area +
  annotate(geom = "point", shape = 21, size = 4, fill = "red", x = 34.5793, y =  64.9555) +
  geom_point(data = kem_points, aes(x = Lon, y = Lat), shape = 21, size = 3, fill = "yellow") 

