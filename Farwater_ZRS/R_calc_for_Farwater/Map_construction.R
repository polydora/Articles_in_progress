library(ggmap)
library(mapproj)
library(maps)
# Создание карты

library(sp)
library(dplyr)
library(mgcv)
library(reshape2)
library(ggplot2)
library(ggmap)
library(mapproj)
library(maps)

library(rgeos) #этот пакет содержит какую-то хрень, которая позволяет обойти проблему пр чтении фалов средсвами maptools. 
#Att! Этот пакет должен быть загружен до maptools

library(mapdata)
library(maptools) # Rgshhs
library(PBSmapping)
library(gridExtra)
library(grid)



Kand_x <- c(32.3, 32.65)
Kand_y <- c(66.95, 67.12)



# read shape file into R
murm_shape <- readShapeSpatial("Maps/Murmanskaya_obl/boundary-polygon-land-lvl4.shp", proj4string = CRS("+proj=longlat +datum=WGS84"))

karel_shape <- readShapeSpatial("Maps/Karelia/boundary-polygon-land-lvl4.shp", proj4string = CRS("+proj=longlat +datum=WGS84"))


# Объединение шейп-файлов
library(raster)

Kand_shape <- bind(murm_shape, karel_shape)



# Карта для ggplot
gg_murm_karel <- fortify(Kand_shape)



Kand_map <- 
  ggplot(gg_murm_karel, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "gray") +
  # coord_map(xlim = c(150., 151.52), ylim = c(59.45, 59.8) )+
  coord_map(xlim = Kand_x, ylim = Kand_y) +
  theme_bw()




Laminaria <- readShapeSpatial("Maps/Laminaria/Laminaria.shp", proj4string = 
                                CRS("+proj=longlat +datum=WGS84"))
gg_Laminaria <- fortify(Laminaria)


Portlandia <- readShapeSpatial("Maps/Portlandia/Portlandia.shp", proj4string = 
                                 CRS("+proj=longlat +datum=WGS84"))
gg_Portlandia <- fortify(Portlandia)


Rhodophyta <- readShapeSpatial("Maps/Rhodophyta/Rhodophyta.shp", proj4string = 
                                 CRS("+proj=longlat +datum=WGS84"))
gg_Rhodophyta <- fortify(Rhodophyta)


Rhynchonella <- readShapeSpatial("Maps/Rhynchonella/Rhynchonella.shp", proj4string = 
                                 CRS("+proj=longlat +datum=WGS84"))
gg_Rhynchonella <- fortify(Rhynchonella)


Hydralmania <- readShapeSpatial("Maps/Hydralmania/Hydralmania.shp", proj4string = 
                                   CRS("+proj=longlat +datum=WGS84"))
gg_Hydralmania <- fortify(Hydralmania)


Mud <- readShapeSpatial("Maps/Mud/Mud.shp", proj4string = 
                                   CRS("+proj=longlat +datum=WGS84"))
gg_Mud <- fortify(Mud)


Pista <- readShapeSpatial("Maps/Pista/Pista.shp", proj4string = 
                          CRS("+proj=longlat +datum=WGS84"))
gg_Pista <- fortify(Pista)


Polydora <- readShapeSpatial("Maps/Polydora/Polydora.shp", proj4string = 
                          CRS("+proj=longlat +datum=WGS84"))
gg_Polydora <- fortify(Polydora)


Portlandia_Macoma <- readShapeSpatial("Maps/Portlandia_Macoma/Portlandia_Macoma.shp", proj4string = 
                          CRS("+proj=longlat +datum=WGS84"))
gg_Portlandia_Macoma <- fortify(Portlandia_Macoma)


Macoma <- readShapeSpatial("Maps/Macoma/Macoma.shp", proj4string = 
                               CRS("+proj=longlat +datum=WGS84"))
gg_Macoma <- fortify(Macoma)


Islands <- readShapeSpatial("Maps/Islands/Islands.shp", proj4string = 
                             CRS("+proj=longlat +datum=WGS84"))
gg_Islands <- fortify(Islands)




Mussel_beds <- readShapeSpatial("Maps/Mytilus/Mussel_beds_points.shp", proj4string = 
                              CRS("+proj=longlat +datum=WGS84"))

gg_Mussel_beds <- data.frame(
  long = coordinates(Mussel_beds)[, 1],
  lat = coordinates(Mussel_beds)[, 2])


farvater <- read_excel("Data/Farvater_coordinates.xlsx", sheet = "Farvater")
otval <- read_excel("Data/Farvater_coordinates.xlsx", sheet = "Otval")


Kand_x <- c(32.5, 32.65)
Kand_y <- c(66.96, 67.06)

library(ggpattern)


ggplot() + 
  geom_polygon(data = gg_Laminaria, aes(x = long, y = lat, group = group), fill = "green") +
  geom_polygon(data = gg_Portlandia, aes(x = long, y = lat, group = group), fill = "yellow") +
  geom_polygon(data = gg_Rhodophyta, aes(x = long, y = lat, group = group), fill = "red") +
  geom_polygon(data = gg_Rhynchonella, aes(x = long, y = lat, group = group), fill = "blue") +
  geom_polygon(data = gg_Hydralmania, aes(x = long, y = lat, group = group), fill = "blue") +
  geom_polygon(data = gg_Macoma, aes(x = long, y = lat, group = group), fill = "black") +
  geom_polygon(data = gg_Pista, aes(x = long, y = lat, group = group), fill = "black") +
    geom_polygon(data = gg_Portlandia_Macoma, aes(x = long, y = lat, group = group), fill = "black") +
  geom_polygon(data = gg_Mud, aes(x = long, y = lat, group = group),
                       fill = "black",
                       alpha = 0.2) +
  geom_polygon(data = Islands, aes(x = long, y = lat, group = group), fill = "gray") +
  geom_polygon(data = gg_murm_karel, aes(x = long, y = lat, group = group), fill = "gray", color = "black") +
  geom_polygon(data = gg_Polydora, aes(x = long, y = lat, group = group), fill = "black") +
  coord_map(xlim = Kand_x, ylim = Kand_y) +
  theme_bw() 
# +
#   geom_polygon(data = farvater,  aes(x = long, y = lat), fill = "brown")
  


# Карта распределеиня мидиевых банок
ggplot() + 
  geom_polygon(data = Islands, aes(x = long, y = lat, group = group), fill = "gray") +
  geom_polygon(data = gg_murm_karel, aes(x = long, y = lat, group = group), fill = "gray", color = "black") +
  geom_point(data = gg_Mussel_beds, aes(x = long, y = lat, group = 1), fill = "black", size = 3) +
  coord_map(xlim = Kand_x, ylim = Kand_y) +
  theme_bw() 


# Карта распределеиня зарослей ламинарии

ggplot() + 
  geom_polygon(data = Islands, aes(x = long, y = lat, group = group), fill = "gray") +
  geom_polygon(data = gg_Laminaria, aes(x = long, y = lat, group = group), fill = "green") +
  geom_polygon(data = gg_murm_karel, aes(x = long, y = lat, group = group), fill = "gray", color = "black") +
  geom_polygon(data = farvater, aes(x = long, y = lat, group = 1), fill = "brown") +
  coord_map(xlim = Kand_x, ylim = Kand_y) +
  theme_bw() 



# Карта распределеиня сообществ мелководных илов

ggplot() + 
  geom_polygon(data = Islands, aes(x = long, y = lat, group = group), fill = "gray") +
  geom_polygon(data = gg_Macoma, aes(x = long, y = lat, group = group), fill = "black") +
  geom_polygon(data = gg_Pista, aes(x = long, y = lat, group = group), fill = "black") +
  geom_polygon(data = gg_Portlandia_Macoma, aes(x = long, y = lat, group = group), fill = "black") +
  geom_polygon(data = gg_Polydora, aes(x = long, y = lat, group = group), fill = "black") +
  geom_polygon(data = gg_Mud, aes(x = long, y = lat, group = group),
               fill = "black") +
  geom_polygon(data = gg_murm_karel, aes(x = long, y = lat, group = group), fill = "gray", color = "black") +
  geom_polygon(data = farvater, aes(x = long, y = lat, group = 1), fill = "brown", alpha = 0.5) +
  coord_map(xlim = Kand_x, ylim = Kand_y) +
  theme_bw() 


# Карта распределеиня сообществ глубоководных илов

ggplot() + 
  geom_polygon(data = gg_Portlandia, aes(x = long, y = lat, group = group), fill = "yellow") +
  geom_polygon(data = gg_murm_karel, aes(x = long, y = lat, group = group), fill = "gray", color = "black") +
  geom_polygon(data = farvater, aes(x = long, y = lat, group = 1), fill = "brown", alpha = 0.5) +
  coord_map(xlim = Kand_x, ylim = Kand_y) +
  theme_bw() 



# Карта распределеиня зарослей багрянок

ggplot() + 
  geom_polygon(data = gg_Rhynchonella, aes(x = long, y = lat, group = group), fill = "blue") +
  geom_polygon(data = gg_Hydralmania, aes(x = long, y = lat, group = group), fill = "blue") +
  geom_polygon(data = gg_murm_karel, aes(x = long, y = lat, group = group), fill = "gray", color = "black") +
  geom_polygon(data = farvater, aes(x = long, y = lat, group = 1), fill = "brown", alpha = 0.5) +
  coord_map(xlim = Kand_x, ylim = Kand_y) +
  theme_bw() 



# Карта распределеиня сообщества промытого гравийно-галичного грунта

ggplot() + 
  geom_polygon(data = gg_Rhodophyta, aes(x = long, y = lat, group = group), fill = "red") +
  geom_polygon(data = gg_murm_karel, aes(x = long, y = lat, group = group), fill = "gray", color = "black") +
  geom_polygon(data = farvater, aes(x = long, y = lat, group = 1), fill = "brown", alpha = 0.5) +
  coord_map(xlim = Kand_x, ylim = Kand_y) +
  theme_bw() 


########################################


# Карта границ заповедника

reserve_boundary <- read.csv("Maps/Nature_reserve_boundary/Вершина_залива_координаты_положение.csv")


farvater <- read_excel("Data/Farvater_coordinates.xlsx", sheet = "Farvater")
otval <- read_excel("Data/Farvater_coordinates.xlsx", sheet = "Otval")

Kand_map +
  geom_polygon(data = reserve_boundary, aes(x = Y, y = X, group = Part), color = "blue", fill= NA) +
  geom_polygon(data = farvater, aes(x = long, y = lat, group = 1), fill = "brown") +
  geom_polygon(data = otval, aes(x = long, y = lat, group = 1), fill = "brown")
  



# Карта располодения станций мониторинга в раоне фарватера

monitor_samples <- read_excel("Data/ZRS_Monitoring_Samples.xlsx")

Kand_map +
  geom_point(data = monitor_samples, aes(fill = Source, group = 1), size = 3, shape = 21) +
  scale_fill_manual(values = c("blue", "yellow")) + 
  geom_polygon(data = farvater, aes(x = long, y = lat, group = 1), fill = "brown", alpha = 0.5) +
  geom_polygon(data = otval, aes(x = long, y = lat, group = 1), fill = "brown", alpha = 0.5) 
  

