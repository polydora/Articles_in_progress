library(lattice)
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
library(gamm4)

library(akima)
library(car)
library(waver)
library(MuMIn)
library(readxl)

# Вычисляем fetch для точек сбора 2023 #################

Path <- "D:/Data_LMBE/Maps/"
gshhs.l.b <- paste(Path, "/GSHHS/gshhs_l.b", sep = "")

gshhs.f.b <- paste(Path, "/GSHHS/gshhs_f.b", sep = "")
wdb_rivers.f.b <- paste(Path, "/GSHHS/wdb_rivers_f.b", sep = "")
wdb_borders.f.b <- paste(Path, "/GSHHS/wdb_borders_f.b", sep = "")


# Задаем пределы координат для карт

Magadan_x <- c(149.8, 151.6)
Magadan_y <- c(59.3, 59.8)



# Читаем файлы с контурными картами

Magadan_map <- getRgshhsMap(fn = gshhs.f.b, xlim = Magadan_x, ylim = Magadan_y)

plot(Magadan_map)


gg_Magadan_large <- fortify(Magadan_map)

# gg_Magadan <- read.csv("Data/gg_Magadan_map.csv")





## Данные по поселениям мидий

points <- read_excel("Data/Magadan_2023.xlsx", sheet = "Точки взятия проб")
points$long <- as.numeric(points$E)
points$lat <- as.numeric(points$N)

points$long_corrected <- as.numeric(points$Lon_corrected)
points$lat_corrected <- as.numeric(points$Lat_corrected)

# Создем датафрейм с координатами точек 
fetch.df = data.frame(
  lon = points$long_corrected, 
  lat = points$lat_corrected,
  Site = points$ID_eng)


fetch_locs = SpatialPoints(fetch.df[, 1:2], 
                           CRS(proj4string(Magadan_map)))


      
fetch <- fetch_len_multi(pts = fetch_locs, bearings = c(0, 45, 90, 135, 180, 225, 270 ), shoreline = Magadan_map,  dmax = 100000, spread = 0,  method = "btree", projected = FALSE)


fetch.df$fetch <- as.data.frame(fetch) %>% rowMeans()/1000 
 
fetch.df$fetch <- round(fetch.df$fetch, 1)

ggplot(gg_Magadan_large, aes(x = long, y = lat, group = group)) + 
  geom_polygon() + 
  # coord_map(xlim = c(150., 151.52), ylim = c(59.4, 59.8) )+
  geom_point(data = fetch.df, aes(x = lon, y = lat, group = 1, size = (fetch) ), fill = "yellow", shape = 21) 
############################

# write.table(fetch.df, "clipboard",sep = "\t", dec = ",", row.names = F)

# fetch.df <- fetch.df[complete.cases(fetch.df),]
# nrow(fetch.df)
# 
# #########################
# # Проверка на принадлежность полигонам
# # Точки не должны лежать в пределах полигона суши
# 
# 
# ggplot(gg_Magadan_large, aes(x = long, y = lat, group = group)) + geom_polygon() + 
#   # coord_map(xlim = c(150., 151.52), ylim = c(59.45, 59.8) )+
#   coord_map(xlim = c(150.05, 150.1), ylim = c(59.67, 59.68) )+
#   geom_point(data = fetch.df[14,], aes(x = lon, y = lat,group = 1), color = "blue", size =2)
  
