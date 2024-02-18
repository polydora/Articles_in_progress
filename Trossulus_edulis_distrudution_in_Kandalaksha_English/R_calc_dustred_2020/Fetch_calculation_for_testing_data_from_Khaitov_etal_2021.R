# В этом скрипте делается попытка оценить приложимось модели, построенной для Белого моря к баренцевоморским данным


# Создание карты

library(sp)
library(dplyr)
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

library(akima)
library(car)
library(waver)
library(readxl)

# Вычисляем fetch для точек сбора вошедших в статью Khaitov et al 2021 #################



## Данные по поселениям мидий

myt_full <- read_excel("data/Testing_data_from_Khaitov_et_al_2021.xlsx")



# detach("package:raster", unload = TRUE)
# detach("package:dplyr", unload = TRUE)
# 
# library(dplyr)

myt_site <- myt_full %>% 
  filter(Sea == "Barents Sea") %>%
  filter(! Region %in% c("Tyuva inlet"))




# read shape file into R
Murm_shape <- readShapeSpatial("Maps/Murmanskaya_obl/boundary-polygon-land-lvl4.shp", proj4string = CRS("+proj=longlat +datum=WGS84"))

# karel_shape <- readShapeSpatial("Maps/Karelia/boundary-polygon-land-lvl4.shp", proj4string = CRS("+proj=longlat +datum=WGS84"))


# Объединение шейп-файлов
# library(raster)

# Kand_shape <- bind(murm_shape, karel_shape)

plot(Murm_shape)

# Карта для ggplot
gg_murm <- fortify(Murm_shape)



# Задаем пределы координат для карт


Murm_x <- c(min(myt_site$Lon)-0.15, max(myt_site$Lon)+0.15)
Murm_y <- c(min(myt_site$Lat)-0.1, max(myt_site$Lat)+0.1)




# Tuva_map <- 
# ggplot(gg_murm, aes(x = long, y = lat, group = group)) + 
#   geom_polygon(fill = "gray20") +
#   # coord_map(xlim = c(150., 151.52), ylim = c(59.45, 59.8) )+
#   coord_map(xlim = Tuva_x_small, ylim = Tuva_y_small) +
#   theme_bw()

Murm_map <- 
  ggplot(gg_murm, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "gray20") +
  # coord_map(xlim = c(150., 151.52), ylim = c(59.45, 59.8) )+
  coord_map(xlim = Murm_x, ylim = Murm_y) +
  theme_bw()

Murm_map +
  geom_point(data = myt_site, aes(x = Lon, y = Lat, group = 1), shape = 21, size = 3, fill = "yellow")

#########################################

points <- myt_site %>% select(Site, Lat, Lon) 


points$long <- as.numeric(points$Lon)
points$lat <- as.numeric(points$Lat)

 points$long_corrected <- points$long
 points$lat_corrected <- points$lat

# Создем датафрейм с координатами точек 
fetch.df = data.frame(
  lon = points$long_corrected, 
  lat = points$lat_corrected,
  Site = points$Site)


fetch_locs = SpatialPoints(fetch.df[, 1:2], CRS(proj4string(murm_shape)))

# plot(fetch_locs)

library(waver)

fetch <- fetch_len_multi(pts = fetch_locs, bearings = c(0, 45, 90, 135, 180, 225, 270, 215), shoreline = murm_shape,  dmax = 100000, spread = 0,  method = "btree", projected = FALSE)


fetch <- 
  cbind(points, fetch ) 




################## Поиск аномальных точек для Fetch-анализа по новому шейп-файлу ##############

anomal_df <- 
  fetch %>%
  filter(!complete.cases(.))

Kand_map +
  geom_point(data = anomal_df, aes(group = 1), shape = 21, color = "blue", fill = "yellow", size = 3)

anomal_x <- c(36.05, 36.06)
anomal_y <- c(69.0825, 69.0875)

69.08560 36.05169


anomal_sampl <- data.frame(long = 36.05155, lat = 69.08560)

ggplot(gg_murm, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "gray20") +
  coord_map(xlim = anomal_x, ylim = anomal_y) +
  theme_bw() +
  geom_point(data = anomal_sampl, aes(x = long, y = lat, group = 1), shape = 21, color = "blue", fill = "yellow")

#######






fetch <-
fetch %>% 
  mutate(Fetch =  rowMeans(select(., 8:15)))
  
fetch$Fetch <- fetch$Fetch/1000


df_fetch <- fetch %>% select(Site, Lat, Lon, Fetch)

#########################

# Вычисление расстояния до ближайшего порта



ports <- data.frame(Shore = c("Kand", "Karel", "Kand", "Karel", "Karel", "Murman", "Murman", "Murman", "Murman"), Port_staus = c("Active", "Active", "Abandoned", "Abandoned", "Abandoned", "Active", "Active", "Active", "Abandoned"),Port = c("Kandalaksha", "Vitino", "Umba", "Chupa", "Sredny", "Severomorsk", "Murmansk", "Polyarny", "Graitny"), Lat = c(67.137283, 67.076570,  66.677970, 66.269964, 66.294178, 69.085054, 68.981067, 69.203571, 69.270334), Lon = c(32.407995, 32.333630, 34.357655, 33.069534, 33.640656, 33.414842, 33.053944, 33.457282, 33.819599))

ports <- 
  ports %>% 
  filter(Shore == "Murman")


nearest_dist <- function(XY, objects = river, x.name = "Lon", y.name = "Lat"){
  
  XY1 <-as.numeric(XY[,1])
  XY2 <- as.numeric(XY[,2])
  dist <- (acos(sin(XY1*pi/180)*sin(objects[ ,y.name] * pi /180) + cos(XY1*pi/180)*cos(objects[ ,y.name]*pi/180)*cos(XY2*pi/180 - objects[ ,x.name]*pi/180)) * 6371)
  
  MD <- data.frame(Min_dist = min(dist))
  cbind(objects[which(dist == min(dist)), ], MD)
  
}



df_port <- nearest_dist(XY = myt_site[1, c("Lat", "Lon")], objects = ports)

df_port[1,] <- NA

for(i in 1:nrow(myt_site)) {
  df_port[i,] <- nearest_dist(XY = myt_site[i, c("Lat", "Lon")], objects = ports)
  df_port$Site[i] <- as.character(myt_site$Site)[i]
}

write.table(df_port, "clipboard", sep = "\t", dec = ",")


##########################

################# Расстояние до ближайшей реки

rivers <- read_excel("data/rivers_BS.xlsx")

str(rivers)

rivers$Lat <- as.numeric(rivers$Lat)

rivers$Lon <- as.numeric(rivers$Lon)



df_river <- nearest_dist(XY = myt_site[1, c("Lat", "Lon")], objects = rivers)

df_river[1,] <- NA

for(i in 1:nrow(myt_site)) {
  df_river[i,] <- nearest_dist(XY = myt_site[i, c("Lat", "Lon")], objects = rivers)
  df_river$Site[i] <- as.character(myt_site$Site)[i]
}



################ Сводим все данные в один датафрейм

df_fetch <- 
  df_fetch %>% 
  select(Site, Fetch)
  

df_port <- 
  df_port %>%  
  select(Site, Port_staus, Port, Min_dist)

df_river <- 
  df_river %>% 
  select(Site, River, River_Size, Drainage_Area, Min_dist)

df1 <-
merge(df_fetch, df_port) 

df2 <-
merge(df1, df_river, by ="Site")



myt_site <- merge(myt_site, df2, by = "Site") 




# write.table(myt_site, "clipboard", sep = "\t", row.names = F, dec = ",")

myt_site %>% 
  filter(Region == "Kola Bay") %>% 
  ggplot(aes(x = Min_dist.x, y = Ptros)) +
  geom_point()



########################################
# Данные по солености из работы 
# Trofimova, V V (2009): (Table 13) Water salinity and temperature at stations along the east coast of the Kola Bay in 2005. Murmansk Marine Biological Institute, PANGAEA, https://doi.org/10.1594/PANGAEA.774263, 


trofimova_data <- read.table("data/Trofimova-2009_Tab13.tab", sep = "\t", skip = 19, header = T)



Murm_x <- c(min(trofimova_data$Longitude)-0.15, max(trofimova_data$Longitude)+0.3)
Murm_y <- c(min(trofimova_data$Latitude)-0.1, max(trofimova_data$Latitude)+0.30)


# Tuva_map <- 
# ggplot(gg_murm, aes(x = long, y = lat, group = group)) + 
#   geom_polygon(fill = "gray20") +
#   # coord_map(xlim = c(150., 151.52), ylim = c(59.45, 59.8) )+
#   coord_map(xlim = Tuva_x_small, ylim = Tuva_y_small) +
#   theme_bw()

Murm_map <- 
  ggplot(gg_murm, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "gray20") +
  # coord_map(xlim = c(150., 151.52), ylim = c(59.45, 59.8) )+
  coord_map(xlim = Murm_x, ylim = Murm_y) +
  theme_bw()

Murm_map +
  geom_point(data = myt_site, aes(x = Lon, y = Lat, group = 1), shape = 21, size = 3, fill = "yellow")



