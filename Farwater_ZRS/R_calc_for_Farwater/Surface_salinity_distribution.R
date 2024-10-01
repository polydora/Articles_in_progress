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

library(readxl)



# read shape file into R
murm_shape <- readShapeSpatial("Maps/Murmanskaya_obl/boundary-polygon-land-lvl4.shp", proj4string = CRS("+proj=longlat +datum=WGS84"))

karel_shape <- readShapeSpatial("Maps/Karelia/boundary-polygon-land-lvl4.shp", proj4string = CRS("+proj=longlat +datum=WGS84"))


# Объединение шейп-файлов
library(raster)

Kand_shape <- bind(murm_shape, karel_shape)


# Карта для ggplot
gg_murm_karel <- fortify(Kand_shape)


Kand_x <- c(32.25, 32.9)
Kand_y <- c(66.9, 67.17)


Kand_map <- 
  ggplot(gg_murm_karel, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "gray70") +
  # coord_map(xlim = c(150., 151.52), ylim = c(59.45, 59.8) )+
  coord_map(xlim = Kand_x, ylim = Kand_y) +
  theme_bw()

ggsave("Kand_map.svg", Kand_map,  dpi = 600, width = 20, units = "cm")



sal <- read_excel("Data/Surface_salinity_2007_2015_2024.xlsx")

sal$Year <- factor(sal$Year)

farvater <- read_excel("Data/Farvater_coordinates.xlsx", sheet = "Farvater")
otval <- read_excel("Data/Farvater_coordinates.xlsx", sheet = "Otval")


Kand_map_farwater <-
Kand_map +
  geom_polygon(data = farvater, aes(group = 1), fill = "brown") + 
  geom_polygon(data = otval, aes(group = 1), fill = "brown") +
  theme(axis.title = element_blank(), axis.text = element_blank())

ggsave("Kand_map_farwater.svg", Kand_map_farwater,  dpi = 600, width = 10, units = "cm")






Kand_map +
  geom_point(data = sal, aes(y = lat, x = long, group = 1), color = "blue") 


Pl_sal_stations <- 
ggplot(sal %>% filter(Year %in% c("2015", "2024")), aes(y = lat, x = long, group = 1))+
  geom_polygon(data = gg_murm_karel, aes(x = long, y = lat, group = group), fill = "gray70" , color = "black", linewidth = 0.1) +
  coord_map(xlim = Kand_x, ylim = Kand_y) +
  geom_polygon(data = farvater, fill = "brown") +
  geom_polygon(data = otval, fill = "brown") +
  geom_point(shape = 21, size = 1, fill = "yellow") +
  facet_wrap(~Year)+
  theme_bw() +
  labs(x = "Долгота", y = "Широта")


ggsave(plot = Pl_sal_stations, filename = "Salinity_stations.png", dpi = 600)



# Модель распределения солености

library(mgcv)

mod_sal <- gam(S ~ s(x = long, y = lat, by = Year) + Year, data = sal %>% filter(Year %in% c("2015", "2024"))) 

plot(mod_sal)

My_data <- expand.grid(Year = factor(c("2015", "2024")), long = seq(from = Kand_x[1], to = Kand_x[2] + 0.06, length.out = 100), lat = seq(from = Kand_y[1], to = Kand_y[2], length.out = 100))

My_data$Predict <- predict(mod_sal, newdata = My_data)

Pl_sal_2015 <-
ggplot(data = My_data %>% filter(Year == "2015"), aes(x = long, y = lat)) +
  geom_tile(aes(fill = Predict)) +
  scale_fill_gradient(low = "yellow", high = "blue") +
  geom_polygon(data = gg_murm_karel, aes(x = long, y = lat, group = group), fill = "gray70", color = "black", linewidth = 0.1) +
  geom_polygon(data = farvater, fill = "brown") +
  geom_polygon(data = otval, fill = "brown") +
  coord_map(xlim = Kand_x, ylim = Kand_y) +
  labs(x = "Долгота", y = "Широта", fill = "Соленость") +
  theme(legend.position = "bottom") 


Pl_sal_2024 <-
  ggplot(data = My_data %>% filter(Year == "2024"), aes(x = long, y = lat)) +
  geom_tile(aes(fill = Predict)) +
  scale_fill_gradient(low = "yellow", high = "blue") +
  geom_polygon(data = gg_murm_karel, aes(x = long, y = lat, group = group), fill = "gray70", color = "black", linewidth = 0.1) +
  geom_polygon(data = farvater, fill = "brown") +
  geom_polygon(data = otval, fill = "brown") +
  coord_map(xlim = Kand_x, ylim = Kand_y) +
  labs(x = "Долгота", y = "Широта", fill = "Соленость") +
  theme(legend.position = "bottom")

ggsave(plot = Pl_sal_2015, filename = "Salinity_map_2015.png", dpi = 600)

ggsave(plot = Pl_sal_2024, filename = "Salinity_map_2024.png", dpi = 600)



####################
# Вертикальное распределине солености


sal_depth <- read_excel("Data/Salinity_vs_Depth_Bouy_19.xlsx")

Pl_slinity_depth <- 
ggplot(sal %>% filter(Year == "2007"), aes(x = Depth, y = S, group = Depth)) +
  geom_point(position = position_jitter(width = 0.3), color = "blue")+
  geom_boxplot(fill = "gray", alpha = 0.7) +
  theme_bw() +
  labs(x = "Горизонт глубины", y = "Соленость (промилле)")
  
Pl_slinity_depth <-
Pl_slinity_depth +
  geom_point(data = sal_depth, aes(x = Depth, y = Salinity), size = 4, color = "red")

ggsave(plot = Pl_slinity_depth, filename = "Salinity_Depth_2007.png", dpi = 600)

