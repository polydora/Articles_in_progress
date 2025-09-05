library(vegan)
library(dplyr)
library(broom)
library(readxl)
library(ggmap)
library(maps)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(reshape2)
library(scatterpie)



# Загрузка данных о странах мира
world <- ne_countries(scale = "medium", returnclass = "sf")

# Фильтрация стран Евразии по континенту
eurasia <- subset(world, continent %in% c("Europe", "Asia", "Africa"))

# Построение карты Евразии
Pl_map <- 
  ggplot(data = eurasia) +
  geom_sf(fill = "gray90", color = "gray30") +
  coord_sf(
    xlim = c(10, 110),   # Долгота: от Португалии до Японии
    ylim = c(0, 60)     # Широта: от Индонезии до России
  ) +
  theme_minimal()


Pl_map_local <- 
  ggplot(data = eurasia) +
  geom_sf(fill = "gray90", color = "gray30") +
  coord_sf(
    xlim = c(40, 90),   # Долгота: от Португалии до Японии
    ylim = c(35, 60)     # Широта: от Индонезии до России
  ) +
  theme_minimal()



range_data_1 <- st_read("Data/Bird_Range_Polygons/Phylloscopus collybita.kml")

range_data_1$Name = c("A", "B", "C", "D")


Pl_map +
  geom_sf(data = range_data_1, aes(fill = Name), alpha = 0.9) + 
  coord_sf(
    xlim = c(10, 110),   # Долгота: от Португалии до Японии
    ylim = c(0, 60)     # Широта: от Индонезии до России
  ) +
  ggtitle("Пеночка теньковка")



range_data_2 <- st_read("Data/Bird_Range_Polygons/Phyl_troch.kml")

range_data_2$Name = c("A", "B", "C")


Pl_map +
  geom_sf(data = range_data_2, aes(fill = Name), alpha = 0.9) + 
  coord_sf(
    xlim = c(10, 110),   # Долгота: от Португалии до Японии
    ylim = c(0, 60)     # Широта: от Индонезии до России
  ) +
  ggtitle("Зеленая пеночка")

