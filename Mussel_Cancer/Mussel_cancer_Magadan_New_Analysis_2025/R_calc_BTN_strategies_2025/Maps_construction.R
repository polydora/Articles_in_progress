
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(ggmap)
library(cowplot)

# Карта дальнего востока

Full_x <- c(141.5, -126.4)
Full_y <- c(45.6, 71.8)



library(ggplot2)
library(sf)
library(dplyr)

# Загружаем данные из пакета maps (более детальные с mapdata)
library(maps)
library(mapdata)

# Получаем данные мира
world_map <- map_data("world2Hires")  # world2Hires имеет долготы 0-360
# или используем world2 (менее детальный)

# Фильтруем по нужным континентам/регионам
asia_namerica <- world_map  %>%
  filter(region %in% c("USA", "Canada", "USSR", "China", "Japan", "North Korea", "South Korea","Alaska"))



# Создаем карту
Pl_map_large_scale <-
  ggplot() +
  geom_polygon(data = asia_namerica, 
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = "gray30", size = 0.2) +
  coord_fixed(xlim = c(145, 225),  # 60°E до 240°E (что соответствует 120°W)
              ylim = c(45, 71.8),
              ratio = 2) +
  theme_map() +
  theme(panel.grid.major = element_line(color = "gray80", size = 0.2),
        panel.background = element_rect(fill = "white", color = NA),
        panel.border = element_rect(fill = NA, color = "black", size = 0.5),
        panel.grid = element_blank())

ggsave(filename = "figures/Map_large_no_grids.png", plot = Pl_map_large_scale, dpi = 600)






######## Карта Магаданской области ################

load("Data/gg_Magadan_large.RData")

Magadan <- data.frame(long = 150 + 48/60, lat = 59 + 34/60)

Pl_map <- 
  ggplot(gg_Magadan_large, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "gray80", color = "gray50") + 
  coord_map(xlim = c(150., 151.52), ylim = c(59.4, 59.8) ) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  theme_map() 

ggsave(filename = "figures/Magadan.png", plot = Pl_map, dpi = 600)

ggsave(filename = "figures/Magadan.svg", plot = Pl_map, dpi = 600)

