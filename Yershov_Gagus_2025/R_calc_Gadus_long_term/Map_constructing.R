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
karel_shape <- st_read("d:/Data_LMBE/Maps/Karelia/boundary-polygon-land-lvl4.shp")

# karel_shape <- st_read("Maps/Karelia/boundary-polygon-land-lvl4.shp")

# arkhangel_shape <- st_read("Maps/Arkhangelskaya_obl/data/boundary-polygon-land-lvl2.gpkg")




Lon_limits <- c(33.57, 33.76)
Lat_limits <- c(66.30, 66.36)


karel_shape_cropped <- st_crop(karel_shape,
                               xmin = Lon_limits[1],
                               xmax = Lon_limits[2],
                               ymin = Lat_limits[1],
                               ymax = Lat_limits[2])


Krivoe_lake <- st_read("Maps/Krivoe_ozero.kml")
Catchment_area <- st_read("Maps/Cod_catchment_area.kml")

library(ggpattern)
library(ggspatial)


Pl_BBS_area <-
  ggplot() +
  geom_sf_pattern(data = Catchment_area,
                  pattern = "stripe",
                  pattern_angle = 45,
                  pattern_density = 0.3,
                  pattern_spacing = 0.01,
                  pattern_size = 0.02,
                  fill = NA,
                  pattern_color = "blue",
                  color = "white") +
  geom_sf(data = karel_shape_cropped,fill = "gray50", color = "black") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "white")) +
  geom_sf(data = Krivoe_lake, fill = "white") +
  scale_x_continuous(breaks = c(33.6, 33.7)) +
  scale_y_continuous(breaks = c(66.3, 66.35))







Fig_1_c <-
Pl_BBS_area +
  annotate(x = 33.62, y = 66.325, geom = "text", label = "Chupa inlet", size = 7) +
  annotate(x = 33.7, y = 66.358, geom = "text", label = "Kandalaksha \nbay", size = 7) +
  annotation_scale(location = "bl",    # расположение: bl = bottom left, br = bottom right
                   width_hint = 0.25,  # ширина линейки относительно графика
                   style = "bar",      # стиль: "bar" или "ticks"
                   text_cex = 0.8,     # размер текста
                   text_col = "black", # цвет текста
                   line_col = "black", # цвет линий
                   pad_x = unit(0.65, "cm"), # отступ по X
                   pad_y = unit(0.2, "cm"))


library(rnaturalearth)
library(rnaturalearthdata)


world <- ne_countries(scale = "medium", returnclass = "sf")

# Фильтрация стран Европы
europe <- subset(world, continent %in% c("Europe"))

# Построение карты СЗ Евразии
Fig_1_a <-
  ggplot(data = europe) +
  geom_sf(fill = "gray90", color = "gray30") +
  coord_sf(
    xlim = c(2, 53),   # Долгота
    ylim = c(52, 72)     # Широта
  ) +
  theme_bw() +
  theme(panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) +
  annotate("rect",
           xmin = 30.5, xmax = 45,
           ymin = 63.6, ymax = 68,
           fill = NA, alpha = 0.3, color = "red", linewidth = 1
  )


world <- ne_countries(scale = "large", returnclass = "sf")  # "large" = 1:10m (наиболее детально)

# lakes <- ne_download(scale = "large", type = "lakes", category = "physical", returnclass = "sf")



Fig_1_b <-
  ggplot() +
  # Суша
  geom_sf(data = world, fill = "gray90", color = "gray30", linewidth = 0.3) +
  coord_sf(
    xlim = c(31.5, 45),   # Долгота
    ylim = c(63.6, 68)     # Широта
  ) +
  theme_bw() +
  theme(panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())  +
  # annotate("text", x = 38, y = 65.9, label = "White Sea",
  #          size = 4.5, fontface = "bold", color = "black") +
  # annotate("text", x = 34.9, y = 66.33, label = "Kandalaksha bay",
  #          size = 4, angle = -32) +
  annotate("rect",
           xmin = 33.57, xmax = 33.76,
           ymin = 66.30, ymax = 66.36,
           fill = NA, color = "red", linewidth = 0.5)
  # Подпись "Chupa inlet"
  # annotate("text", x = 33, y = 66.25, label = "Chupa inlet",
  #          size = 3, fontface = "italic", color = "red")

library(patchwork)
Fig_1 <-
(Fig_1_a/Fig_1_b) | Fig_1_c


ggsave(filename = "Figures/Fig_1_a.tif", plot = Fig_1_a, dpi = 1200)
ggsave(filename = "Figures/Fig_1_b.tif", plot = Fig_1_b, dpi = 1200)
ggsave(filename = "Figures/Fig_1_c.tif", plot = Fig_1_c, dpi = 1200)
