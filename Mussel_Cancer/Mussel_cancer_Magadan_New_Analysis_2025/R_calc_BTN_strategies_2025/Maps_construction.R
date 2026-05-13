
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)

# Карта дальнего востока

Full_x <- c(3, 50)
Full_y <- c(55, 72)

# Скачиваем береговую линию мира с низким разрешением (аналог gshhs_l.b)
# scale = "large" для высокого разрешения, "medium" для среднего, "small" для низкого [citation:6]
coastline <- ne_coastline(scale = "medium", returnclass = "sf")

land <- ne_countries(scale = "medium", returnclass = "sf")



# Обрезаем по нужной области (аналог xlim/ylim в getRgshhsMap)
coastline_clipped <- st_crop(coastline, xmin = Full_x[1], xmax = Full_x[2], 
                             ymin = Full_y[1], ymax = Full_y[2])


# Обрезаем по нужной области
land_clipped <- st_crop(land, xmin = Full_x[1], xmax = Full_x[2], 
                        ymin = Full_y[1], ymax = Full_y[2])




ggplot() +
  geom_sf(data = land_clipped, fill = "lightgray", color = "black") +
  # geom_sf(data = coastline_clipped, fill = "lightgray", color = "black") +
  coord_sf(xlim = Full_x, ylim = Full_y, expand = FALSE) +
  theme_bw()





# Это вся карта мира в высоком разрешении. ЧИТАЕТ ДОЛГО!
gshhs_f_shp <- "Maps/GSHHS_f_L1.shp"
coastline_full <- st_read(gshhs_f_shp)








# Обрезаем по нужной области
Full_x <- c(3, 50)
Full_y <- c(55, 72)


Small_x <- c(3, 50)
Small_y <- c(55, 72)



Этот код читает shp файлы. Аt! для чтения нужно, чтобы в той же папке лежали .dbf, 
coastline_clipped <- st_crop(coastline_full, 
                             xmin = Full_x[1], xmax = Full_x[2],
                             ymin = Full_y[1], ymax = Full_y[2])


ggplot() +
  geom_sf(data = coastline_clipped, fill = "lightgray", color = "black") +
  coord_sf(xlim = Full_x, ylim = Full_y, expand = FALSE) +
  theme_bw()




