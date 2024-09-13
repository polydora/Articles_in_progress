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

proj4string(Kand_shape)
proj4string(Laminaria)


plot(Kand_shape)


# Карта для ggplot
gg_murm_karel <- fortify(Kand_shape)



Kand_map <- 
  ggplot(gg_murm_karel, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "gray20") +
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




Kand_x <- c(32.5, 32.65)
Kand_y <- c(66.96, 67.06)

library(ggpattern)


ggplot() + 
  geom_polygon(data = gg_Laminaria, aes(x = long, y = lat, group = group), fill = "green") +
  geom_polygon(data = gg_Portlandia, aes(x = long, y = lat, group = group), fill = "yellow") +
  geom_polygon(data = gg_Rhodophyta, aes(x = long, y = lat, group = group), fill = "red") +
  geom_polygon(data = gg_Rhynchonella, aes(x = long, y = lat, group = group), fill = "cyan") +
  geom_polygon(data = gg_Hydralmania, aes(x = long, y = lat, group = group), fill = "lightblue") +
  geom_polygon(data = gg_Macoma, aes(x = long, y = lat, group = group), fill = "sienna") +
  geom_polygon(data = gg_Pista, aes(x = long, y = lat, group = group), fill = "orange") +
    geom_polygon(data = gg_Portlandia_Macoma, aes(x = long, y = lat, group = group), fill = "black") +
  geom_polygon(data = gg_Mud, aes(x = long, y = lat, group = group),
                       fill = "black",
                       alpha = 0.2) +
  geom_polygon(data = Islands, aes(x = long, y = lat, group = group), fill = "gray") +
  geom_polygon(data = gg_murm_karel, aes(x = long, y = lat, group = group), fill = "gray", color = "black") +
  geom_polygon(data = gg_Polydora, aes(x = long, y = lat, group = group), fill = "gray40") +
  coord_map(xlim = Kand_x, ylim = Kand_y) +
  theme_bw()
  


########################################


# Создаем карту по обычным лекалам


# Fin_x <- c(27.29, 31)
# Fin_y <- c(59.20, 60.13)

Fin_x <- c(28.06, 29.9)
Fin_y <- c(59.61, 60.11)

#
gshhs.l.b <- "d:/Data_LMBE/Maps/Gshhs/gshhs_l.b"

gshhs.f.b <- "d:/Data_LMBE/Maps/Gshhs/gshhs_f.b"
wdb_rivers.f.b <- "d:/Data_LMBE/Maps/Gshhs/wdb_rivers_f.b"

wdb_borders.f.b <- "d:/Data_LMBE/Maps/Gshhs/wdb_borders_f.b"

wdb_rivers.shp <- "D:/Data_LMBE/Maps/GSHHS/WDBII_shp/f/WDBII_river_f_L05.shp"

wdb_borders.f.b <- "d:/Data_LMBE/Maps/Gshhs/wdb_borders_f.b"
#
#
Fin_f <- getRgshhsMap(fn = gshhs.f.b, xlim = Fin_x, ylim = Fin_y)
plot(Fin_f)

# Rivers <- Rgshhs(wdb_rivers.f.b, xlim = Fin_x, ylim = Fin_y) 


Rivers <- importGSHHS(wdb_rivers.f.b, xlim = Fin_x, ylim = Fin_y)

str(Rivers)

as.data.frame(unlist(unclass(Rivers)))

Borders <- Rgshhs(wdb_borders.f.b, xlim = Fin_x, ylim = Fin_y)

#
# Fin_df <- fortify(Fin_f)
#
# write.csv(Fin_df, file = "Data/Finnish_South_2023_plan.csv")



#
# map <- esp_get_prov("Asturias") %>%
#   st_transform(27572) %>%
#   st_bbox()




map <- st_as_sfc(Fin_f)
plot(map)

rivers <- st_read(dsn =  wdb_rivers.shp)

st_as_sfc(rivers, xmin = Fin_x[1], xmax = Fin_x[2])


plot(rivers)

save(map, file = "Finnish_gulf_South.RData")


# Pl_Finnish_Gulf_1 <-
# ggplot(map) +
#   geom_sf()



Fin_df <- read.csv("Data/Finnish_Gulf_East.csv") # Map poligones

# Basic map layer####################

theme_set(theme_bw())



Pl_Finnish_Gulf <- 
  ggplot(Fin_df, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill = "gray90", colour = "gray20") + 
  theme(axis.title.x = element_blank(),  axis.title.y = element_blank(), plot.background = element_blank(), panel.border = element_blank(), panel.grid = element_blank()) + 
  theme(axis.text.x =element_blank(), axis.text.y= element_blank()) + 
  theme(axis.ticks = element_blank())

Pl_Finnish_Gulf + geom_path(data = Rivers, aes(x = X, y = Y, group = PID))




# Data reading

stations <- read_excel("Data/Finnish_gulf_working_data.xlsx", sheet = "Station parameters")

points <- stations %>% select(lat, long) %>% filter(long >= 27.054848)


Pl_Finnish_Gulf + 
  geom_point(data = points, aes(group = 1), color = "blue", size = 2)



points_sf <- st_as_sf(points, coords = c("long", "lat"), crs = 4326 )

# crs - система отсчета координат см https://spatialreference.org/
# st_crs(Fin_f)

point_in_pol <- as.numeric(st_within(points_sf, map))
str(point_in_pol)


# point_in_pol <- as.vector(st_intersects(map, points_sf, sparse = FALSE))
# Там еще много функций на сравнение полигонов

stations_on_land <- stations %>% filter(point_in_pol == 1)

Pl_Finnish_Gulf + 
  geom_point(data = stations_on_land, aes(group = 1), color = "blue", size = 2)

