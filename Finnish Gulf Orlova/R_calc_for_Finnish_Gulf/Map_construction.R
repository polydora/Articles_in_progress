library(ggmap)
library(mapproj)
library(maps)
library(rgeos) #этот пакет содержит какую-то хрень, которая позволяет обойти проблему пр чтении фалов средсвами maptools. 
#Att! Этот пакет должен быть загружен до maptools
library(mapdata)
library(maptools) # Rgshhs
library(PBSmapping)
library(sf)


library(readxl)
library(dplyr)
library(cowplot)
library(png)
library(reshape2)

library(mgcv)
library(vegan)


# Создаем карту по обычным лекалам


Fin_x <- c(27.29, 31)
Fin_y <- c(59.20, 60.13)
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
# write.csv(Fin_df, file = "Data/Finnish_South.csv")



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

