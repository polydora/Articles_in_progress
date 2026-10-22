# В этом скрипте обрабатываются данные по распростанению BTN в Кольском заливе

library(readxl)
library(dplyr)

library(sp)
library(mgcv)
library(reshape2)
library(ggplot2)
library(ggmap)
library(mapproj)
library(maps)
library(mapdata)
library(PBSmapping)
library(car)
library(waver)


# read shape file into R

murm_shape <- st_read("Data/Maps/Murmanskaya_obl/boundary-polygon-land-lvl4.shp")

# Установка системы координат если нужно
murm_shape <- st_set_crs(murm_shape, 4326) # WGS84



# Данные сайтов

kola_site <- read_excel("Data/Kola_site_size_structure.xlsx", na = "NA")


points <- 
  kola_site %>% 
  rename(Lat = Latitude, Lon = Longitude) %>% 
  select(Site_code, Date, Lat, Lon) 

fetch.df = data.frame(
  lon = points$Lon, 
  lat = points$Lat,
  Site = points$Site_code)

# Корректировка координат

fetch.df$lon[5] <- 33.270836
fetch.df$lat[5] <- 69.064726


fetch.df$lon[7] <- 33.466392
fetch.df$lat[7] <- 69.082990


fetch.df$lon[8] <- 33.172963 
fetch.df$lat[8] <- 69.075116



fetch_locs = SpatialPoints(fetch.df[, 1:2])
str(fetch_locs)


fetch_locs <- st_as_sf(fetch_locs)


st_crs(fetch_locs)  # проверяем проекцию точек
st_crs(murm_shape)  # проверяем проекцию береговой линии

st_crs(fetch_locs) <- 4326  # WGS84
st_crs(murm_shape) <- 4326  # WGS84


library(waver)

fetch <- 
  fetch_len_multi(pts = fetch_locs, bearings = c(0, 45, 90, 135, 180, 225, 270, 215), shoreline = murm_shape,  dmax = 100000, spread = 0,  method = "btree", projected = FALSE)


fetch <- 
  cbind(points, fetch ) 

fetch <-
  fetch %>% 
  mutate(Fetch =  rowMeans(select(., 5:12)))


fetch$Fetch <- fetch$Fetch/1000

kola_site <- 
merge(kola_site, fetch) %>% 
  select(-c( "Lat",  "Lon",  "0", "45", "90", "135", "180", "225", "270", "215"))

library(writexl)
write_xlsx(kola_site, "Data/sites_info_KolaBay_2026.xlsx")

