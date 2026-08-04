# В этом скрипте делается попытка оценить приложимось модели, построенной для Белого моря к баренцевоморским данным


# Создание карты
library(sf)

library(sp)
library(dplyr)
library(mgcv)
library(reshape2)
library(ggplot2)
library(ggmap)
library(mapproj)
library(maps)

library(mapdata)
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

# Задаем пределы координат для карт

# Tuva_x <- c(33.4,  33.65)
# 
# Tuva_y <- c(69.17, 69.23)
# 
# 
# Tuva_x_small <- c(33.56,  33.65)
# 
# Tuva_y_small <- c(69.17, 69.21)



# Kand_x <- c(32, 36.2)
# Kand_y <- c(65.6, 67.25)



# read shape file into R
murm_shape <- st_read("d:/Text/Article/Trossulus_edulis_distrudution_in_Kandalaksha_English_2025/R_calc_dustred_2020/Maps/Murmanskaya_obl/boundary-polygon-land-lvl4.shp")

karel_shape <- st_read("d:/Text/Article/Trossulus_edulis_distrudution_in_Kandalaksha_English_2025/R_calc_dustred_2020/Maps/Karelia/boundary-polygon-land-lvl4.shp")


points <- read_excel("Data/substrates for VM2.xlsx", na = "NA")

points <- 
  points %>% 
  filter_out(is.na(latitude)) %>% 
  rename(Lat = latitude, Lon = longitude, Site = site)

# myt_full <- read_excel("data/myt_full.xls")




points$long <- as.numeric(points$Lon)
points$lat <- as.numeric(points$Lat)

 points$long_corrected <- points$long
 points$lat_corrected <- points$lat

# Создем датафрейм с координатами точек 
fetch.df = data.frame(
  lon = points$long_corrected, 
  lat = points$lat_corrected,
  Site = points$Site)


fetch_locs <- st_as_sf(fetch.df, coords = c(1, 2), crs = st_crs(murm_shape))




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

anomal_x <- c(32.49, 32.51)
anomal_y <- c(66.89, 66.9)


anomal_sampl <- data.frame(long = 32.50190, lat = 66.896)

ggplot(gg_murm_karel, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "gray20") +
  coord_map(xlim = anomal_x, ylim = anomal_y) +
  theme_bw() +
  geom_point(data = anomal_sampl, aes(x = long, y = lat, group = 1), shape = 21, color = "blue", fill = "yellow")

#######






fetch <-
fetch %>% 
  mutate(Fetch =  rowMeans(select(., 8:15)))
  
fetch$Fetch <- fetch$Fetch/1000

df_fetch <- 
myt_site %>% 
  select(Site, Average_Fetch) %>% 
  merge(., fetch) %>% 
  select(Site, Average_Fetch, Lat, Lon, Fetch)

ggplot(df_fetch_murmansk, aes(Average_Fetch, Fetch)) +
  geom_point() + geom_abline()

merge(myt_full, df_fetch)






