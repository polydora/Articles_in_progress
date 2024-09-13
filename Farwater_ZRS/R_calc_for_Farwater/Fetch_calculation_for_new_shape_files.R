# В этом скрипте делается попытка оценить приложимось модели, построенной для Белого моря к баренцевоморским данным


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



Kand_x <- c(32, 36.2)
Kand_y <- c(65.6, 67.25)



# read shape file into R
murm_shape <- readShapeSpatial("Maps/Murmanskaya_obl/boundary-polygon-land-lvl4.shp", proj4string = CRS("+proj=longlat +datum=WGS84"))

karel_shape <- readShapeSpatial("Maps/Karelia/boundary-polygon-land-lvl4.shp", proj4string = CRS("+proj=longlat +datum=WGS84"))


# Объединение шейп-файлов
library(raster)

Kand_shape <- bind(murm_shape, karel_shape)

plot(Kand_shape)




# Карта для ggplot
gg_murm_karel <- fortify(Kand_shape)


# Tuva_map <- 
# ggplot(gg_murm, aes(x = long, y = lat, group = group)) + 
#   geom_polygon(fill = "gray20") +
#   # coord_map(xlim = c(150., 151.52), ylim = c(59.45, 59.8) )+
#   coord_map(xlim = Tuva_x_small, ylim = Tuva_y_small) +
#   theme_bw()

Kand_map <- 
  ggplot(gg_murm_karel, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "gray20") +
  # coord_map(xlim = c(150., 151.52), ylim = c(59.45, 59.8) )+
  coord_map(xlim = Kand_x, ylim = Kand_y) +
  theme_bw()

Kand_map


## Данные по поселениям мидий

# points <- read_excel("data/TuMyt_2009_2010_for_SDM.xlsx")

myt_full <- read_excel("data/myt_full.xls")



# detach("package:raster", unload = TRUE)
# detach("package:dplyr", unload = TRUE)
# 
# library(dplyr)

myt_site <- myt_full %>% 
  group_by(Site) %>% 
  select(Lat, Lon, N_T, N_E, Salinity, Min_dist_river, River, River_Size, Min_dist_river_Large, Min_dist_port, Port, Port_Status, Average_Fetch, Dist_cut) %>% 
  summarise(Lat = mean(Lat), Lon = mean(Lon), N_T = sum(N_T), N_E = sum(N_E), Salinity = mean(Salinity), Min_dist_river = mean(Min_dist_river), River = unique(River), River_Size = unique(River_Size), Min_dist_river_Large = mean(Min_dist_river_Large),  Min_dist_port = mean(Min_dist_port), Port = unique(Port), Port_Status = unique(Port_Status), Average_Fetch = mean(Average_Fetch),   Dist_cut = mean(Dist_cut)) %>% 
  mutate(Prop_T = N_T/(N_T+N_E))


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

fetch <- fetch_len_multi(pts = fetch_locs, bearings = c(0, 45, 90, 135, 180, 225, 270, 215), shoreline = Kand_shape,  dmax = 100000, spread = 0,  method = "btree", projected = FALSE)


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



write.table(merge(myt_full, df_fetch), "data/myt_full_2024.csv", sep = ";" , dec = ",", row.names = FALSE)



#########################

# Вычисление расстояния до ближайшего порта, за который взята точка с координатами (69.190018, 33.622952)


ports <- data.frame(Shore = c("Kand", "Karel", "Kand", "Karel", "Karel", "Murman"), Port = c("Кандалакша", "Витино", "Умба", "Чупа", "Средний", "Севеоморск"), Lat = c(67.137283, 67.076570,  66.677970, 66.269964, 66.294178, 69.085054), Lon = c(32.407995, 32.333630, 34.357655, 33.069534, 33.640656, 33.414842))




nearest_dist <- function(XY, objects = river, x.name = "Lon", y.name = "Lat"){
  
  XY1 <-as.numeric(XY[,1])
  XY2 <- as.numeric(XY[,2])
  dist <- (acos(sin(XY1*pi/180)*sin(objects[ ,y.name]*pi/180) + cos(XY1*pi/180)*cos(objects[ ,y.name]*pi/180)*cos(XY2*pi/180 - objects[ ,x.name]*pi/180)) * 6371)
  
  MD <- data.frame(Min_dist = min(dist))
  cbind(objects[which(dist == min(dist)), ], MD)
  
}


tuv <- read_excel("data/TuMyt_2009_2010_for_SDM.xlsx")


df_port <- nearest_dist(XY = tuv[1, c("Lat", "Lon")], objects = ports)

df_port[1,] <- NA

for(i in 1:nrow(tuv)) {
  df_port[i,] <- nearest_dist(XY = tuv[i, c("Lat", "Lon")], objects = ports)
  df_port$Site[i] <- as.character(tuv$Sample_ID)[i]
}

write.table(df_port, "clipboard", sep = "\t", dec = ",")


##########################




tuv <- read_excel("data/TuMyt_2009_2010_for_SDM.xlsx")




ggplot(tuv, aes(x = Average_Fetch, y = PT )) +
  geom_point() +
  geom_smooth()



ggplot(tuv, aes(x = Position, y = PT )) +
  geom_boxplot() 


ggplot(tuv, aes(x = Min_dist_river, y = PT )) +
  geom_point() + 
  geom_smooth()

ggplot(tuv, aes(x = Min_dist_port, y = PT )) +
  geom_point() + 
  geom_smooth()


tuv$Port_Status <- factor(tuv$Port_Status)
tuv$Position <- factor(tuv$Position)
tuv$River_Size <- factor(tuv$River_Size)

levels(tuv$Port_Status) <- levels(myt_full$Port_Status)
levels(tuv$River_Size) <- levels(myt_full$River_Size)
tuv$Min_dist_river <- tuv$Min_dist_river/1000



summary(Model_2.2)

predict(Model_2.2, newdata = tuv)

betas <- fixed.effects(Model_2.2)

X <- model.matrix(~ Position + Min_dist_river +  River_Size + Average_Fetch +  Min_dist_port + Port_Status, data = tuv)

predicted <- X %*% betas

model_prediction <- data.frame(Mod_2.2_pred = logit_back(predicted))


qplot(model_prediction$Mod_2.2_pred, tuv$PT)


Mod_gam_2 <- gam(Prop_T ~  s(Min_dist_river, bs = "cr") + s(Average_Fetch, bs = "cr") + s(Min_dist_port, bs = "cr") + Position + River_Size + Port_Status + s(Site, k = Total_site, bs = "re"), method = "REML", family = betar(link = "logit", eps = 0.000000001), data = myt_full )

summary(Mod_gam_2)



model_prediction <- data.frame(Mod_2.2_pred = logit_back(predict(Mod_gam_2, newdata = tuv)))


qplot(model_prediction$Mod_2.2_pred, tuv$PT) + geom_smooth(method = "lm", se =  F)
  




