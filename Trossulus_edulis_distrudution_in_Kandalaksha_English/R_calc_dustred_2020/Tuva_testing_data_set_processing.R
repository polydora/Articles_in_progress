# В этом скрипте делается попытка оценить приложимось модели, построенной для Белого моря к бареневоморским данным


# Создание карты

library(lattice)
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

Tuva_x <- c(33.4,  33.65)

Tuva_y <- c(69.17, 69.23)


Tuva_x_small <- c(33.56,  33.65)

Tuva_y_small <- c(69.17, 69.21)


# read shape file into R
murm_shape <- readShapeSpatial("Maps/boundary-polygon-land-lvl4.shp", proj4string = CRS("+proj=longlat +datum=WGS84"))


gg_murm <- fortify(murm_shape)


Tuva_map <- 
ggplot(gg_murm, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "gray20") +
  # coord_map(xlim = c(150., 151.52), ylim = c(59.45, 59.8) )+
  coord_map(xlim = Tuva_x_small, ylim = Tuva_y_small) +
  theme_bw()





## Данные по поселениям мидий

points <- read_excel("data/TuMyt_2009_2010_for_SDM.xlsx")
points$long <- as.numeric(points$Lon)
points$lat <- as.numeric(points$Lat)

 points$long_corrected <- as.numeric(points$Lon_corrected)
 points$lat_corrected <- as.numeric(points$Lat_corrected)

# Создем датафрейм с координатами точек 
fetch.df = data.frame(
  lon = points$long_corrected, 
  lat = points$lat_corrected,
  Site = points$Sample_ID)


fetch_locs = SpatialPoints(fetch.df[, 1:2], CRS(proj4string(murm_shape)))



fetch <- fetch_len_multi(pts = fetch_locs, bearings = c(0, 45, 90, 135, 180, 225, 270 ), shoreline = murm_shape,  dmax = 100000, spread = 0,  method = "btree", projected = FALSE)

# cbind(points, fetch ) %>% select(Sample_ID, Site, Lat, Lon, "0" )

write.table(data.frame(AverageFetch = apply(fetch, MARGIN = 1, FUN =  mean)/1000), "clipboard", sep = "\t" , dec = ",")



fetch.df$fetch <- as.data.frame(fetch) %>% rowMeans()/1000 

fetch.df$fetch <- round(fetch.df$fetch, 1)

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
  




