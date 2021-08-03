library(ggplot2)
library(dplyr)
library(mgcv)

library(tidyr)
library(reshape2)


##### Data reading
myt <- read.table("data/Distred_samples_fetch_corrected_2018.csv", header = T, sep = ",")

sal <- read.table("data/Distred_samples_salinity_2018.csv", header = T, sep = ",")

myt <- merge(myt, sal, all = T)

river <- read.table("data/Rivers.csv", sep = ";", header = T)

ports <- data.frame(Shore = c("Kand", "Karel", "Kand", "Karel", "Karel"), Port = c("Кандалакша", "Витино", "Умба", "Чупа", "Средний"), Lat = c(67.137283, 67.076570,  66.677970, 66.269964, 66.294178), Lon = c(32.407995, 32.333630, 34.357655, 33.069534, 33.640656))


sites_fetch_df <- read.table("data/Distred_samples_fetch_values_2018.csv", sep = ",", header = T)


##### Связь уровня сброса рек с размером площади водосбора
ggplot(river, aes(x = Drainage_Are, y = Discharge )) + geom_point()

# Модель для вычисления уровня сброса рек в зависимости от Drainage_Are

Mod_river <- lm(Discharge ~ Drainage_Are, data = river)
summary(Mod_river)





######## Формируем датасет со всми предикторами

# Функция для вычисления от заданной точки до ближайшего объекта


nearest_dist <- function(XY, objects = river, x.name = "Lon", y.name = "Lat"){
  
  XY1 <-as.numeric(XY[,1])
  XY2 <- as.numeric(XY[,2])
  dist <- (acos(sin(XY1*pi/180)*sin(objects[ ,y.name]*pi/180) + cos(XY1*pi/180)*cos(objects[ ,y.name]*pi/180)*cos(XY2*pi/180 - objects[ ,x.name]*pi/180)) * 6371)
  
  MD <- data.frame(Min_dist = min(dist))
  cbind(objects[which(dist == min(dist)), ], MD)
  
}


df_river <- nearest_dist(XY = myt[1, c("Lat", "Lon")], objects = river)

df_river[1,] <- NA

for(i in 1:nrow(myt)) {
  df_river[i,] <- nearest_dist(XY = myt[i, c("Lat", "Lon")], objects = river)
  df_river$Site[i] <- as.character(myt$Site)[i]
}

names(df_river) <- c( "Shore_river", "River", "Discharge", "Drainage_Are", "Lat_river", "Lon_river", "Min_dist_river", "Site" )


df_river$Discharge_pred <- predict(Mod_river, newdata = df_river) 




# Находим расстояния для ближайшего порта.

df_port <- nearest_dist(XY = myt[1, c("Lat", "Lon")], objects = ports)

df_port[1,] <- NA

for(i in 1:nrow(myt)) {
  df_port[i,] <- nearest_dist(XY = myt[i, c("Lat", "Lon")], objects = ports)
  df_port$Site[i] <- as.character(myt$Site)[i]
}

names(df_port) <- c("Shore_port",  "Port", "Lat_port", "Lon_port", "Min_dist_port", "Site")






# Сводим все датафреймы в один

nrow(myt)
nrow(df_river)

d <- cbind(myt, df_river %>% select(-Site))

dd <- cbind(d, df_port %>% select(-Site))


ddd <- merge(dd, sites_fetch_df, by = "Site")

myt_full <- ddd

# Условная граница кута Кандалакшского залива
Shore_boundary = c(67.162360, 32.332371)

# Переводим в радианы
Shore_boundary <- Shore_boundary*pi/180


myt_full$Dist_cut <- with(myt_full, acos(sin(Shore_boundary[1])*sin(Lat*pi/180) + cos(Shore_boundary[1])*cos(Lat*pi/180)*cos(Shore_boundary[2] - Lon*pi/180))) * 6371


myt_full <- myt_full %>% mutate(Total_N = N_T + N_E) %>% mutate(Prop_T = N_T/(N_T +N_E)) %>% mutate(Fi_T = 2*asin(sqrt(Prop_T))*180/pi) 

myt_site <- myt_full %>% select(-Prop_T, -Fi_T) %>% group_by(Position, Site, Shore) %>% select(Lat,      Lon, N_T, N_E, Min_dist_river, Min_dist_port, Average,   Dist_cut, Total_N) %>% summarise(Lat = mean(Lat), Lon = mean(Lon), N_T = sum(N_T), N_E = sum(N_E), Min_dist_river = mean(Min_dist_river), Min_dist_port = mean(Min_dist_port), Average = min(Average),   Dist_cut = mean(Average), Total_N = sum(Total_N)) %>% mutate(Prop_T = N_T/(N_T+N_E)) %>% mutate(Fi_T = 2*asin(sqrt(Prop_T))*180/pi)

str(myt_site)

myt_site <- myt_site %>% as.data.frame()

# Убираем сайты с NA

myt_site <- myt_site %>% filter(complete.cases(.))

# Убираем сайты с неполной схемой вятия проб (нет пары фукус-грунт)


sites_excluded <- c("chupa_fg", "umba_pioner", "umba_06", "umba_fg", "umba_sovhoz", "umba_kamni", "umba_bridge", "umba_pikut", "padan", "porya", "Vor5", "Ovech", "oenij", "Korg", "Mat", "Mal", "salnij", "Lubch", "kanal",  "Vor4", "Vor2")

nrow(myt_site)

myt_site <- myt_site %>% filter(! Site %in% sites_excluded)

# Строим модель с помощью пакета glmmfields

library(nlme)
library(lme4)


coords <- myt_site %>% select(Lat, Lon)

f1 <- formula(Fi_T ~ Position + Min_dist_river + Average + Min_dist_port)

Mod_gls1 <- gls(f1,  data = myt_site)

xycoords2 <- myt_site %>% as.data.frame() %>% select(Lon, Lat)  %>% as.matrix()


#   myt_site_karel %>% group_by(Site) %>% summarise(Lon = mean(Lon), Lat = mean(Lat))
# sites <- xycoords2$Site

# xycoords2 <- xycoords2 %>% select(Lon, Lat) %>% as.matrix()

rownames(xycoords2) = myt_site$Site
colnames(xycoords2) = c("Lon","Lat")

# xycoords2 <- xycoords2 + rnorm(n = nrow(xycoords2),mean = 0.00001, sd = 0.00000001)

xycoords2 <- xycoords2 + rnorm(n = nrow(xycoords2),mean = 0.00001, sd = 0.0000001)

myt_site <- myt_site %>% mutate(Lat_modif = Lat + rnorm(n(),mean = 0.00001, sd = 0.0000001), Lon_modif = Lon + rnorm(n(),mean = 0.00001, sd = 0.0000001))

hist(myt_site$Lat - myt_site$Lat_modif)
hist(myt_site$Lon - myt_site$Lon_modif)

Y - myt_site$Fi_T


Mod_gls2 <- gls(f1,  data = myt_site, correlation = corSpher(form = ~ Lon_modif + Lat_modif, nugget = TRUE))

Mod_gls3 <- gls(f1,  data = myt_site, correlation = corRatio(form = ~ Lon_modif + Lat_modif, nugget = TRUE))

Mod_gls4 <- gls(f1,  data = myt_site, correlation = corGaus(form = ~ Lon_modif + Lat_modif, nugget = TRUE))

Mod_gls5 <- gls(f1,  data = myt_site, correlation = corExp(form = ~ Lon_modif + Lat_modif, nugget = TRUE))


AIC(Mod_gls1, Mod_gls2, Mod_gls3, Mod_gls4, Mod_gls5)


var_Mod <- Variogram(Mod_gls5, form = ~ Lon_modif + Lat_modif, robust = TRUE,
                         resType = "pearson",  data = myt_site)   

plot(var_Mod)

summary(Mod_gls5)

