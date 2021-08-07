library(ggplot2)
library(lme4)
library(dplyr)




##### Data reading
myt <- read.table("data/Distred_samples_fetch_corrected_2021.csv", header = T, sep = ",")

sal <- read.table("data/Distred_samples_salinity_2021.csv", header = T, sep = ",")

myt <- merge(myt, sal, all = T)

river <- read.table("data/Rivers_2021.csv", sep = ",", header = T)

ports <- data.frame(Shore = c("Kand", "Karel", "Kand", "Karel", "Karel"), Port = c("Kandalaksha", "Vitino", "Umba", "Chupa", "Sredny"), Status = c("Active", "Active", "Abandoned", "Abandoned", "Abandoned" ), Lat = c(67.137283, 67.076570,  66.677970, 66.269964, 66.294178), Lon = c(32.407995, 32.333630, 34.357655, 33.069534, 33.640656))


sites_fetch_df <- read.table("data/Distred_samples_fetch_values_2021.csv", sep = ",", header = T)





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

names(df_river) <- c( "Shore_river", "River", "Drainage_Area", "River_size", "Lat_river", "Lon_river", "Min_dist_river", "Site" )


# Находим расстояния для ближайшего порта.

df_port <- nearest_dist(XY = myt[1, c("Lat", "Lon")], objects = ports)

df_port[1,] <- NA

for(i in 1:nrow(myt)) {
  df_port[i,] <- nearest_dist(XY = myt[i, c("Lat", "Lon")], objects = ports)
  df_port$Site[i] <- as.character(myt$Site)[i]
}

names(df_port) <- c("Shore_port",  "Port","Port_Status", "Lat_port", "Lon_port", "Min_dist_port", "Site")






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

myt_site <- myt_full %>% select(-Prop_T, -Fi_T) %>% group_by(Shore, Site, Position) %>% select(Lat,      Lon, N_T, N_E, Min_dist_river, River, River_size, Min_dist_port, Port, Port_Status, Average, Dist_cut, Total_N) %>% summarise(Lat = mean(Lat), Lon = mean(Lon), N_T = sum(N_T), N_E = sum(N_E), Min_dist_river = mean(Min_dist_river), River = unique(River), River_size = unique(River_size), Min_dist_port = mean(Min_dist_port), Port = unique(Port), Port_Status = unique(Port_Status), Average = min(Average),   Dist_cut = mean(Average), Total_N = sum(Total_N)) %>% mutate(Prop_T = N_T/(N_T+N_E)) %>% mutate(Fi_T = 2*asin(sqrt(Prop_T))*180/pi)

str(myt_site)

myt_site <- myt_site %>% as.data.frame()

# Убираем сайты с NA

myt_site <- myt_site %>% filter(complete.cases(.))


# Убираем сайты с неполной схемой вятия проб (нет пары фукус-грунт, или какой-нибудь другой сбой в схеме взятия проб)

sites_excluded <- c("chupa_fg", "umba_pioner", "umba_06", "umba_fg", "umba_sovhoz", "umba_kamni", "umba_bridge", "umba_pikut", "padan", "porya", "Vor5", "Ovech", "oenij", "Korg", "Mat", "Mal", "salnij", "Lubch", "kanal",  "Vor4", "Vor2", "Kurt", "Ryazh4", "Ryazh5", "Youzh")

nrow(myt_site)

myt_site <- myt_site %>% filter(! Site %in% sites_excluded) 

nrow(myt_site)

myt_full <- myt_full %>% filter(! Site %in% sites_excluded) 

nrow(myt_full)

myt_full$Position <- factor(myt_full$Position)

myt_full$Position <- relevel(myt_full$Position, ref = "Bottom")

myt_full$Port_Status <- factor(myt_full$Port_Status)

myt_full$Port_Status <- relevel(myt_full$Port_Status, ref = "Abandoned")


myt_full$River_Size <- factor(myt_full$River_size)

myt_full$River_Size <- relevel(myt_full$River_size, ref = "Small")


myt_full$Site <- factor(myt_full$Site)


str(myt_full$Position)

str(myt_full$Port_Status)

str(myt_full$River_size)


# myt_full$Lat2 <- myt_full$Lat + rep(seq(0.00000, 0.00005, by = 0.00001), nrow(myt_full)/6) 

# myt_full$Lat - myt_full$Lat2

### Общая характеристика материала #########

library(reshape2)
myt_full %>% group_by(Site, Position) %>% summarise(N_samples = n()) %>% dcast(Site ~ Position ) 


# Добавляю к широте и долготе минимальное значение, чтобы обеспечить смещение ближайших проб на небольшое расстояние друг относительно друга. Это нужно для анализа пространственных автокорреляций.

myt_full$Lat2 <- myt_full$Lat + rep(seq(0.00000, 0.00000005, by = 0.00000001), nrow(myt_full)/6)

myt_full$Lon2 <- myt_full$Lon + rep(seq(0.00000, 0.00000005, by = 0.00000001), nrow(myt_full)/6)




library(glmmTMB)

model_TMB <- glmmTMB(cbind(N_T, N_E) ~ Position + Salinity + Min_dist_river + River_Size + Average_Fetch + Min_dist_port + Port_Status + (1|Site),  data = myt_full, family = betabinomial(link = "logit"))

summary(model_TMB)

E1 <- residuals(model_TMB)
p1 <- length(fixef(model_TMB)) + 1  #+1 because of sigma_zoo
Overdisp1 <- sum(E1^2) / (nrow(myt_full) - p1)
Overdisp1

overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}


overdisp_fun(model_TMB)




library(sp)
library(spdep)

spdat <- SpatialPointsDataFrame(cbind(myt_full$Lon2, myt_full$Lat2), myt_full)
plot(spdat)

lstw  <- nb2listw(knn2nb(knearneigh(spdat, k = 60)))

lstw$neighbours

str(lstw)

moran.test(residuals(model_TMB), lstw) 


# Стрю модель с поиощью пакета glmmADMB

# install.packages("R2admb")
# install.packages("glmmADMB", 
#                  repos=c("http://glmmadmb.r-forge.r-project.org/repos",
#                          getOption("repos")),
#                  type="source")




library(glmmADMB)

model_ADMB <- glmmadmb(cbind(N_T, N_E) ~ Position + Salinity + Min_dist_river + River_Size + Average_Fetch + Min_dist_port + Port_Status + (1|Site),  data = myt_full, family = "betabinomial")

summary(model_ADMB)
summary(model_TMB)


moran.test(residuals(model_ADMB), lstw) 

