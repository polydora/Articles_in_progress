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


##### Связь уровня сброса рек с размером площади водосбора
ggplot(river, aes(x = Drainage_Area, y = Discharge )) + geom_point()

# Модель для вычисления уровня сброса рек в зависимости от Drainage_Are

Mod_river <- lm(Discharge ~ Drainage_Area, data = river)
summary(Mod_river)


quantile(river$Drainage_Area, na.rm = T)






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

names(df_river) <- c( "Shore_river", "River", "Discharge", "Drainage_Area", "River_size", "Lat_river", "Lon_river", "Min_dist_river", "Site" )


df_river$Discharge_pred <- predict(Mod_river, newdata = df_river) 




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


str(myt_full$Position)

str(myt_full$Port_Status)


# myt_full$Lat2 <- myt_full$Lat + rep(seq(0.00000, 0.00005, by = 0.00001), nrow(myt_full)/6) 

# myt_full$Lat - myt_full$Lat2

### Общая характеристика материала #########

library(reshape2)
myt_full %>% group_by(Site, Position) %>% summarise(N_samples = n()) %>% dcast(Site ~ Position ) 


# Добавляю к широте и долготе минимальное значение, чтобы обеспечить смещение ближайших проб на небольшое расстояние друг относительно друга. Это нужно для анализа пространственных автокорреляций.

myt_full$Lat2 <- myt_full$Lat + rep(seq(0.00000, 0.00000005, by = 0.00000001), nrow(myt_full)/6)

myt_full$Lon2 <- myt_full$Lon + rep(seq(0.00000, 0.00000005, by = 0.00000001), nrow(myt_full)/6)


# Ближайшие реки

river

unique(myt_full$River) 

myt_full %>% filter(Site == "Por")

# Распределение значений Wind Fetch между визуальной оценкой степени открытости побережья.

sites_fetch_df %>% 
  melt(., id.vars = c("Site", "Site_Exposition"), value.name = "Fetch", variable.name = "Direction") %>%
  ggplot(., aes(x = Site_Exposition, y = Fetch, fill = Direction)) + geom_boxplot() + scale_fill_manual(values = c("gray95", "gray80", "gray70", "gray60", "gray40"))


citation("fetchR")

# Оценка связи солености с возможными proxy

salinity <- myt_full %>% group_by(Site) %>% summarize(Min_dist_river = mean(Min_dist_river), River_size = unique(River_size), Salinity = mean(Salinity))

sum(!is.na(salinity$Salinity))

nrow(salinity)


ggplot(salinity, aes(x = Min_dist_river, y = Salinity)) + geom_point() + geom_smooth()

ggplot(salinity, aes(x = River_size, y = Salinity)) + geom_boxplot()

Model_salinity <- lm(Salinity ~ Min_dist_river * River_size, data = salinity)

summary(Model_salinity)


# Реконструкция солености для тех сайтов, где нет прямого наблюдения
# 
# salinity_no_data <- salinity %>% filter(is.na(Salinity))
# 
# salinity_no_data$Sal_predicted <- predict(Model_salinity, newdata = salinity_no_data)
# 

# fit a non-spatial model

library(sp)
library(spdep)

spdat <- SpatialPointsDataFrame(cbind(myt_full$Lon2, myt_full$Lat2), myt_full)
plot(spdat)

lstw  <- nb2listw(knn2nb(knearneigh(spdat, k = 60)))

lstw$neighbours

str(lstw)

model <- glmer(cbind(N_T, N_E) ~ Position + Salinity + Min_dist_river + River_size + Average + Min_dist_port + Port_Status + (1|Site),  data = myt_full, family = binomial(link = "logit"), control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)) )

model2 <- glm(cbind(N_T, N_E) ~ Position +  Salinity + Min_dist_river + River_size + Average + Min_dist_port + Port_Status,  data = myt_full, family = binomial(link = "logit"))

AIC(model, model2)


str(myt_full$Position)

moran.test(residuals(model), lstw) 

moran.test(residuals(model2), lstw) 




overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

library(performance)
library(RVAideMemoire)

check_overdispersion(model)


plot(model)


ggplot(myt_full, aes(x = Dist_cut, y = residuals(model, type = "pearson"), color = Shore)) + geom_point() + geom_smooth()  



summary(model)

library(car)

vif(model)

library(MuMIn)

r.squaredGLMM(model)



library(partR2)


res_part_R2 <- partR2(model, partvars=c("Position", "Salinity", "Min_dist_river", "River_size",  "Average", "Min_dist_port", "Port_Status"),max_level=1, nboot=100, parallel = FALSE)

# save(res_part_R2, file = "partR2 result glmer binomial samples in sites.RData")

load(file = "partR2 result glmer binomial samples in sites.RData")

summary(res)
summary(model)
forestplot(res, type = "R2")

p1 <- forestplot(res, type = "R2")

p2 <- forestplot(res, type = "IR2")

p3 <- forestplot(res, type = "SC")

p4 <- forestplot(res, type = "BW")


library(patchwork)

(p1 + p2) / (p3 + p4) + plot_annotation(tag_levels = "A", tag_prefix = "(", tag_suffix = ")")





### Пробую построить модель с помощью пакета spaMM

library(spaMM)

m_spamm <- fitme(cbind(N_T, N_E) ~ Position + Min_dist_river + River_size + Average + Min_dist_port + Port_Status + Matern(1 | Lon2 + Lat2), data = myt_full, family = "binomial")

summary(m_spamm)

AIC(m_spamm)

library(geodist)
dd <- geodist(myt_full[,c("Lon2","Lat2")], measure = "cheap")

mm <- MaternCorr(dd, nu = 0.026, rho = 3.45)
plot(as.numeric(dd), as.numeric(mm), xlab = "Distance between pairs of location [in m]", ylab = "Estimated correlation")
