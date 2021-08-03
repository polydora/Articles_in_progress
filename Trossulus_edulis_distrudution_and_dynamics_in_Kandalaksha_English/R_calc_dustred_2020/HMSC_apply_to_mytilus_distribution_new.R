library(ggplot2)
library(dplyr)
library(Hmsc)


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


sites_excluded <- c("chupa_fg", "umba_pioner", "umba_06", "umba_fg", "umba_sovhoz", "umba_kamni", "umba_bridge", "umba_pikut", "padan", "porya", "Vor5", "Ovech", "oenij", "Korg", "Mat", "Mal", "salnij", "Lubch", "kanal",  "Vor4", "Vor2", "Kash")

nrow(myt_site)

myt_site <- myt_site %>% filter(! Site %in% sites_excluded) 



# Баесовские установки

nChains = 2
test.run = F
if (test.run){
  #with this option, the vignette runs fast but results are not reliable
  thin = 1
  samples = 10
  transient = 5
  verbose = 5
} else {
  #with this option, the vignette evaluates slow but it reproduces the results of the
  #.pdf version
  thin = 5
  samples = 1000
  transient = 500*thin
  verbose = 50*thin
}





############## Попытка построить модель ###########

# Отбираем только данные по Карельскому берегу, они будут trainning datset

myt_full_karel <- myt_full %>% filter(Shore == "Karel")

myt_full_karel <- myt_full_karel %>% filter(!is.na(Position))

nrow(myt_full_karel)

myt_full_kand <- myt_full %>% filter(Shore == "Kand")

myt_full_kand <- myt_full_kand %>% filter(!is.na(Position))




myt_site_karel <- myt_site %>% filter(Shore == "Karel")

myt_site_karel <- myt_site_karel %>% filter(!is.na(Position))


myt_site_kand <- myt_site %>% filter(Shore == "Kand")

myt_site_kand <- myt_site_kand %>% filter(!is.na(Position))





############ Модель, основанная на полном наборе данных (оба берега) ##########
library(Hmsc)

nChains = 2
test.run = F
if (test.run){
  #with this option, the vignette runs fast but results are not reliable
  thin = 1
  samples = 10
  transient = 5
  verbose = 5
} else {
  #with this option, the vignette evaluates slow but it reproduces the results of the
  #.pdf version
  thin = 5
  samples = 1000
  transient = 500*thin
  verbose = 20*thin
}

# myt_site <- rbind(myt_site_karel, myt_site_kand)

library(tidyr)
library(reshape2)

t_site_long <- myt_site %>% slice(rep(1:n(), N_T)) %>% mutate(Sp = "t") %>% select(-N_E, -N_T, -Total_N, -Prop_T,  -Fi_T ) 

e_site_long <- myt_site %>% slice(rep(1:n(), N_E)) %>% mutate(Sp = "e") %>% select(-N_E, -N_T, -Total_N, -Prop_T,  -Fi_T )  


t_full_long <- myt_full %>% slice(rep(1:n(), N_T)) %>% mutate(Sp = "t") %>% select(-N_E, -N_T, -Total_N, -Prop_T,  -Fi_T ) 

e_full_long <- myt_full %>% slice(rep(1:n(), N_E)) %>% mutate(Sp = "e") %>% select(-N_E, -N_T, -Total_N, -Prop_T,  -Fi_T )  



myt_site_long <- rbind(t_long, e_long) 

myt_full_long <- rbind(t_full_long, e_full_long) 


myt_site_long$Sp2 <- ifelse(myt_site_long$Sp == "t", 1, 0)


myt_full_long$Sp2 <- ifelse(myt_full_long$Sp == "t", 1, 0)

myt_full_long <- myt_full_long %>% filter(! Site %in% sites_excluded) 



# y = myt_site_long$Sp2

y = myt_full_long$Sp2


table(y)

# y = myt_site$N_T

# y = cbind(myt_site$N_T, myt_site$N_E)

# y = myt_site$Prop_T


Y=as.matrix(y)

# XData <- myt_full_karel %>% select(Total_N, Position, Min_dist_river,  Average, Min_dist_port)

# XData <- myt_site_long %>% select(Position, Min_dist_river,  Average, Min_dist_port, Total_N)

XData <- myt_full_long %>% select(Position, Min_dist_river,  Average, Min_dist_port)


XData$Position <- factor(XData$Position)

XData <- as.data.frame(XData)

str(XData)

# xycoords = myt_full_karel %>% select(Lon, Lat) %>% as.matrix()
# rownames(xycoords) = myt_full_karel$Sample
# colnames(xycoords) = c("Lon","Lat")
# 
# studyDesign = data.frame(sample = as.factor(myt_full_karel$Sample), plot = as.factor(myt_full_karel$Site)) 
# 
# xycoords <- xycoords + rnorm(n = nrow(xycoords),mean = 0.00001, sd = 0.0000001)




xycoords2 <- myt_full_long %>% as.data.frame() %>% select(Lon, Lat)  %>% as.matrix()


#   myt_site_karel %>% group_by(Site) %>% summarise(Lon = mean(Lon), Lat = mean(Lat))
# sites <- xycoords2$Site

# xycoords2 <- xycoords2 %>% select(Lon, Lat) %>% as.matrix()

rownames(xycoords2) = myt_full_long$Site
colnames(xycoords2) = c("Lon","Lat")

# xycoords2 <- xycoords2 + rnorm(n = nrow(xycoords2),mean = 0.00001, sd = 0.00000001)

xycoords2 <- xycoords2 + rnorm(n = nrow(xycoords2),mean = 0.00001, sd = 0.0000001)


plot(xycoords2)

# studyDesign = data.frame(sample = as.factor(myt_site$Site), position = as.factor(myt_site$Position))

studyDesign = data.frame(Site = as.factor(myt_full_long$Site), Sample =  as.factor(myt_full_long$Sample))



rL = HmscRandomLevel(sData = xycoords2, longlat = F)

m_spat = Hmsc(Y=Y, XData=XData, XFormula = ~  Position + Min_dist_river + Average + Min_dist_port,  studyDesign=studyDesign, ranLevels=list("Site"=rL), distr="probit")

# 

# m_spat_1 = Hmsc(Y=Y, XData=XData, XFormula = ~  Position + Min_dist_river + Average + Min_dist_port,  studyDesign=studyDesign, ranLevels=list("Site"=rL))


# , distr="lognormal poisson"

m_spat = sampleMcmc(m_spat, thin = thin, samples = samples, transient = transient,
                    nChains = nChains, verbose = verbose)



save(m_spat, file = "mspat_probit_all_samples_separately.RData")


mpost_spat = convertToCodaObject(m_spat)

# mpost_spat_1 = convertToCodaObject(m_spat_1)

summary(mpost_spat$Beta)

summary(mpost_spat_1$Beta)

plot(mpost_spat$Beta)

#
# ess.beta = effectiveSize(mpost_spat$Beta)
# psrf.beta = gelman.diag(mpost_spat$Beta,multivariate = FALSE)$psrf
# ess.alpha = effectiveSize(mpost_spat$Alpha[[1]])
# psrf.alpha = gelman.diag(mpost_spat$Alpha[[1]],multivariate =
#                            FALSE)$psrf
# 

effectiveSize(mpost_spat$Beta)
gelman.diag(mpost_spat$Beta, multivariate=FALSE)$psrf

preds_spat = computePredictedValues(m_spat)
MF_spat = evaluateModelFit(hM=m_spat, predY=preds_spat)



preds = computePredictedValues(m_spat, expected = FALSE)
MF = evaluateModelFit(hM = m_spat, predY = preds)





preds.mean = apply(preds, FUN=mean, MARGIN=1) 



# Остатки находим как разность между наблюдемым значением и предсказанным
nres = scale(y-preds.mean)


par(mfrow=c(1,2))
hist(nres, las = 1)
plot(preds.mean,nres, las = 1)
abline(a=0,b=0)

qplot(preds.mean,nres) + geom_smooth()

qplot(preds.mean_1, nres) + geom_smooth()


myt_site %>% filter(preds.mean<0)



m_spat$X



groupnames = c("Habitat",  "Salinity", "Surf", "Port")
group = c(1,1,2,3,4)

group_1 = c(1,1,2,3,4,1)


VP = computeVariancePartitioning(m_spat,group = group, groupnames = groupnames)

VP$vals


VP_1 = computeVariancePartitioning(m_spat_1,group = group_1, groupnames = groupnames)




load(file = "mspat_probit_all_samples_separately.RData")

m_spat_full <- m_spat

mpost_spat = convertToCodaObject(m_spat_full)

summary(mpost_spat$Beta)

computeWAIC(m_spat_full)






# Модель где все пробы из одного сайта объединены ####
load(file = "mspat_probit_full.RData")

m_spat_1 <- m_spat



#####################################3




# Моделируем зависимость Fi_T

y = myt_site$Fi_T


Y=as.matrix(y)

# XData <- myt_full_karel %>% select(Total_N, Position, Min_dist_river,  Average, Min_dist_port)

XData <- myt_site %>% select(Position, Min_dist_river,  Average, Min_dist_port, Total_N)

# XData <- myt_site_long %>% select(Position, Min_dist_river,  Average, Min_dist_port)


XData$Position <- factor(XData$Position)

XData <- as.data.frame(XData)

# xycoords = myt_full_karel %>% select(Lon, Lat) %>% as.matrix()
# rownames(xycoords) = myt_full_karel$Sample
# colnames(xycoords) = c("Lon","Lat")
# 
# studyDesign = data.frame(sample = as.factor(myt_full_karel$Sample), plot = as.factor(myt_full_karel$Site)) 
# 
# xycoords <- xycoords + rnorm(n = nrow(xycoords),mean = 0.00001, sd = 0.0000001)




xycoords2 <- myt_site %>% as.data.frame() %>% select(Lon, Lat)  %>% as.matrix()


#   myt_site_karel %>% group_by(Site) %>% summarise(Lon = mean(Lon), Lat = mean(Lat))
# sites <- xycoords2$Site

# xycoords2 <- xycoords2 %>% select(Lon, Lat) %>% as.matrix()

rownames(xycoords2) = myt_site$Site
colnames(xycoords2) = c("Lon","Lat")

# xycoords2 <- xycoords2 + rnorm(n = nrow(xycoords2),mean = 0.00001, sd = 0.00000001)

xycoords2 <- xycoords2 + rnorm(n = nrow(xycoords2),mean = 0.00001, sd = 0.0000001)


plot(xycoords2)

# studyDesign = data.frame(sample = as.factor(myt_site$Site), position = as.factor(myt_site$Position))

studyDesign = data.frame(Site = as.factor(myt_site$Site))



rL = HmscRandomLevel(sData = xycoords2, longlat = F)

m_spat = Hmsc(Y=Y, XData=XData, XFormula = ~  Position + Min_dist_river + Average + Min_dist_port,  studyDesign=studyDesign, ranLevels=list("Site"=rL))

# 

# m_spat_1 = Hmsc(Y=Y, XData=XData, XFormula = ~  Position + Min_dist_river + Average + Min_dist_port,  studyDesign=studyDesign, ranLevels=list("Site"=rL))


# , distr="lognormal poisson"

m_spat = sampleMcmc(m_spat, thin = thin, samples = samples, transient = transient,
                    nChains = nChains, verbose = verbose)


save(m_spat, file = "m_spat_Fi_T.RData")

# m_spat_1 = sampleMcmc(m_spat_1, thin = thin, samples = samples, transient = transient,
# nChains = nChains, verbose = verbose)


mpost_spat = convertToCodaObject(m_spat)

# mpost_spat_1 = convertToCodaObject(m_spat_1)

summary(mpost_spat$Beta)

summary(mpost_spat_1$Beta)

plot(mpost_spat$Beta)

#
# ess.beta = effectiveSize(mpost_spat$Beta)
# psrf.beta = gelman.diag(mpost_spat$Beta,multivariate = FALSE)$psrf
# ess.alpha = effectiveSize(mpost_spat$Alpha[[1]])
# psrf.alpha = gelman.diag(mpost_spat$Alpha[[1]],multivariate =
#                            FALSE)$psrf
# 

effectiveSize(mpost_spat$Beta)
gelman.diag(mpost_spat$Beta, multivariate=FALSE)$psrf

preds_spat = computePredictedValues(m_spat)
MF_spat = evaluateModelFit(hM=m_spat, predY=preds_spat)



preds = computePredictedValues(m_spat, expected = FALSE)
MF = evaluateModelFit(hM = m_spat, predY = preds)


preds_1 = computePredictedValues(m_spat_1, expected = FALSE)
MF_1 = evaluateModelFit(hM = m_spat_1, predY = preds_1)


str(preds_1)


preds.mean = apply(preds, FUN=mean, MARGIN=1) 

preds.mean_1 = apply(preds_1, FUN=mean, MARGIN=1) 


# Остатки находим как разность между наблюдемым значением и предсказанным
nres = scale(y-preds.mean)

nres_1 = scale(y-preds.mean_1)


par(mfrow=c(1,2))
hist(nres, las = 1)
plot(preds.mean,nres, las = 1)
abline(a=0,b=0)

qplot(preds.mean,nres) + geom_smooth()

qplot(preds.mean_1, nres) + geom_smooth()


myt_site %>% filter(preds.mean<0)



m_spat$X

m_spat_1$X

groupnames = c("Habitat",  "Salinity", "Surf", "Port", "Effort")
group = c(1,1,2,3,4,5)

group_1 = c(1,1,2,3,4,1)


VP = computeVariancePartitioning(m_spat,group = group, groupnames = groupnames)

VP$vals


VP_1 = computeVariancePartitioning(m_spat_1,group = group_1, groupnames = groupnames)


computeWAIC(m_spat)





# Gradient  construction##############


par(mfrow = c(1,2))

Gradient = constructGradient(m_spat, focalVariable = "Min_dist_port", non.focalVariables = list(Position = 1))


predY = predict(m_spat, Gradient = Gradient, expected = TRUE)

plotGradient(m_spat, Gradient, pred = predY, measure = "Y",
             index = 1,showData = TRUE)


Gradient2 = constructGradient(m_spat, focalVariable = "Min_dist_port", non.focalVariables = list(Position = 2))


predY = predict(m_spat, Gradient = Gradient2, expected = TRUE)

plotGradient(m_spat, Gradient2, pred = predY, measure = "Y",
             index = 1,showData = TRUE)




Gradient = constructGradient(m_spat, focalVariable = "Average", non.focalVariables = list(Position = 1))
plotGradient(m_spat, Gradient, pred = predY, measure = "Y", index = 1, showData = TRUE)

Gradient = constructGradient(m_spat, focalVariable = "Average", non.focalVariables = list(Position = 0))
plotGradient(m_spat, Gradient, pred = predY, measure = "Y", index = 1, showData = TRUE)






Gradient = constructGradient(m_spat, focalVariable = "Position")
plotGradient(m_spat, Gradient, pred = predY, measure = "Y", index = 1, showData = TRUE)



Gradient = constructGradient(m_spat, focalVariable = "Average", non.focalVariables = list(Position = 0))
plotGradient(m_spat, Gradient, pred = predY, measure = "Y", index = 1, showData = TRUE)
