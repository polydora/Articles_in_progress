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


sites_excluded <- c("chupa_fg", "umba_pioner", "umba_06", "umba_fg", "umba_sovhoz", "umba_kamni", "umba_bridge", "umba_pikut", "padan", "porya", "Vor5", "Ovech", "oenij", "Korg", "Mat", "Mal", "salnij", "Lubch", "kanal", "Kash", "Vor4", "Vor2")

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






###### Строим просто модель, не смешанную

y = myt_site_karel$Fi_T 
Y=as.matrix(y)

XData <- myt_site_karel %>% select(Position, Min_dist_river,  Average, Min_dist_port) %>% as.data.frame() %>% select(-Site)

XData$Position <- factor(XData$Position)



m_simple = Hmsc(Y=Y, XData=XData, XFormula=~  Position + Min_dist_river + Average + Min_dist_port, distr="lognormal poisson")


m_simple = sampleMcmc(m_simple, thin = thin, samples = samples, transient = transient,
               nChains = nChains, verbose = verbose)



mpost = convertToCodaObject(m_simple)
summary(mpost$Beta)


effectiveSize(mpost$Beta)
gelman.diag(mpost$Beta, multivariate=FALSE)$psrf

plot(mpost$Beta)


preds = computePredictedValues(m_simple)
MF_simple = evaluateModelFit(hM=m_simple, predY=preds)




####### В модель вводится пространственная структура

y = myt_site_kand$Fi_T 
Y=as.matrix(y)

# XData <- myt_full_karel %>% select(Total_N, Position, Min_dist_river,  Average, Min_dist_port)

XData <- myt_site_kand %>% select(Position, Min_dist_river,  Average, Min_dist_port)


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




xycoords2 <- myt_site_kand %>% as.data.frame() %>% select(Lon, Lat)  %>% as.matrix()
  
  
#   myt_site_karel %>% group_by(Site) %>% summarise(Lon = mean(Lon), Lat = mean(Lat))
# sites <- xycoords2$Site

# xycoords2 <- xycoords2 %>% select(Lon, Lat) %>% as.matrix()
rownames(xycoords2) = myt_site_kand$Site
colnames(xycoords2) = c("Lon","Lat")

xycoords2 <- xycoords2 + rnorm(n = nrow(xycoords2),mean = 0.00001, sd = 0.00000001)

plot(xycoords2)

studyDesign = data.frame(sample = as.factor(myt_site_kand$Site))


rL = HmscRandomLevel(sData = xycoords2, longlat = F)

m_spat = Hmsc(Y=Y, XData=XData, XFormula = ~  Position + Min_dist_river + Average + Min_dist_port,  studyDesign=studyDesign, ranLevels=list("sample"=rL))

# , distr="lognormal poisson"

m_spat = sampleMcmc(m_spat, thin = thin, samples = samples, transient = transient,
               nChains = nChains, verbose = verbose)



mpost_spat = convertToCodaObject(m_spat)
summary(mpost_spat$Beta)
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

str(preds)


preds.mean = apply(preds, FUN=mean, MARGIN=1) 

# Остатки находим как разность между наблюдемым значением и предсказанным
nres = scale(y-preds.mean)


par(mfrow=c(1,2))
hist(nres, las = 1)
plot(preds.mean,nres, las = 1)
abline(a=0,b=0)

qplot(preds.mean,nres) + geom_smooth()

m_spat$X

groupnames = c("Habitat",  "Salinity", "Surf", "Port")
group = c(1,1,2,3,4)



VP = computeVariancePartitioning(m_spat,group = group, groupnames = groupnames)


computeWAIC(m_spat)





m_spat_coefs <- summary(mpost_spat$Beta)$statistics[,1]


Mod_matr_kand <- model.matrix(~  Position + Min_dist_river + Average + Min_dist_port, data =  myt_site_kand)

Mod_matr_karel <- model.matrix(~ Position + Min_dist_river + Average + Min_dist_port, data =  myt_site_karel)




predicted_karel_spat <- (Mod_matr_karel %*% m_spat_coefs) 




qplot(x = myt_site_karel$Fi_T, y = (predicted_karel_spat)) + geom_abline() + geom_abline() + geom_smooth(method = "lm")




####### Упрощенная модель, включающая пространственную структуру

y = myt_site_karel$Fi_T 
Y=as.matrix(y)

# XData <- myt_full_karel %>% select(Total_N, Position, Min_dist_river,  Average, Min_dist_port)

XData <- myt_site_karel %>% select(Position, Min_dist_port)


XData$Position <- factor(XData$Position)

XData <- as.data.frame(XData)

XData<- XData %>% select(-Site)

str(XData)

# xycoords = myt_full_karel %>% select(Lon, Lat) %>% as.matrix()
# rownames(xycoords) = myt_full_karel$Sample
# colnames(xycoords) = c("Lon","Lat")
# 
# studyDesign = data.frame(sample = as.factor(myt_full_karel$Sample), plot = as.factor(myt_full_karel$Site)) 
# 
# xycoords <- xycoords + rnorm(n = nrow(xycoords),mean = 0.00001, sd = 0.0000001)




xycoords2 <- myt_site_karel %>% as.data.frame() %>% select(Lon, Lat)  %>% as.matrix()


#   myt_site_karel %>% group_by(Site) %>% summarise(Lon = mean(Lon), Lat = mean(Lat))
# sites <- xycoords2$Site

# xycoords2 <- xycoords2 %>% select(Lon, Lat) %>% as.matrix()
rownames(xycoords2) = myt_site_karel$Site
colnames(xycoords2) = c("Lon","Lat")

xycoords2 <- xycoords2 + rnorm(n = nrow(xycoords2),mean = 0.00001, sd = 0.00000001)

plot(xycoords2)

studyDesign = data.frame(sample = as.factor(myt_site_karel$Site))


rL = HmscRandomLevel(sData = xycoords2, longlat = T)

m_spat2 = Hmsc(Y=Y, XData=XData, XFormula = ~  Position + Min_dist_port,  studyDesign=studyDesign, ranLevels=list("sample"=rL))

# , distr="lognormal poisson"

m_spat2 = sampleMcmc(m_spat2, thin = thin, samples = samples, transient = transient,
                    nChains = nChains, verbose = verbose)



mpost_spat2 = convertToCodaObject(m_spat2)
summary(mpost_spat2$Beta)
plot(mpost_spat2$Beta)

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

str(preds)


preds.mean = apply(preds, FUN=mean, MARGIN=1) 

# Остатки находим как разность между наблюдемым значением и предсказанным
nres = scale(y-preds.mean)


par(mfrow=c(1,2))
hist(nres, las = 1)
plot(preds.mean,nres, las = 1)
abline(a=0,b=0)



m_spat2$X

groupnames = c("Habitat",  "Port")
group = c(1,1,2)



VP = computeVariancePartitioning(m_spat2,group = group, groupnames = groupnames)


# Строим градиент

par(mfrow = c(1,2))
Gradient = constructGradient(m_spat, focalVariable = "Min_dist_port", non.focalVariables = list(Position = 1))


predY = predict(m_spat, Gradient = Gradient, expected = TRUE)

plotGradient(m_spat, Gradient, pred = predY, measure = "Y",
             index = 1,showData = TRUE)

Gradient = constructGradient(m_spat, focalVariable = "Average", non.focalVariables = list(Position = 1))
plotGradient(m_spat, Gradient, pred = predY, measure = "Y", index = 1, showData = TRUE)






XData_kand <- myt_site_kand %>% select(Position, Min_dist_port)


XData_kand$Position <- factor(XData_kand$Position)

XData_kand <- as.data.frame(XData_kand)

XData_kand<- XData_kand %>% select(-Site) 

str(XData_kand)

xycoords_kand <- myt_site_kand %>% as.data.frame() %>% select(Lon, Lat)  %>% as.matrix()

colnames(xycoords_kand) = c("Lon","Lat")

xycoords_kand <- xycoords_kand + rnorm(n = nrow(xycoords_kand),mean = 0.00001, sd = 0.00000001)


plot(xycoords_kand)


Gradient = prepareGradient(m_spat2, XDataNew = XData_kand, sDataNew = list(sample = xycoords_kand))
  
predY = predict(m_spat2, Gradient = Gradient)





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

myt_site <- rbind(myt_site_karel, myt_site_kand)

y = myt_site$Fi_T 
Y=as.matrix(y)

# XData <- myt_full_karel %>% select(Total_N, Position, Min_dist_river,  Average, Min_dist_port)

XData <- myt_site %>% select(Position, Min_dist_river,  Average, Min_dist_port)


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




xycoords2 <- myt_site %>% as.data.frame() %>% select(Lon, Lat)  %>% as.matrix()


#   myt_site_karel %>% group_by(Site) %>% summarise(Lon = mean(Lon), Lat = mean(Lat))
# sites <- xycoords2$Site

# xycoords2 <- xycoords2 %>% select(Lon, Lat) %>% as.matrix()

rownames(xycoords2) = myt_site$Site
colnames(xycoords2) = c("Lon","Lat")

xycoords2 <- xycoords2 + rnorm(n = nrow(xycoords2),mean = 0.00001, sd = 0.00000001)

plot(xycoords2)

studyDesign = data.frame(sample = as.factor(myt_site$Site), position = as.factor(myt_site$Position))


rL = HmscRandomLevel(sData = xycoords2, longlat = F)

m_spat = Hmsc(Y=Y, XData=XData, XFormula = ~  Position + Min_dist_river + Average + Min_dist_port,  studyDesign=studyDesign, ranLevels=list("sample"=rL))

m_spat_1 = Hmsc(Y=Y, XData=XData, XFormula = ~  Position + Min_dist_river + Average + Min_dist_port + Position:Average,  studyDesign=studyDesign, ranLevels=list("sample"=rL))


# , distr="lognormal poisson"

m_spat = sampleMcmc(m_spat, thin = thin, samples = samples, transient = transient,
                    nChains = nChains, verbose = verbose)


m_spat_1 = sampleMcmc(m_spat_1, thin = thin, samples = samples, transient = transient,
                    nChains = nChains, verbose = verbose)


mpost_spat = convertToCodaObject(m_spat)

mpost_spat_1 = convertToCodaObject(m_spat_1)

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


preds_1.mean = apply(preds, FUN=mean, MARGIN=1) 


# Остатки находим как разность между наблюдемым значением и предсказанным
nres = scale(y-preds.mean)
nres_1 = scale(y-preds_1.mean)


par(mfrow=c(1,2))
hist(nres, las = 1)
plot(preds.mean,nres, las = 1)
abline(a=0,b=0)

qplot(preds.mean,nres) + geom_smooth(method = "lm")

qplot(preds_1.mean,nres) + geom_smooth()



m_spat$X

m_spat_1$X

groupnames = c("Habitat",  "Salinity", "Surf", "Port")
group = c(1,1,2,3,4)

group_1 = c(1,1,2,3,4,1)


VP = computeVariancePartitioning(m_spat,group = group, groupnames = groupnames)

VP_1 = computeVariancePartitioning(m_spat_1,group = group_1, groupnames = groupnames)


computeWAIC(m_spat_1)





m_spat_coefs <- summary(mpost_spat$Beta)$statistics[,1]


Mod_matr_kand <- model.matrix(~  Position + Min_dist_river + Average + Min_dist_port, data =  myt_site_kand)

Mod_matr_karel <- model.matrix(~ Position + Min_dist_river + Average + Min_dist_port, data =  myt_site_karel)




predicted_karel_spat <- (Mod_matr_karel %*% m_spat_coefs) 




qplot(x = myt_site_karel$Fi_T, y = (predicted_karel_spat)) + geom_abline() + geom_abline() + geom_smooth(method = "lm")


