# В данном скрипте проводится анализ солености при двух сценариях: без строительства сооружений и после их установки. Помимо этого вычислятся расстояние до ближайшего портового сооружения, как прокси для антропогенного влияния.


#  Пакеты ##############
library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)

library(ggmap)
library(mapproj)
library(maps)

library(rgeos) 

library(mapdata)
library(maptools) # Rgshhs
library(PBSmapping)
library(cowplot)
library(mgcv)
library(ggrepel)
library(png)
library(broom)




# данные по солености из модели


salmod <- read_excel("data/Bottom_salinity_from_model.xls")

table(salmod$Month)

salmod <- salmod %>% select(Long, Lat, Depth, Month, Constructed, No) %>% mutate_if(is.character, as.numeric) %>% mutate(Station = salmod$Station, Date = salmod$Date) %>% select(Station, Date, Month, Long, Lat, Depth, Constructed, No)




# Средняя соленость для каждого месяца

salmod_mean <- salmod  %>% group_by(Station, Month) %>% summarise(Depth = mean(Depth), Lat = mean(Lat), Long = mean(Long), Sal_Constructed = mean(Constructed), Sal_No = mean(No), Dif = mean(Constructed - No) ) 

salmod_mean2 <- salmod  %>% group_by(Station) %>% summarise(Depth = mean(Depth), Lat = mean(Lat), Long = mean(Long), Sal_Constructed = mean(Constructed), Sal_No = mean(No), Dif = mean(Constructed - No) ) 
 



# Источники антропогенного воздействия

ports <- data.frame(Name = c("Sabetta", "Terminal"), Lat = c(71.235220, 71.010886), Long = c(72.126754, 73.793525))





#The function for calculation of neares object and distance to it  

nearest_dist <- function(XY, objects = ports, x.name = "Long", y.name = "Lat"){
  XY1 <-as.numeric(XY[,1])
  XY2 <- as.numeric(XY[,2])
  dist <- (acos(sin(XY1*pi/180)*sin(objects[ ,y.name]*pi/180) + cos(XY1*pi/180)*cos(objects[ ,y.name]*pi/180)*cos(XY2*pi/180 - objects[ ,x.name]*pi/180)) * 6371)
  
  MD <- data.frame(Min_dist = min(dist))
  cbind(objects[which(dist == min(dist)), ], MD)
  
}


df_ports <- nearest_dist(XY = salmod_mean[1, c("Lat", "Long")], objects = ports)

for(i in 2:nrow(salmod_mean)) {
  df_ports[i, ] <- nearest_dist(XY = salmod_mean[i, c("Lat", "Long")], objects = ports)
}


salmod_mean$Nearest_Port <- df_ports$Name
salmod_mean$Min_dist <- df_ports$Min_dist


ggplot(salmod_mean, aes(y = Lat, x = Long, size = Min_dist)) + geom_point() 


hist(salmod_mean$Min_dist)

ggplot(salmod_mean, aes(x = Min_dist, y = Sal_Constructed)) + geom_point() 


# ЗАвисимость различий в солености при разных сценариях от расстояния до ближайшего порта
ggplot(salmod_mean, aes(x = Min_dist, y = Dif)) + geom_point() + geom_hline(yintercept = 0) 

# # Вводим разделение станций на три группы 
# 
# salmod_mean <-  salmod_mean %>% mutate(Dist_class = case_when(Min_dist <25 ~ "Nearest", 
#                                                               Min_dist >= 25 & Min_dist <75 ~ "intermediate", 
#                                                               Min_dist >= 75 ~  "Distant"))


salmod_mean %>% group_by(Station) %>% summarise_all(.funs = mean) %>% 
  ggplot(., aes(x = Min_dist, y = Dif)) + geom_point() + geom_hline(yintercept = 0) 



# Рисуем карту распределения изменений солености ################


theme_set(theme_bw())

Ob_df <- read.csv("Data/Obskaya_bay_map.csv") # Map poligones


# Coordinate limites 
# Ob_x <- c(71, 79)

Ob_x <- c(71, 75.2)
Ob_y <- c(70, 73.2)



# Sabetta
Sabetta_y <- 71.235220
Sabetta_x <- 72.126754

# Terminal

Terminal_y <- 71.010886
Terminal_x <- 73.793525


Pl_Obskaya_bay <- 
  ggplot(Ob_df, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill = "gray90", colour = "gray20") + 
  coord_map(xlim = Ob_x, ylim = Ob_y) + 
  theme_bw() +  
  theme(axis.title.x = element_blank(),  axis.title.y = element_blank(), plot.background = element_blank(), panel.border = element_blank(), panel.grid = element_blank()) + 
  theme(axis.text.x =element_blank(), axis.text.y= element_blank()) + 
  theme(axis.ticks = element_blank()) +
  geom_point(aes(x = Sabetta_x, y = Sabetta_y), size = 3, shape = 21, fill = "red") + 
  geom_point(aes(x = Terminal_x, y = Terminal_y), size = 3, shape = 22, fill = "yellow") 




my_data <- expand.grid(Lat = seq(from = Ob_y[1], to = 72.8 , length.out = 100), Long =  seq(from = Ob_x[1], to =Ob_x[2] , length.out = 100))

salmod_mean2 <- salmod_mean %>% group_by(Station) %>% summarise_all(.funs = mean)

Mod_Dif <- gam(Dif  ~ s(Long, Lat), data = salmod_mean2)

summary(Mod_Dif)

my_data$Dif_predicted <- predict(Mod_Dif, newdata = my_data)

ggplot(my_data, aes(x = Long, y = Lat)) + 
  geom_tile(aes(fill = Dif_predicted)) +
  geom_polygon(data = Ob_df, aes(x=long, y=lat, group=group), fill = "gray90", colour = "gray20") +
  scale_fill_gradient(low = "white", high = "red") +
  geom_point(data = salmod_mean2) +
  labs(fill = "Salinity changes")







################################################################################3
# Data on benthic communities ######
bent <- read_excel("Data/Obskaya_bay_2020.xlsx", sheet = "Benthos")


bent_B <- bent %>% filter(Type == "Biomass") %>% select(-Type)

bent_N <- bent %>% filter(Type == "Abundance") %>% select(-Type)


Spec_abund <- bent_N %>% select(-Station) %>% colSums(.) %>% t() %>% as.vector() 

spec_total_abund <- data.frame(Sp = bent_N %>% select(-Station) %>% colnames(), Spec_abund)

N_porog <- 0 

spec_selected <- spec_total_abund %>% filter(Spec_abund > N_porog)

bent_short <- bent_N %>% select(Station, spec_selected$Sp)


bent_short <-
  bent_short %>% mutate(Oligochaeta = Oligochaeta_gen._sp. + Oligochaeta_gen._spp.) %>% 
  select(-c(Oligochaeta_gen._sp., Oligochaeta_gen._spp., Oligochaeta_gen._sp._cocons,Senecella_siberica) )



stations <- read_excel("Data/Obskaya_bay_2020.xlsx", sheet = "Station parameters", na = "NA")

# Stations with biological samples
stat_full <- stations %>% filter(Exclude == 0)

stat_full <- as.data.frame(stat_full)





### Данные для модели ######


# Убираем станции с отсутствующими оценками солености

salmod_mean2 <- salmod_mean2 %>% filter(!is.na(salmod_mean2$Sal_No))

bent_short <- bent_short %>% filter(Station %in% salmod_mean2$Station)


bent_short <- bent_short %>% select(-Station)

bent_short <- bent_short[, colSums(bent_short) > 0]


stat_full <- stat_full %>% filter(Station %in% salmod_mean$Station)




salmod_mean2 <- salmod_mean2  %>% filter(!is.na(salmod_mean2$Sal_No)) 


n <- nrow(bent_short) # Число станций 
ns <- ncol(bent_short)-1 #Число видов




xycoords <- salmod_mean2 %>% select(Station, Long, Lat) %>% select(-Station) %>% as.matrix(.)

colnames(xycoords) = c("x-coordinate","y-coordinate")
rownames(xycoords) = 1:n




Ydata <- bent_short  %>% round(., 0) %>% as.matrix(.)
colnames(Ydata) <- names(bent_short)
rownames(Ydata) = 1:n

Ydata <- log(Ydata + 1)

# Ydata <- sqrt(Ydata)


Xdata_No <- salmod_mean2 %>% select(Sal_No, Depth) 

colnames(Xdata_No) <- c("Sal", "Depth")


Xdata_Constructed <- salmod_mean2 %>% select(Sal_Constructed, Depth)

colnames(Xdata_Constructed) <- c("Sal", "Depth", "Dist")


Xdata_initial <- stat_full %>% mutate (Sal = (Bottom_Salinity_Aug_20), Depth = (Depth_aug_20 + Depth_sep_20)/2 ) %>% select(Sal, Depth) 

colnames(Xdata_initial) <- c("Sal", "Depth")



# qplot(Xdata$Dist, Xdata$Sal)


library(Hmsc)

# Model building ####################

studyDesign = data.frame(sample = as.factor(1:n))
rL.spatial = HmscRandomLevel(sData = xycoords)
rL.spatial = setPriors(rL.spatial,nfMin=1,nfMax=1) 


XFormula = ~ poly(Sal, degree = 2, raw = TRUE) 

# + poly(Depth, degree = 2, raw = TRUE) 

#We limit the model to one latent variables for visualization 



m.spatial_sqrt = Hmsc(Y=Ydata, XData=Xdata, XFormula= XFormula,
                 studyDesign=studyDesign, ranLevels=list("sample"=rL.spatial),distr="normal")

# m.spatial_constructed = Hmsc(Y=Ydata, XData=Xdata_Constructed, XFormula= XFormula,
#                  studyDesign=studyDesign, ranLevels=list("sample"=rL.spatial),distr="normal")


# m.spatial_initial = Hmsc(Y=Ydata, XData=Xdata_initial, XFormula= XFormula,
                             # studyDesign=studyDesign, ranLevels=list("sample"=rL.spatial),distr="normal")

# Bayesian set up

nChains = 2
test.run = FALSE
if (test.run){
  # with this option, the vignette runs fast but results are not reliable
  thin = 1
  samples = 10
  transient = 5
  verbose = 1
} else {
  # with this option, the vignette evaluates slow but it reproduces the results of
  # the .pdf version
  thin = 10
  samples = 1000 #1000
  transient = 1000 #1000
  verbose = 1
}



# Bayesian samplings

m.spatial = sampleMcmc(m.spatial_sqrt, thin = thin, samples = samples, transient = transient,
                       nChains = nChains, verbose = verbose, initPar = "fixed effects",
                       updater=list(GammaEta=FALSE))


# m.spatial_constructed = sampleMcmc(m.spatial_constructed, thin = thin, samples = samples, transient = transient,
#                        nChains = nChains, verbose = verbose, initPar = "fixed effects", 
#                        updater=list(GammaEta=FALSE))

# m.spatial_initial = sampleMcmc(m.spatial_initial, thin = thin, samples = samples, transient = transient,
#                                    nChains = nChains, verbose = verbose, initPar = "fixed effects", 
#                                    updater=list(GammaEta=FALSE))




# save(m.spatial, file = "m.spatial.RData")

# save(m.spatial_constructed, file = "m.spatial_constructed.RData")

save(m.spatial, file = "m.spatial_sqrt.RData")


#################################################################
# Оценка модели

# load("m.spatial.RData")

# load("m.spatial_constructed.RData")

load("m.spatial_sqrt.RData")

# Model evaluation

# mpost = convertToCodaObject(m.spatial, spNamesNumbers =
#                               c(T,F), covNamesNumbers = c(T,F))

# mpost = convertToCodaObject(m.spatial_constructed, spNamesNumbers =
#                               c(T,F), covNamesNumbers = c(T,F))


mpost = convertToCodaObject(m.spatial, spNamesNumbers =
                              c(T,F), covNamesNumbers = c(T,F))


ess.beta = effectiveSize(mpost$Beta)

psrf.beta = gelman.diag(mpost$Beta,multivariate = FALSE)$psrf

ess.alpha = effectiveSize(mpost$Alpha[[1]])
psrf.alpha = gelman.diag(mpost$Alpha[[1]],multivariate =
                           FALSE)$psrf

#Explanatory power

# preds.spatial = computePredictedValues(m.spatial)

preds.spatial = computePredictedValues(m.spatial)
str(preds.spatial)

# MF.spatial = evaluateModelFit(hM=m.spatial, predY=preds.spatial)
MF.spatial = evaluateModelFit(hM=m.spatial, predY=preds.spatial)

MF.spatial$SR2








##############################################

library(vegan)

Predicted_data <- round(apply(preds.spatial, c(1,2), mean), 0)

Predicted_data^2

Predicted_data <- as.data.frame(Predicted_data^2)





pca_predicted_data <- rda(Predicted_data)  

plot(pca_predicted_data, display = "species")


pca_initial <- rda(log(bent_short +1))

plot(pca_initial, display = "species")


# Соотношение главных компонент для исходной матрицы данных и для смоделированной
plot(scores(pca_predicted_data)[[2]][,1], scores(pca_initial)[[2]][,1])



# Изоражаем на одной ординации исходные данные и смоделированные

Predicted_data$Type <- "Predicted"
bent_short <- exp(bent_short) - 1
bent_short$Type <- "Initial"


predicted_initial <- rbind(Predicted_data, bent_short)


pca_predicted_initial <- predicted_initial %>% select(-Type) %>% cca(.)

as.data.frame(scores(pca_predicted_initial)[[2]]) %>% 
  ggplot(., aes(CA1, CA2)) + geom_point(aes(color = predicted_initial$Type))


 
##################################################



# Оценка R2 для видов
R2 <- data.frame(Species = names(bent_short), R2  = MF.spatial$R2 )


#Predictive power
partition = createPartition(m.spatial, nfolds = 2, column = "sample")
cvpreds.spatial = computePredictedValues(m.spatial, partition=partition, updater=list(GammaEta=FALSE))

qplot(x = Ydata[, 1], y = cvpreds.spatial[,1,1] )


# Оценка отсутсвия паттернов в цепочках
mpost.spatial = convertToCodaObject(m.spatial_poisson)
plot(mpost.spatial$Alpha[[1]])


summary(mpost.spatial$Alpha[[1]])


# Оценки параметров модели
round(summary(mpost.spatial$Beta, quantiles = c(0.025, 0.5, 0.975))
      [[2]],2)



Gradient = constructGradient(m.spatial_initial, focalVariable = "Sal")

predY = predict(m.spatial_initial, Gradient = Gradient, expected = TRUE)

str(predY)

predY2 <- predict(m.spatial, expected = TRUE)

str(predY2)


Xdata_Constructed 
XX <- as.matrix(Xdata_Constructed[1])

predY_Constructed <- predict(m.spatial, X = XX, expected = TRUE)






q = c(0.25,0.5,0.75)

R2

plotGradient(m.spatial_initial, Gradient, pred = predY, measure = "Y", showData = F, q = q, index = 4)


