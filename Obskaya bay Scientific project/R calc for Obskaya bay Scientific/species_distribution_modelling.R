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


predictors <- read.csv("data/Bottom_salinity_and_curents_from_model.csv")

table(predictors$Scenario)

predictors_No <- predictors %>% filter(Scenario == "No") %>% select(Station, Long, Lat, Depth, Month, Sal_predicted, Cur_Zon_predicted, Cur_Mer_predicted)


predictors_Constructed <- predictors %>% filter(Scenario == "Constructed") %>% select(Station, Long, Lat, Depth, Month, Sal_predicted, Cur_Zon_predicted, Cur_Mer_predicted)

qplot(predictors_No$Sal_predicted, predictors_Constructed$Sal_predicted)

qplot(predictors_No$Cur_Zon_predicted, predictors_Constructed$Cur_Zon_predicted)


qplot(predictors_No$Cur_Mer_predicted, predictors_Constructed$Cur_Mer_predicted)



mean_predictors_No <- predictors_No %>% group_by(Station) %>% summarise( Long = mean(Long), Lat= mean(Lat), Sal = mean(Sal_predicted), Cur_Zon = mean(abs(Cur_Zon_predicted) ), Cur_Mer = mean(abs(Cur_Mer_predicted)), Prop_Sal_anomalia = mean(Sal_predicted > mean(Sal_predicted)), Prop_Sal_5 = mean(Sal_predicted > 5))


mean_predictors_No <- mean_predictors_No %>% filter(complete.cases(.))



mean_predictors_Constructed <- predictors_Constructed %>% group_by(Station) %>% summarise( Long = mean(Long), Lat= mean(Lat), Sal = mean(Sal_predicted), Cur_Zon = mean(abs(Cur_Zon_predicted) ), Cur_Mer = mean(abs(Cur_Mer_predicted)), Prop_Sal_anomalia = mean(Sal_predicted > mean(Sal_predicted)), Prop_Sal_5 = mean(Sal_predicted > 5))


mean_predictors_Constructed <- mean_predictors_Constructed %>% filter(complete.cases(.))



##########################################


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
bent_B <- bent_B %>% select(Station, spec_selected$Sp)


bent_short <-
  bent_short %>% mutate(Oligochaeta = Oligochaeta_gen._sp. + Oligochaeta_gen._spp.) %>% 
  select(-c(Oligochaeta_gen._sp., Oligochaeta_gen._spp., Oligochaeta_gen._sp._cocons,Senecella_siberica) )

bent_B <-
  bent_B %>% mutate(Oligochaeta = Oligochaeta_gen._sp. + Oligochaeta_gen._spp.) %>% 
  select(-c(Oligochaeta_gen._sp., Oligochaeta_gen._spp., Oligochaeta_gen._sp._cocons,Senecella_siberica) )


mean_weight <- (bent_B %>% select(-Station) %>% colSums()) / (bent_short %>% select(-Station) %>% colSums()) 




length(mean_weight)

str(mean_weight)

stations <- read_excel("Data/Obskaya_bay_2020.xlsx", sheet = "Station parameters", na = "NA")

# Stations with biological samples
stat_full <- stations %>% filter(Exclude == 0)

stat_full <- as.data.frame(stat_full)





### Данные для модели ######


# Убираем станции с отсутствующими оценками предикторов

bent_short <- bent_short %>% filter(Station %in% mean_predictors_No$Station)


bent_short <- bent_short %>% select(-Station)

bent_short <- bent_short[, colSums(bent_short) > 0]


stat_full <- stat_full %>% filter(Station %in% mean_predictors_No$Station)


mean_weight <- mean_weight[names(mean_weight) %in% names(bent_short)] 

length(mean_weight)


n <- nrow(bent_short) # Число станций 
ns <- ncol(bent_short) #Число видов




xycoords <- mean_predictors_No %>% select(Station, Long, Lat) %>% select(-Station) %>% as.matrix(.)

colnames(xycoords) = c("x-coordinate","y-coordinate")
rownames(xycoords) = 1:n




Ydata <- bent_short  %>% round(., 0) %>% as.matrix(.)
colnames(Ydata) <- names(bent_short)
rownames(Ydata) = 1:n

Ydata <- log(Ydata + 1)

# Ydata <- sqrt(Ydata)


Xdata_No <- mean_predictors_No %>% select(Sal, Cur_Zon, Cur_Mer, Prop_Sal_anomalia, Prop_Sal_5) 

colnames(Xdata_No) <- c("Sal", "Cur_Zon", "Cur_Mer", "Prop_Sal_anomalia", "Prop_Sal_5")

cor(Xdata_No)



Xdata_Constructed <- mean_predictors_Constructed %>% select(Sal, Cur_Zon, Cur_Mer, Prop_Sal_anomalia, Prop_Sal_5) 

colnames(Xdata_Constructed) <- c("Sal", "Cur_Zon", "Cur_Mer", "Prop_Sal_anomalia", "Prop_Sal_5")

cor(Xdata_Constructed)


##############################

library(Hmsc)

# Model building ####################

studyDesign = data.frame(sample = as.factor(1:n))
rL.spatial = HmscRandomLevel(sData = xycoords)
rL.spatial = setPriors(rL.spatial,nfMin=1,nfMax=1) 


XFormula = ~ poly(Sal, degree = 2, raw = TRUE) + Cur_Zon + Cur_Mer 

# + poly(Depth, degree = 2, raw = TRUE) 

#We limit the model to one latent variables for visualization 



m.spatial_No = Hmsc(Y=Ydata, XData=Xdata_No, XFormula= XFormula,
                      studyDesign=studyDesign, distr="normal")

# 
# m.spatial_No = Hmsc(Y=Ydata, XData=Xdata_No, XFormula= XFormula,
#                     studyDesign=studyDesign, ranLevels=list("sample"=rL.spatial),distr="normal")


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

m.spatial = sampleMcmc(m.spatial_No, thin = thin, samples = samples, transient = transient,
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

save(m.spatial, file = "m.spatial_No_no_random.RData")


#################################################################
# Оценка модели


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






# Оценка R2 для видов
R2 <- data.frame(Species = names(bent_short), R2  = MF.spatial$R2 )

# 
# #Predictive power
# partition = createPartition(m.spatial, nfolds = 2, column = "sample")
# cvpreds.spatial = computePredictedValues(m.spatial, partition=partition, updater=list(GammaEta=FALSE))
# 
# qplot(x = Ydata[, 1], y = cvpreds.spatial[,1,1] )


# Оценка отсутсвия паттернов в цепочках
mpost.spatial = convertToCodaObject(m.spatial)
plot(mpost.spatial$Alpha[[1]])


summary(mpost.spatial$Alpha[[1]])


# Оценки параметров модели
round(summary(mpost.spatial$Beta, quantiles = c(0.025, 0.5, 0.975))
      [[2]],2)



Gradient = constructGradient(m.spatial, focalVariable = "Cur_Mer")

predY = predict(m.spatial, Gradient = Gradient, expected = TRUE)


Xdata_No 
XX <- as.matrix(Xdata_No[1])

predY_No <- predict(m.spatial, X = XX, expected = TRUE)






q = c(0.25,0.5,0.75)

R2

plotGradient(m.spatial, Gradient, pred = predY, measure = "Y", showData = F, q = q, index = 25)


############################### Предсказание для новых значений

library(Hmsc)

load( "m.spatial_No_new.RData")


X_new <- as.matrix(data.frame(intercept = 1, Sal = Xdata_No$Sal, Sal2 = Xdata_No$Sal^2, Xdata_No$Cur_Zon, Xdata_No$Cur_Mer))

Predicted_No <- predict(m.spatial, X = X_new)


Predicted_No_mean <- Reduce("+", Predicted_No) / length(Predicted_No)

Predicted_No_mean <- round(exp(Predicted_No_mean) -1, 0)
Predicted_No_mean <- as.data.frame(Predicted_No_mean)


ncol(Predicted_No_mean)


Predicted_No_mean_B <- Predicted_No_mean * mean_weight






# Новые значения для предсказания для сценария, когда есть конструкции 
X_new <- as.matrix(data.frame(intercept = 1, Sal = Xdata_Constructed$Sal, Sal2 = Xdata_Constructed$Sal^2, Xdata_Constructed$Cur_Zon, Xdata_Constructed$Cur_Mer))

Predicted_Constr <- predict(m.spatial, X = X_new)


Predicted_Constr_mean <- Reduce("+", Predicted_Constr) / length(Predicted_Constr)

Predicted_Constr_mean <- round(exp(Predicted_Constr_mean) -1, 0)
Predicted_Constr_mean <- as.data.frame(Predicted_Constr_mean)

Predicted_Constr_mean_B <- Predicted_Constr_mean * mean_weight






library(vegan)

H_No <- rowSums(Predicted_No_mean_B)

H_Constructed <- rowSums(Predicted_Constr_mean_B)


# H_No <- diversity(Predicted_No_mean)
# 
# H_Constructed <- diversity(Predicted_Constr_mean)



qplot(H_No, H_Constructed) + geom_abline()


H <- data.frame(Lat = xycoords[,2], Long = xycoords[,1], H_No, H_Constructed)

H$Dif_H <- with(H, log(H_Constructed/H_No))


# Рисуем карту распределения изменений солености ################

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


my_data <- expand.grid(Lat = seq(from = 70.8, to = 72.8 , length.out = 200), Long =  seq(from = Ob_x[1], to =Ob_x[2] , length.out = 200))


Mod_Dif_H <- gam(Dif_H ~ s(Long, Lat), data = H)

plot(Mod_Dif_H)

my_data$Dif_predicted <- predict(Mod_Dif_H, newdata = my_data)

quantiles <- quantile(H$Dif_H, probs = c(0.25, 0.75))

my_data <- my_data %>% mutate(Dif_class = case_when(Dif_predicted > quantiles[2] ~ "Increase",
                                         Dif_predicted <= quantiles[2] & Dif_predicted >= quantiles[1] ~ "Stable",
                                         Dif_predicted < quantiles[1] ~ "Decrease"))


# my_data$Dif_class <- ifelse(my_data$Dif_predicted >= 0, "Increase", "Decrease")



ggplot(my_data, aes(x = Long, y = Lat)) + 
  geom_tile(aes(fill = Dif_predicted)) +
  geom_polygon(data = Ob_df, aes(x=long, y=lat, group=group), fill = "gray90", colour = "gray20") +
  scale_fill_gradient(low = "yellow", high = "red") +
  geom_point(data = H)


ggplot(my_data, aes(x = Long, y = Lat)) + 
  geom_tile(aes(fill = Dif_class)) +
  geom_polygon(data = Ob_df, aes(x=long, y=lat, group=group), fill = "gray90", colour = "gray20") +
  scale_fill_manual(values = c("yellow","red", "gray")) +
  geom_point(data = H)+
  geom_point(aes(x = Sabetta_x, y = Sabetta_y), size = 3, shape = 21, fill = "blue") + 
  geom_point(aes(x = Terminal_x, y = Terminal_y), size = 3, shape = 22, fill = "blue") 

