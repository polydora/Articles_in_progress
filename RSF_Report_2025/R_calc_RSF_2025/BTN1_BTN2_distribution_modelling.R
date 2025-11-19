# В этом скрипте идет построение SDM для BTN1 и BTN2 на основе литературных данных и данных по географии распространения BTN, полученных группой Стрелкова.


library(dplyr)
library(broom)
library(readxl)
library(ggmap)
library(maps)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(reshape2)
library(scatterpie)
library(cowplot)


# Загрузка данных о странах мира
world <- ne_countries(scale = "medium", returnclass = "sf")

# Фильтрация стран Евразии по континенту
# eurasia <- subset(world, continent %in% c("Europe", "Asia", "Africa"))

Pl_map <- 
  ggplot(data = world) +
  geom_sf(fill = "gray50", color = "gray50") +
  theme_map()  



# ЗАгрузка всех данных
# Загрузка данных о странах мира
world <- ne_countries(scale = "medium", returnclass = "sf")

# Фильтрация стран Евразии по континенту
# eurasia <- subset(world, continent %in% c("Europe", "Asia", "Africa"))

Pl_map <- 
ggplot(data = world) +
  geom_sf(fill = "gray50", color = "gray50") +
  theme_map()  



# ЗАгрузка всех данных

btn_geography <- read_excel("Data/MtBTN geography 2025.xlsx", sheet = "Data_for_analyzis")

btn_Magadan <- read_excel("Data/summary table_Magadan_itog.xlsx", sheet = "data")

btn_Kola <- read_excel("Data/Кольский рак было-стало.xlsx")


dn_world <- read_excel("Data/World_DN_prevalence.xlsx", na = "NA")



#
# Унификация записей в разных датасетах

btn_geography_uni <- 
  btn_geography %>% 
  select(Sample_ID, Year, Lat, Lon, DN) %>% 
  rename(Site_code = Sample_ID) %>% 
  mutate(BTN_presence = ifelse(DN == 0, "no", "yes")) %>% 
  select(-DN) %>% 
  mutate(Dataset = "Our geography") %>% 
  arrange(BTN_presence)


btn_Magadan_uni <-
btn_Magadan %>% 
  select(Site_code, Year, Lat, Lon, DN_FC) %>% 
  rename(DN = DN_FC) %>% 
  mutate(BTN_presence = ifelse(DN == 0, "no", "yes")) %>% 
  select(-DN) %>% 
  mutate(Dataset = "Magadan") %>% 
  arrange(BTN_presence)


btn_Kola_uni <-
  btn_Kola %>% 
  select(Site, Year, Latitude, Longitude, BTN) %>% 
  rename(Site_code = Site, Lat = Latitude, Lon = Longitude) %>% 
  mutate(BTN_presence = ifelse(BTN == "no", "no", "yes")) %>% 
  select(-BTN) %>% 
  mutate(Dataset = "Kola") %>% 
  arrange(BTN_presence)


btn_world_uni <-
  dn_world %>% 
  select(Site, Year, Lat, Lon,N_DN, N_BTN) %>% 
  mutate(N_DN = ifelse(!is.na(N_DN), N_DN, N_BTN)) %>% 
  rename(Site_code = Site, DN = N_DN) %>% 
  mutate(BTN_presence = ifelse(DN == 0, "no", "yes")) %>% 
  select(-DN, -N_BTN) %>% 
  mutate(Dataset = "World") %>% 
  arrange(desc(BTN_presence))

all_points <- rbind(btn_geography_uni, btn_Magadan_uni, btn_Kola_uni, btn_world_uni)

all_points <- 
  all_points %>% 
  filter(complete.cases(.))

########################## Собираем обучающую выборку ################

# ЗАгрузка данных, пригодных для моделирования распространения BTN1 и BTN2

btn_geography_selected <- 
  read_excel("Data/MtBTN geography 2025_Selected.xlsx", sheet = "Data_for_analyzis") %>%
  filter(Include == 1)


btn_geography_selected_test <- 
  read_excel("Data/MtBTN geography 2025_Selected.xlsx", sheet = "Data_for_analyzis") %>%
  filter(Include != 1)


btn_Magadan_selected <- 
  read_excel("Data/summary table_Magadan_itog_Selected.xlsx", sheet = "data") %>% 
  filter(Include == 1)

dn_Hammel_selected <- 
  read_excel("Data/World_DN_prevalence_Selected.xlsx", na = "NA") %>% 
  filter(Include == 1)



# Приводим обучающую выборку к одинаковым обозначениям

btn_geography_uni_modelling <- 
  btn_geography_selected %>% 
  select(Sample_ID, Year, Lat, Lon, BTN1, BTN2) %>% 
  rename(Site_code = Sample_ID) %>% 
  mutate(BTN1 = ifelse(BTN1 == 0, 0, 1), BTN2 = ifelse(BTN2 == 0, 0, 1) ) %>%  
  mutate(Dataset = "Our geography")


btn_Magadan_uni_modelling <-
  btn_Magadan_selected %>% 
  select(Site_code, Year, Lat, Lon, BTN1, Double, BTN2.1, BTN2.2) %>% 
  mutate(BTN1 = BTN1 + Double, BTN2 = Double + BTN2.1 + BTN2.2) %>% 
  select(-c(Double, BTN2.1, BTN2.2))%>% 
  mutate(BTN1 = ifelse(BTN1 == 0, 0, 1), BTN2 = ifelse(BTN2 == 0, 0, 1) ) %>% 
  mutate(Dataset = "Magadan") 

# Att! В датасете от Hammel считаем, что все BTN2
btn_Hammel_uni_modelling <-
  dn_Hammel_selected %>% 
  select(Site, Year, Lat, Lon, N_BTN) %>% 
  mutate(BTN2 = ifelse(N_BTN !=0, 1, 0)) %>% 
  rename(Site_code = Site) %>% 
  select(-N_BTN) %>% 
  mutate(BTN1 = 0, Dataset = "World") 

modelling_df <- rbind(btn_geography_uni_modelling, btn_Magadan_uni_modelling, btn_Hammel_uni_modelling)

modelling_df <- 
  modelling_df %>% 
  filter(complete.cases(.))


#### Создаем датасет с тестовыми данными, где определяли BTN1 и BTN2

btn_geography_selected_test <-
  btn_geography_selected_test %>% 
  select(Sample_ID, Year, Lat, Lon, BTN1, BTN2) %>% 
  rename(Site_code = Sample_ID) %>% 
  mutate(BTN1 = ifelse(BTN1 == 0, 0, 1), BTN2 = ifelse(BTN2 == 0, 0, 1) ) %>%  
  mutate(Dataset = "Our geography")



######## Создаем датасет с предикторами ###############


library(raster)

lst <- list.files(path="D:/Data_LMBE/BIO_Oracle_predictors/",pattern='asc$',full.names = T) # list the name of files in the specified

predictors <- stack(lst)

names(predictors) <- c(
  "Chlorophyll",
  "Current",
  "Dissolved_O2",
  "Nitrate",
  "pH",
  "Phosphate",
  "Phytoplankton",
  "Primary_Prod",
  "Salinity",
  "Silicate",
  "Temperature"
)


my.sites <- as.matrix(modelling_df %>% dplyr::select(Lon, Lat))

df_predictors <- as.data.frame(extract(predictors, my.sites, method = 'bilinear'))

preds <- data.frame(Lon = my.sites[, 1], Lat = my.sites[, 2],  df_predictors)




 # Строим модели для каждого из BTN в отдельности
library(sdm)

modelling_df %>% 
  dplyr::select(Lon, Lat, BTN1, BTN2)  ->
  BTN_occur

coordinates(BTN_occur) <- ~ Lon + Lat
proj4string(BTN_occur) <- CRS("+init=epsg:4326")  # WGS84


species <- BTN_occur

# Создаем данные для моделирования
dat_BTN1 <- sdmData(formula = BTN1 ~ Chlorophyll + Current + Nitrate + Salinity + Temperature, train=species, predictors=predictors)

dat_BTN2 <- sdmData(formula = BTN2 ~ Chlorophyll + Current + Nitrate + Salinity + Temperature, train=species, predictors=predictors)

dat_BTN <- sdmData(formula = BTN1 + BTN2 ~ Chlorophyll + Current + Nitrate + Salinity + Temperature, train=species, predictors=predictors)


sdm_BTN1 <- sdm(BTN1 ~., data = dat_BTN1, methods = c('gam', 'maxent',  'brt', 'rf'))

sdm_BTN2 <- sdm(BTN2 ~., data = dat_BTN2, methods = c('gam', 'maxent',  'brt', 'rf'))


# Лучше брать модель эту, но две предыдущие это то же самое
sdm_BTN <- sdm(BTN1 + BTN2 ~., data = dat_BTN, methods = c('gam', 'maxent',  'brt', 'rf'))


# Предсказанные значения
train_predictions <- ensemble(sdm_BTN, newdata = preds)

train_predictions_preds <- cbind(preds, train_predictions)


# sdm_BTN <- sdm(BTN1 + BTN2 ~., data = dat_BTN, methods = c('maxent'))
# 
# sdm_BTN <- sdm(BTN1 + BTN2 ~., data = dat_BTN, methods = c('gam'))
# 
# sdm_BTN <- sdm(BTN1 + BTN2 ~., data = dat_BTN, methods = c('rf'))



############  Рисунки для демонстрации SDM ###########

# Карта распределния точек обучающей выборки

# Загрузка береговой линии
# Загружаем данные береговой линии
coastline <- ne_coastline(scale = "medium", returnclass = "sf")

# Преобразуем в ту же проекцию, что и растр
coastline <- st_transform(coastline, crs = crs(preds_far_east))


# Получаем данные суши
land <- ne_countries(scale = "medium", returnclass = "sf")
land <- st_transform(land, crs(preds_far_east))



Pl_Eurasia <- 
ggplot() +
  geom_sf(data = land, fill = "gray80") +
  coord_sf(
    xlim = c(-10, 180),   
    ylim = c(30, 80)     
  ) +
  theme_bw()


Pl_Eurasia +
  geom_point(data = modelling_df, aes(x = Lon, y = Lat), size = 2, shape = 21, fill = "green")

ggsave(filename = "Training_data.jpg", dpi = 400)






########### Важность переменных ##########

Var_importance_BTN1 <- getVarImp(sdm_BTN, id = "ensemble", species="BTN1")

Var_importance_BTN1_df <- data.frame(Lineage = "BTN1", Variable = Var_importance_BTN1@variables, Importance = Var_importance_BTN1@varImportance$AUCtest)


Var_importance_BTN2 <- getVarImp(sdm_BTN, id = "ensemble", species="BTN2")

# plot(Var_importance_BTN2, species="BTN2")

Var_importance_BTN2_df <- data.frame(Lineage = "BTN2", Variable = Var_importance_BTN2@variables, Importance = Var_importance_BTN2@varImportance$AUCtest)


Var_importance_df <- rbind(Var_importance_BTN1_df, Var_importance_BTN2_df)

Var_importance_df %>% 
  ggplot(aes(x = Variable, y = Importance *100, fill = Lineage ))+
  geom_col(position = position_dodge()) +
  theme_bw() +
  labs(y = "Сила влияния предиктора (%)", fill = "Линии", x = "Пердикторы") +
  scale_fill_manual(values = c("red", "blue")) 

ggsave(file = "Predictors_importance.jpg")
  


# Response curve Для разны BTN 

rc_BTN1 <- getResponseCurve(sdm_BTN1, id = "ensemble", confidence = T)

rc_BTN2 <- getResponseCurve(sdm_BTN2, id = "ensemble", confidence = T)

smoother_method <- "gam"

Pl_Temperature <- 
ggplot()+
  geom_point(data = rc_BTN1@response$Temperature, aes(Temperature, Response), color = "red", size=0.5) +
  geom_smooth(data = rc_BTN1@response$Temperature, aes(Temperature, Response), color = "red", method = smoother_method, se = F) +
  geom_point(data = rc_BTN2@response$Temperature, aes(Temperature, Response), color = "blue", size=0.5) +
  geom_smooth(data = rc_BTN2@response$Temperature, aes(Temperature, Response), color = "blue", method = smoother_method, se = F) +
  labs(y = "Вероятность") +
  theme_bw()

ggsave(file = "Response_to_Temperature.jpg")


Pl_Salinity <- 
  ggplot()+
  geom_point(data = rc_BTN1@response$Salinity, aes(Salinity, Response), color = "red", size=0.5) +
  geom_smooth(data = rc_BTN1@response$Salinity, aes(Salinity, Response), color = "red", method = smoother_method, se = F) +
  geom_point(data = rc_BTN2@response$Salinity, aes(Salinity, Response), color = "blue", size=0.5) +
  geom_smooth(data = rc_BTN2@response$Salinity, aes(Salinity, Response), color = "blue", method = smoother_method, se = F) +
  labs(y = "Вероятность") +
  theme_bw()


Pl_Current <- 
  ggplot()+
  geom_point(data = rc_BTN1@response$Current, aes(Current, Response), color = "red", size=0.5) +
  geom_smooth(data = rc_BTN1@response$Current, aes(Current, Response), color = "red", method = smoother_method, se = F) +
  geom_point(data = rc_BTN2@response$Current, aes(Current, Response), color = "blue", size=0.5) +
  geom_smooth(data = rc_BTN2@response$Current, aes(Current, Response), color = "blue", method = smoother_method, se = F) +
  labs(y = "Вероятность") +
  theme_bw()


Pl_Chlorophyll <- 
  ggplot()+
  geom_point(data = rc_BTN1@response$Chlorophyll, aes(Chlorophyll, Response), color = "red", size=0.5) +
  geom_smooth(data = rc_BTN1@response$Chlorophyll, aes(Chlorophyll, Response), color = "red", method = smoother_method, se = F) +
  geom_point(data = rc_BTN2@response$Chlorophyll, aes(Chlorophyll, Response), color = "blue", size=0.5) +
  geom_smooth(data = rc_BTN2@response$Chlorophyll, aes(Chlorophyll, Response), color = "blue", method = smoother_method, se = F) +
  labs(y = "Вероятность") +
  theme_bw()



rc_BTN1 <- sdm::rcurve(sdm_BTN1, id = "ensemble", n = "Temperature", confidence = T)
str(rc_BTN1)

plot(rc_BTN1)

rc_BTN2 <- sdm::rcurve(sdm_BTN2, id = "ensemble")
plot(rc_BTN2)








############## Предсказание моделей для Дальнего Востока

preds_far_east <- brick("preds_coastal_Far_East_50_km.tif")

names(preds_far_east) <- c(
  "Chlorophyll",
  "Current",
  "Dissolved_O2",
  "Nitrate",
  "pH",
  "Phosphate",
  "Phytoplankton",
  "Primary_Prod",
  "Salinity",
  "Silicate",
  "Temperature"
)


# predictions_sdm_BTN <- predict(sdm_BTN,newdata=preds_far_east, filename='predictions_sdm_BTN_Far_East.img', overwrite=TRUE) # many commonly used raster format is supported (throplot(p1)


predictions_ensemble_sdm_BTN_Far_East <- ensemble(sdm_BTN, newdata=preds_far_east, filename="predictions_sdm_BTN_Far_East.img", overwrite=TRUE, setting=list(method='weighted',stat='AUC'))          

plot(predictions_ensemble_sdm_BTN_Far_East)

# plot(predictions_ensemble_sdm_BTN_Far_East)

predict_BTN <- brick("predictions_sdm_BTN_Far_East.img")

plot(predict_BTN)


# Загрузка береговой линии
# Загружаем данные береговой линии
coastline <- ne_coastline(scale = "medium", returnclass = "sf")

# Преобразуем в ту же проекцию, что и растр
coastline <- st_transform(coastline, crs = crs(preds_far_east))


# Получаем данные суши
land <- ne_countries(scale = "medium", returnclass = "sf")
land <- st_transform(land, crs(preds_far_east))

# 
raster_df <- as.data.frame(predict_BTN, xy = TRUE)

raster_df <- 
  raster_df %>% 
  filter(complete.cases(.))


# df_predictions <-
#   raster_df %>% 
#   mutate(Prob_BTN1 = (id_1__sp_BTN1__m_gam + id_2__sp_BTN1__m_maxent + id_3__sp_BTN1__m_brt + id_4__sp_BTN1__m_rf)/4, Prob_BTN2 = (id_5__sp_BTN2__m_gam + id_6__sp_BTN2__m_maxent + id_7__sp_BTN2__m_brt + id_8__sp_BTN2__m_rf)/4) %>% 
#   dplyr::select(x, y, Prob_BTN1, Prob_BTN2) %>% 
#   rename(Lon = x, Lat = y)


df_predictions <-
  raster_df %>% 
  mutate(Prob_BTN1 = ensemble_BTN1_weighted, Prob_BTN2 = ensemble_BTN2_weighted) %>% 
  dplyr::select(x, y, Prob_BTN1, Prob_BTN2) %>% 
  rename(Lon = x, Lat = y)


Pl_predictions_BTN1 <- 
  ggplot() +
  geom_tile(data = df_predictions, aes(x = Lon, y = Lat, fill = Prob_BTN1)) +
  geom_sf(data = land, fill = "gray80") +
  coord_sf(
    xlim = c(120, 180),   
    ylim = c(40, 74)     
  ) +
  scale_fill_gradient(low = "white", high = "red")+
  labs(fill = "Probability") +
  theme_bw()+
  ggtitle("BTN1")



Pl_predictions_BTN2 <- 
  ggplot() +
  geom_tile(data = df_predictions, aes(x = Lon, y = Lat, fill = Prob_BTN2)) +
  geom_sf(data = land, fill = "gray80") +
  coord_sf(
    xlim = c(120, 180),   
    ylim = c(40, 74)     
  ) +
  scale_fill_gradient(low = "white", high = "blue")+
  labs(fill = "Probability") +
  theme_bw()+
  ggtitle("BTN2")




# 
# library(cowplot)
# 
# plot_grid(Pl_predictions_BTN1, Pl_predictions_BTN2)
# 


# Наносим точки из тестового датасета

btn_geography_selected_test %>% 
  filter(!Site_code %in% unique(modelling_df$Site_code)) ->
  testing_df

# testing_df_presence <- 
#   testing_df %>% 
#   filter(BTN_presence == "yes")

library(ggnewscale)

Pl_predictions_BTN1 +
  geom_point(data = testing_df %>% filter(BTN1 == 1), aes(x = Lon, y = Lat), size = 4, shape = 21, stroke = 1, fill = "transparent", color = "black") + 
  geom_point(data = testing_df %>% filter(BTN1 != 1), aes(x = Lon, y = Lat), size = 2, shape = 21, fill = "white", color = "black") 


ggsave(file = "SDM_prediction_BTN1_Far_East.jpg", dpi = 400)


Pl_predictions_BTN2 +
  geom_point(data = testing_df %>% filter(BTN2 == 1), aes(x = Lon, y = Lat), size = 4, shape = 21,  stroke = 1, fill = "transparent", color = "black") + 
  geom_point(data = testing_df %>% filter(BTN2 != 1), aes(x = Lon, y = Lat), size = 2, shape = 21, fill = "white", color = "black")


ggsave(file = "SDM_prediction_BTN2_Far_East.jpg", dpi = 400)



############## Предсказание моделей для Европы

preds_Euro <- brick("preds_coastal_Europ_50_km.tif")

names(preds_Euro) <- c(
  "Chlorophyll",
  "Current",
  "Dissolved_O2",
  "Nitrate",
  "pH",
  "Phosphate",
  "Phytoplankton",
  "Primary_Prod",
  "Salinity",
  "Silicate",
  "Temperature"
)


predictions_sdm_BTN <- predict(sdm_BTN,newdata=preds_Euro, filename='predictions_sdm_BTN_Euro.img', overwrite=TRUE) # many commonly used raster format is supported (throplot(p1)

predict_BTN <- brick("predictions_sdm_BTN_Euro.img")

plot(predict_BTN)


predictions_ensemble_sdm_BTN <- ensemble(sdm_BTN, newdata=preds_Euro,setting=list(method='weighted',stat='AUC'))          


# Загрузка береговой линии
# Загружаем данные береговой линии
coastline <- ne_coastline(scale = "medium", returnclass = "sf")

# Преобразуем в ту же проекцию, что и растр
coastline <- st_transform(coastline, crs = crs(preds_Euro))


# Получаем данные суши
land <- ne_countries(scale = "medium", returnclass = "sf")
land <- st_transform(land, crs(preds_Euro))
# plot(land)

raster_df <- as.data.frame(predict_BTN, xy = TRUE)

raster_df <- as.data.frame(predictions_ensemble_sdm_BTN, xy = TRUE)


raster_df <- 
  raster_df %>% 
  filter(complete.cases(.))


df_predictions <-
  raster_df %>% 
  mutate(Prob_BTN1 = ensemble_BTN1_weighted, Prob_BTN2 = ensemble_BTN2_weighted) %>% 
  dplyr::select(x, y, Prob_BTN1, Prob_BTN2) %>% 
  rename(Lon = x, Lat = y)



Pl_predictions_BTN1 <- 
  ggplot() +
  geom_tile(data = df_predictions, aes(x = Lon, y = Lat, fill = Prob_BTN1)) +
  geom_sf(data = land, fill = "gray80") +
  coord_sf(
    xlim = c(-20, 45),   
    ylim = c(30, 80)     
  ) +
  scale_fill_gradient(low = "white", high = "red")+
  labs(fill = "Probability") +
  theme_bw()+
  ggtitle("BTN1")



Pl_predictions_BTN2 <- 
  ggplot() +
  geom_tile(data = df_predictions, aes(x = Lon, y = Lat, fill = Prob_BTN2)) +
  geom_sf(data = land, fill = "gray80") +
  coord_sf(
    xlim = c(-20, 45),   
    ylim = c(30, 80)     
  ) +
  scale_fill_gradient(low = "white", high = "blue")+
  labs(fill = "Probability") +
  theme_bw()+
  ggtitle("BTN2")


Pl_predictions_BTN1 +
  geom_point(data = testing_df %>% filter(BTN1 == 1), aes(x = Lon, y = Lat), size = 4, shape = 21, stroke = 1, fill = "transparent", color = "black") + 
  geom_point(data = testing_df %>% filter(BTN1 != 1), aes(x = Lon, y = Lat), size = 2, shape = 21, fill = "white", color = "black") 


ggsave(file = "SDM_prediction_BTN1_Europ.jpg", dpi = 400)


Pl_predictions_BTN2 +
  geom_point(data = testing_df %>% filter(BTN2 == 1), aes(x = Lon, y = Lat), size = 4, shape = 21,  stroke = 1, fill = "transparent", color = "black") + 
  geom_point(data = testing_df %>% filter(BTN2 != 1), aes(x = Lon, y = Lat), size = 2, shape = 21, fill = "white", color = "black")


ggsave(file = "SDM_prediction_BTN2_Europ.jpg", dpi = 400)





library(cowplot)

plot_grid(Pl_predictions_BTN1, Pl_predictions_BTN2)





######### Пердсказание встречи раковых линий для тестового датасета (точки, которые не вошли в обучающую выборку)


all_points %>% 
  filter(!Site_code %in% unique(modelling_df$Site_code)) ->
  testing_df


  
  
my.sites_test <- as.matrix(testing_df %>% dplyr::select(Lon, Lat))

df_predictors_testing <- as.data.frame(extract(predictors, my.sites_test, method = 'bilinear'))

preds_test <- data.frame(Lon = my.sites_test[, 1], Lat = my.sites_test[, 2],  df_predictors_testing)

nrow(preds_test)

preds_test <-
  preds_test %>% 
  filter(complete.cases(.))


predictions_sdm_BTN_test <- ensemble(sdm_BTN, newdata=preds_test,setting=list(method='weighted',stat='AUC'))         
  
  
 
preds_test_prediction <- cbind(preds_test, predictions_sdm_BTN_test)

testing_df_predictions <-
merge(testing_df, preds_test_prediction) %>% 
  rename(Prob_BTN1 = ensemble_BTN1_weighted , Prob_BTN2 = ensemble_BTN2_weighted )


testing_df_predictions %>% 
  ggplot(aes(x = Prob_BTN1)) +
  geom_histogram() 

testing_df_predictions %>% 
  ggplot(aes(x = Prob_BTN2)) +
  geom_histogram() +
  geom_point(x = mean(testing_df_predictions$BTN_presence == "yes"), y = 0, color= "blue", size = 4)

mean(testing_df_predictions$BTN_presence == "yes")

testing_df_predictions %>% 
  ggplot(aes(x = BTN_presence, y = log(Prob_BTN2) )) +
  geom_boxplot()



testing_df_predictions %>% 
  ggplot(aes(x = BTN_presence, y = log(Prob_BTN1) )) +
  geom_boxplot() 


# Оценка частоты BTN1 BTN2 в регионах

predict_BTN_Far_East <- brick("predictions_sdm_BTN_Far_East.img")

predict_BTN_Europ <- brick("predictions_sdm_BTN_Euro.img")

df_predicted_Far_East <- 
  as.data.frame(predict_BTN_Far_East) %>% 
  filter(complete.cases(.))

df_predicted_Far_East$Region <- "Far_East"

# Делаю в спешке надо переделать!!!
df_predicted_Europ <- 
  as.data.frame(predictions_ensemble_sdm_BTN) %>% 
  filter(complete.cases(.))

df_predicted_Europ$Region <- "Europ"

df_predicted_All <- rbind(df_predicted_Far_East, df_predicted_Europ)


testing_df %>% 
  summarise(BTN1 = mean(BTN1), BTN2 = mean(BTN2))

df_predicted_All %>% 
  summarise(BTN1 = mean(ensemble_BTN1_weighted), BTN2 = mean(ensemble_BTN2_weighted))


df_predicted_All %>% 
  ggplot(aes(x = ensemble_BTN1_weighted, fill = Region)) + 
  geom_density(alpha = 0.5) 



df_predicted_All %>% 
  ggplot(aes(x = ensemble_BTN2_weighted, fill = Region)) + 
  geom_density(alpha = 0.5) 
