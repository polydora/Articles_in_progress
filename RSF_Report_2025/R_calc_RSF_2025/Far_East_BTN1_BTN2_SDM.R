# В этом скрипте идет построение SDM для BTN1 и BTN2 только по данным дальневосточных сборов группы Стрелкова


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





########################## Собираем обучающую выборку по материалам Дальнего Востока ################

# ЗАгрузка данных, пригодных для моделирования распространения BTN1 и BTN2

btn_geography_selected <- 
  read_excel("Data/MtBTN geography 2025_Selected.xlsx", sheet = "Data_for_analyzis") %>%
  filter(Include == 1)


btn_Magadan_selected <- 
  read_excel("Data/summary table_Magadan_itog_Selected.xlsx", sheet = "data") %>% 
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


modelling_df <- rbind(btn_geography_uni_modelling, btn_Magadan_uni_modelling)

modelling_df <- 
  modelling_df %>% 
  filter(complete.cases(.))


#### Создаем датасет с тестовыми данными, где определяли BTN1 и BTN2
# 
# btn_geography_selected_test <-
#   btn_geography_selected_test %>% 
#   select(Sample_ID, Year, Lat, Lon, BTN1, BTN2) %>% 
#   rename(Site_code = Sample_ID) %>% 
#   mutate(BTN1 = ifelse(BTN1 == 0, 0, 1), BTN2 = ifelse(BTN2 == 0, 0, 1) ) %>%  
#   mutate(Dataset = "Our geography")



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

# Анализ коллинеарности предикторов

foo_model <- lm(Lat ~ Chlorophyll + Current + Dissolved_O2  +  Nitrate + pH+ Phosphate + Phytoplankton + Primary_Prod + Salinity + Silicate + Temperature, data = preds)

vif(foo_model)

foo_model <- lm(Lat ~ Chlorophyll + Current + Dissolved_O2  +  Nitrate + pH+ Phosphate +  Primary_Prod + Salinity + Silicate + Temperature, data = preds)

vif(foo_model)

foo_model <- lm(Lat ~ Chlorophyll + Current + Nitrate + pH+ Phosphate +  Primary_Prod + Salinity + Silicate + Temperature, data = preds)

vif(foo_model)
 
foo_model <- lm(Lat ~ Chlorophyll + Current + Nitrate + pH+  Primary_Prod + Salinity + Silicate + Temperature, data = preds)

vif(foo_model)

foo_model <- lm(Lat ~ Chlorophyll + Current + Nitrate + pH+  Salinity + Silicate + Temperature, data = preds)

vif(foo_model)

foo_model <- lm(Lat ~ Chlorophyll + Current + Nitrate + pH+  Salinity + Temperature, data = preds)

vif(foo_model)

foo_model <- lm(Lat ~ Chlorophyll + Current + Nitrate +  Salinity + Temperature, data = preds)

vif(foo_model)

# Строим модели для каждого из BTN в отдельности
library(sdm)

modelling_df %>% 
  dplyr::select(Lon, Lat, BTN1, BTN2)  ->
  BTN_occur

coordinates(BTN_occur) <- ~ Lon + Lat
proj4string(BTN_occur) <- CRS("+init=epsg:4326")  # WGS84


# Создаем данные для моделирования
dat_BTN1 <- sdmData(formula = BTN1 ~ Chlorophyll + Current + Nitrate + Salinity + Temperature, train=BTN_occur, predictors=predictors)

dat_BTN2 <- sdmData(formula = BTN2 ~ Chlorophyll + Current + Nitrate + Salinity + Temperature, train=BTN_occur, predictors=predictors)

dat_BTN <- sdmData(formula = BTN1 + BTN2 ~ Chlorophyll + Current + Nitrate + Salinity + Temperature, train=BTN_occur, predictors=predictors)


# рассчитываем модели
sdm_BTN1 <- sdm(BTN1 ~., data = dat_BTN1, methods = c('gam', 'maxent',  'brt', 'rf'))

sdm_BTN2 <- sdm(BTN2 ~., data = dat_BTN2, methods = c('gam', 'maxent',  'brt', 'rf'))


# Лучше брать модель эту, но две предыдущие это то же самое
sdm_BTN <- sdm(BTN1 + BTN2 ~., data = dat_BTN, methods = c('gam', 'maxent',  'brt', 'rf'))


# Предсказанные значения
train_predictions <- ensemble(sdm_BTN, newdata = preds)

train_predictions_preds <- cbind(preds, train_predictions)

train_predictions_preds %>% 
  ggplot(aes(x = Temperature, y = ensemble_BTN1_weighted)) +
  geom_point(aes(y = ensemble_BTN1_weighted), color = "red") +
  geom_point(aes(y = ensemble_BTN2_weighted), color = "blue")


# sdm_BTN <- sdm(BTN1 + BTN2 ~., data = dat_BTN, methods = c('maxent'))
# 
# sdm_BTN <- sdm(BTN1 + BTN2 ~., data = dat_BTN, methods = c('gam'))
# 
# sdm_BTN <- sdm(BTN1 + BTN2 ~., data = dat_BTN, methods = c('rf'))



########### Важность переменных ##########

Var_importance_BTN1 <- getVarImp(sdm_BTN, id = "ensemble", species="BTN1")

Var_importance_BTN1_df <- data.frame(Lineage = "BTN1", Variable = Var_importance_BTN1@variables, Importance = Var_importance_BTN1@varImportance$AUCtest)


Var_importance_BTN2 <- getVarImp(sdm_BTN, id = "ensemble", species="BTN2")

Var_importance_BTN2_df <- data.frame(Lineage = "BTN2", Variable = Var_importance_BTN2@variables, Importance = Var_importance_BTN2@varImportance$AUCtest)


Var_importance_df <- rbind(Var_importance_BTN1_df, Var_importance_BTN2_df)

Pl_var_importance <-
Var_importance_df %>% 
  ggplot(aes(x = Variable, y = Importance *100, fill = Lineage ))+
  geom_col(position = position_dodge()) +
  theme_bw() +
  labs(y = "Сила влияния (%)", fill = "Линии", x = "Пердикторы") +
  scale_fill_manual(values = c("red", "blue")) 

ggsave(file = "Predictors_importance_Far_East.jpg")



# Response curve Для разных BTN 

rc_BTN1 <- getResponseCurve(sdm_BTN1, id = "ensemble", confidence = T)

rc_BTN2 <- getResponseCurve(sdm_BTN2, id = "ensemble", confidence = T)

smoother_method <- "gam"

Pl_Temperature <- 
  ggplot()+
  geom_line(data = rc_BTN1@response$Temperature, aes(Temperature, Response), color = "red", linewidth=2) +
  # geom_smooth(data = rc_BTN1@response$Temperature, aes(Temperature, Response), color = "red", method = smoother_method, se = F) +
  geom_line(data = rc_BTN2@response$Temperature, aes(Temperature, Response), color = "blue", linewidth=2) +
  # geom_smooth(data = rc_BTN2@response$Temperature, aes(Temperature, Response), color = "blue", method = smoother_method, se = F) +
  labs(y = "Вероятность") +
  theme_bw()

ggsave(file = "Response_to_Temperature_Far_East.jpg")


Pl_Salinity <- 
  ggplot()+
  geom_line(data = rc_BTN1@response$Salinity, aes(Salinity, Response), color = "red", linewidth=2) +
  # geom_smooth(data = rc_BTN1@response$Salinity, aes(Salinity, Response), color = "red", method = smoother_method, se = F) +
  geom_line(data = rc_BTN2@response$Salinity, aes(Salinity, Response), color = "blue", linewidth=2) +
  # geom_smooth(data = rc_BTN2@response$Salinity, aes(Salinity, Response), color = "blue", method = smoother_method, se = F) +
  labs(y = "Вероятность") +
  theme_bw()


ggsave(file = "Response_to_Salinuty_Far_East.jpg")




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


library(patchwork)

Pl_var_importance/(Pl_Temperature + Pl_Salinity)

ggsave(file = "Var_importance_Response_to_Temperature_Salinuty_Far_East.jpg", dpi = 600)




  ############  Рисунки для демонстрации SDM ###########



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


# Читаем файл в фомате img с предсказаниями модели
predict_BTN <- brick("predictions_sdm_BTN_Far_East.img")

plot(predict_BTN)


# Загрузка береговой линии
# Загружаем данные береговой линии
coastline <- ne_coastline(scale = "medium", returnclass = "sf")

# Преобразуем в ту же проекцию, что и растр
coastline <- st_transform(coastline, crs = crs(predict_BTN))


# Получаем данные суши
land <- ne_countries(scale = "medium", returnclass = "sf")
land <- st_transform(land, crs(predict_BTN))

# 
raster_df <- as.data.frame(predict_BTN, xy = TRUE)

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
    xlim = c(120, 180),   
    ylim = c(40, 74)     
  ) +
  scale_fill_gradient(low = "white", high = "red")+
  labs(fill = "Probability") +
  theme_bw()+
  ggtitle("BTN1") +
  theme_map()



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
  ggtitle("BTN2")+
  theme_map()





# Наносим точки, где взяты пробы и обнаружены BTN

df_BTN_occur <- as.data.frame(BTN_occur)


# testing_df_presence <- 
#   testing_df %>% 
#   filter(BTN_presence == "yes")

library(ggnewscale)

Pl_predictions_BTN1 +
  geom_point(data = df_BTN_occur %>% filter(BTN1 == 1), aes(x = Lon, y = Lat), size = 4, shape = 21, stroke = 1, fill = "transparent", color = "black") + 
  geom_point(data = df_BTN_occur %>% filter(BTN1 != 1), aes(x = Lon, y = Lat), size = 2, shape = 21, fill = "white", color = "black") 


ggsave(file = "SDM_prediction_BTN1_Far_East.jpg", dpi = 400)


Pl_predictions_BTN2 +
  geom_point(data = df_BTN_occur %>% filter(BTN2 == 1), aes(x = Lon, y = Lat), size = 4, shape = 21,  stroke = 1, fill = "transparent", color = "black") + 
  geom_point(data = df_BTN_occur %>% filter(BTN2 != 1), aes(x = Lon, y = Lat), size = 2, shape = 21, fill = "white", color = "black")


ggsave(file = "SDM_prediction_BTN2_Far_East.jpg", dpi = 400)

df_BTN_occur %>% 
  summarise(BTN1 = mean(BTN1), BTN2 = mean(BTN2) )


train_predictions %>% 
  summarise(BTN1 = mean(ensemble_BTN1_weighted), BTN2 = mean(ensemble_BTN2_weighted) )
  
