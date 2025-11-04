library(sdm)
library(raster)
library(rgdal)







# file <- system.file("external/species.shp", package="sdm") # get the location of the species shapefile 
# so, file is simply a filename 


all_points_predictors %>% 
  dplyr::select(Lon, Lat, Out) %>%
  filter(complete.cases(.)) ->
  BTN_occur
  

coordinates(BTN_occur) <- ~ Lon + Lat
proj4string(BTN_occur) <- CRS("+init=epsg:4326")  # WGS84

# class(BTN_occur)
# 
# plot(BTN_occur)


# species <- shapefile(file)

species <- BTN_occur

# class(species)
# 
# head(species)


# path <- system.file("external", package="sdm") # path to the folder contains the data

lst <- list.files(path="D:/Data_LMBE/BIO_Oracle_predictors/",pattern='asc$',full.names = T) # list the name of files in the specified


preds <- stack(lst)

names(preds) <- c(
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



# plot(preds)


plot(preds[[2]]) # only plot the 4th layer
plot(species,add=T)



# points_buffer <- buffer(BTN_occur, width = 1)  # ширина в градусах
# 
# # Вырезаем область вокруг точек
# preds_cropped <- crop(preds, points_buffer)
# plot(preds_cropped[[2]])


my_extent <- extent(100, 180, 30, 80)  # например: долгота 10-20°, широта 35-45°

# Вырезаем область
preds_cropped <- crop(preds, my_extent)

plot(preds_cropped[[11]])




library(raster)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

# my_extent <- extent(100, 180, 40, 80)
# preds_cropped <- crop(preds, my_extent)

# Загружаем данные береговой линии
coastline <- ne_coastline(scale = "medium", returnclass = "sf")
plot(coastline)

# Преобразуем в ту же проекцию, что и растр
coastline <- st_transform(coastline, crs = crs(preds_cropped))

# Создаем буфер вокруг береговой линии
buffer_distance <- 10  # расстояние в градусах (настройте под ваш масштаб)
coastline_buffer <- st_buffer(coastline, dist = buffer_distance)

# Конвертируем в Spatial объект для работы с raster
coastline_buffer_sp <- as(coastline_buffer, "Spatial")

# plot(coastline_buffer_sp)

# Создаем маску береговой линии
coast_mask <- rasterize(coastline_buffer_sp, preds_cropped[[1]], field = 1)

# Выделяем пиксели вблизи береговой линии
preds_coastal <- mask(preds_cropped, coast_mask)

# Визуализация
plot(preds_coastal[[1]], main = "Пиксели вдоль береговой линии")
plot(coastline$geometry, add = TRUE, col = "red")



##################Использование расстояния до берега####################3

library(raster)
library(rnaturalearth)
library(gdistance)

my_extent <- extent(100, 180, 40, 80)
preds_cropped <- crop(preds, my_extent)

# Получаем данные суши
land <- ne_countries(scale = "medium", returnclass = "sf")
land <- st_transform(land, crs(preds_cropped))

# Создаем бинарный растр (1 = земля, 0 = море)
land_binary <- rasterize(land, preds_cropped[[1]], field = 1, background = NA)

# Вычисляем расстояние до берега для каждого морского пикселя
# Используем функцию distance
dist_to_shore <- distance(land_binary)

# Задаем максимальное расстояние от берега (в метрах или градусах)
max_distance_km <- 50  # 50 км от берега
max_distance_deg <- max_distance_km / 111  # приблизительно в градусах

# Создаем маску для прибрежной зоны
coastal_mask <- dist_to_shore <= max_distance_deg
coastal_mask[land_binary == 1] <- NA  # убираем сушу
coastal_mask[coastal_mask == 0] <- NA  # убираем области вне радиуса

# Применяем маску
preds_coastal <- mask(preds_cropped, coastal_mask)

# Визуализация
plot(preds_coastal[[1]], main = paste("Прибрежная зона", max_distance_km, "км"))
plot(st_geometry(land), add = TRUE, col = "darkgreen")





##### Рисуем в ggplot ##################

library(ggplot2)
library(raster)
library(viridis) 

raster_df <- as.data.frame(preds_cropped[[2]], xy = TRUE)

# Создаем карту в ggplot
ggplot() +
  geom_raster(data = raster_df, aes(x = x, y = y, fill = Current)) +
  geom_sf(data = coastline) +
  coord_sf(
    xlim = c(100, 180),   
    ylim = c(30, 80)     
  ) + 
   scale_fill_viridis(na.value = "transparent") 

  



##############################


# Получаем береговую линию
coastline <- ne_coastline(scale = "medium", returnclass = "sf")
coastline <- st_transform(coastline, crs = crs(preds_cropped))

# Создаем растр расстояния до береговой линии
# Конвертируем береговую линию в Spatial объект
coastline_sp <- as(coastline, "Spatial")

# Создаем пустой растр с теми же характеристиками
template_raster <- preds_cropped[[2]]
template_raster[] <- 1

# plot(template_raster)

# Растеризуем береговую линию (1 = берег, NA = остальное)
coast_raster <- rasterize(coastline_sp, template_raster, field = 1, background = NA)

# Вычисляем расстояние до береговой линии (в метрах)
distance_raster <- distance(coast_raster)

# Задаем максимальное расстояние от берега (в метрах)
max_distance <- 50000  # 50 км

# Создаем маску: TRUE - пиксели в пределах расстояния, FALSE - дальше
coastal_mask <- distance_raster <= max_distance

# Применяем маску к данным
coastal_data <- mask(preds_cropped[[2]], coastal_mask)

# Преобразуем в data.frame для ggplot
raster_df <- as.data.frame(coastal_data, xy = TRUE)
colnames(raster_df)[3] <- "Current"  # переименовываем столбец





# d <- sdmData(formula=Occurrence~., train=species, predictors=preds)

d <- sdmData(train=species, predictors=preds)

# installAll()

m1 <- sdm(Out ~., data=d, methods=c('glm','bioclim','brt', 'rf'))
# 
# m1 <- sdm(Out ~., data=d, methods=c('rf'))


m1

getModelInfo(m1)

roc(m1)

# m2 <- sdm(Out ~., data=d,methods=c('rf','brt','bioclim','glm'),replicatin='sub',test.percent=30,n=2)
#           
# m2



# getModelInfo(m2)


# roc(m2)

# roc(m2,smooth=TRUE)


p1 <- predict(m1,newdata=preds,filename='p2.img') # many commonly used raster format is supported (throplot(p1)

plot(p1)


getModelObject(m1)  # получаем оригинальный объект модели
summary(getModelObject(m1))

# Или через sdm
getVarImp(m1)  # важность переменных


vi <- getVarImp(m1)
vi
plot(vi)


r3 <- curve(m1, smooth = TRUE, main = "Response Curves")

vi@varImportance$AUCtest  # через AUC
vi@varImportance$corTest  # через корреляцию

r2 <- curve(m1, id = 1:3)  # первые 3 предиктора
plot(r2)

anova(m1)
pd <- partial(m1)
plot(pd)
