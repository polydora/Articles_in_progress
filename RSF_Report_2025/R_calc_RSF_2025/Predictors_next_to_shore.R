# В этом скрипте создается датасет с морскими предикторами для каждого пикселя для прибрежной полосы

library(sf)

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



my_extent <- extent(100, 180, 40, 80) # гранцы для дальневосточных сборов
# my_extent2 <- extent(-20, 45, 30, 80) # Для Европы


preds_cropped <- crop(preds, my_extent)

plot(preds_cropped[[11]])



library(raster)
library(rnaturalearth)
library(rnaturalearthdata)
library(gdistance)

# Получаем данные суши
land <- ne_countries(scale = "medium", returnclass = "sf")
land <- st_transform(land, crs(preds_cropped))

# Создаем бинарный растр (1 = земля, 0 = море)
land_binary <- rasterize(land, preds_cropped[[1]], field = 1, background = NA)

plot(land_binary)
# Вычисляем расстояние до берега для каждого морского пикселя
# Используем функцию distance
# dist_to_shore <- distance(land_binary)



load("Data/Dist_to_Shore_Far_East.RData")

dist_to_shore

plot(dist_to_shore)

# Задаем максимальное расстояние от берега (в метрах или градусах)
max_distance_m <- 50000  # 50 км от берега

# Создаем маску для прибрежной зоны
coastal_mask <- dist_to_shore <= max_distance_m

coastal_mask[land_binary == 1] <- NA  # убираем сушу

plot(coastal_mask)

coastal_mask[coastal_mask == 0] <- NA  # убираем области вне радиуса

plot(coastal_mask)

library(terra)
# Применяем маску
preds_coastal <- mask(preds_cropped, coastal_mask)

plot(preds_coastal[[11]])

writeRaster(preds_coastal, "preds_coastal_Far_East_50_km.tif")

