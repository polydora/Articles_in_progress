library(sdmpredictors)
library(leaflet)

# options(sdmpredictors_datadir="C:\\Users\\polyd\\AppData\\Local\\Temp\\RtmpuyXZRU/sdmpredictors")
list_datasets()

list_layer <- list_layers()

# Скачиваем данные по средней температуре воды на срденей глубине и срденей солености на срденей глубине. 
environment.bottom <- load_layers( layercodes = c("BO2_salinitymean_bdmean") , equalarea=FALSE, rasterstack=TRUE)


SSS <- load_layers( layercodes = c("BO2_salinitymean_ss") , equalarea=FALSE, rasterstack=TRUE)


my.sites <- data.frame(Site = myt_`site$Site, Lon=myt_site$Lon, Lat= myt_site$Lat)
my.sites


library(raster)

SSS <- raster("D:/Data_LMBE/BIO_Oracle_predictors/Present.Surface.Salinity.Lt.min.tif")

my.sites.environment <- data.frame(Site = myt_site$Site, Lat = myt_site$Lat, Lon = myt_site$Lon, extract(SSS,my.sites[,2:3]) )

