library(sdmpredictors)
library(leaflet)

# options(sdmpredictors_datadir="C:\\Users\\polyd\\AppData\\Local\\Temp\\RtmpuyXZRU/sdmpredictors")
list_datasets()

list_layer <- list_layers()

# Скачиваем данные по средней температуре воды на срденей глубине и срденей солености на срденей глубине. 
SST <- load_layers( layercodes = c("BO_sstmean") , equalarea=FALSE, rasterstack=TRUE)


my.sites <- data.frame(Site = myt$site, Lon=myt$Lon, Lat= myt$Lat)
my.sites


library(raster)

SST <- raster("D:/Data_LMBE/BIO_Oracle_predictors/wc2.1_10m_tmin_1960-08.tif")

my.sites.environment <- data.frame(Site = myt$site, Lat = myt$Lat, Lon = myt$Lon, extract(SST,my.sites[,2:3]))

names(my.sites.environment)[4] <- "SST"


Murm_map +
  geom_point(data = my.sites.environment, aes(x = Lon, y = Lat, group = 1, fill = SST), shape = 21, size = 4) +
  scale_fill_gradient(low = "yellow", high = "red")

ggplot(my.sites.environment, aes(x = Lon, y = SST)) + 
  geom_point()

myt_trmperature <- 
merge(myt, my.sites.environment)


myt_trmperature %>% 
  # filter(source == "Romanova" ) %>% 
  # filter(SST > 5) %>% 
  ggplot(aes(x = (SST), y = log(B_kg) )) +
  geom_point() +
  geom_smooth(method = "lm")


# myt_trmperature %>% 
#   filter(source == "Romanova" ) %>% 
#   filter(SST > 5) ->
#   training_df


library(mgcv)
Mod_myt_temp <- lm(log(B_kg) ~ (SST), data = training_df)

plot(Mod_myt_temp)






