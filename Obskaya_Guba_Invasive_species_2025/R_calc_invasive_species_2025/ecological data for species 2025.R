
library(dplyr)


# Загрузка данных с координатами встреч видов






## Дополнительные PNIS 2025
# Сводим все встречи дополнительных PNIS в один датафрейм

path <- "Data/Additional_PNIS_Phytoplancton/"
  

file_list <- dir(path)


df_all_PNIS <- NULL
  
  
for(file_name in file_list){
  df <- read.table(file = paste(path, file_name, sep =""), sep = ";", header = T)
  df_all_PNIS <- rbind(df_all_PNIS, df)
  print(file_name)
}


# unique(df_all_PNIS$species)

df_species <- df_all_PNIS



df_species$lon <- as.numeric(df_species$lon)
df_species$lat <- as.numeric(df_species$lat)
df_species <- df_species[complete.cases(df_species), ]


  
library(sdmpredictors)
library(leaflet)

# options(sdmpredictors_datadir="C:\\Users\\polyd\\AppData\\Local\\Temp\\RtmpuyXZRU/sdmpredictors")


# Скачиваем данные по средней температуре воды на срденей глубине и срденей солености на срденей глубине. 


# library(sdmpredictors)
# library(leaflet)



# devtools::install_github("bio-oracle/biooracler")

# options(sdmpredictors_datadir="C:\\Users\\polyd\\AppData\\Local\\Temp\\RtmpuyXZRU/sdmpredictors")

# Att! Изменился формат функций для скачивания данных по слоям см. https://github.com/bio-oracle/biooracler


# library(biooracler)
# 
# df_layers_discription <- list_layers(simplify = T)

# Скачиваем данные по средней температуре воды на срденей глубине и срденей солености на срденей глубине из старых слоев BIO-ORACLE

library(raster)

salinity = raster("D:/Data_LMBE/BIO_Oracle_predictors/Present.Surface.Salinity.Mean.asc")
temp = raster("D:/Data_LMBE/BIO_Oracle_predictors/Present.Surface.Temperature.Mean.asc")






# environment.bottom <- load_layers( layercodes = c("thetao_baseline_2000_2019_depthmean" ,  "	
# so_baseline_2000_2019_depthmean") , equalarea=FALSE, rasterstack=TRUE, datadir = NULL)



# bathymetry <- load_layers("BO_bathymean")


my.sites <- data.frame(species = df_species$species, Lon=df_species$lon, Lat= df_species$lat)
my.sites

my.sites_points <- as.matrix(my.sites[,-1])


my.sites.environment <- data.frame(species = my.sites$species, Lat = my.sites$Lat, Lon = my.sites$Lon, Temp = extract(temp,my.sites_points), Sal = extract(salinity, my.sites_points) )


dat_for_species <-
my.sites.environment %>% 
  group_by(species) %>% 
  summarise(Marine = sum(!is.na(Sal)), Not_Marine = sum(is.na(Sal) & is.na(Temp))) %>% 
  mutate(Not_Marine_Prop = Not_Marine/(Marine + Not_Marine)) %>% 
  arrange(Not_Marine_Prop)


dat_for_species %>% 
  filter(Not_Marine_Prop >= 0.5) %>% 
  pull(species) -> 
  not_marine_species


my.sites.environment_marine <- 
  my.sites.environment %>%  
  filter(!is.na(Temp) & !is.na(Sal))

names(my.sites.environment_marine) <- c("species", "Lat", "Lon", "Temp", "Sal")

# Записываем данные по встречаемостям PNIS и температуре и солености в морских местообитаниях
write.csv(my.sites.environment_marine, "Data/additional_PNIS_Phytoplancton_2025_marine.csv")






my.sites.environment_not_marine <- 
  my.sites.environment %>%  
  filter(is.na(Temp) & is.na(Sal))

# my.sites.environment_not_marine <-
#   my.sites.environment_not_marine %>% 
#   filter(species %in% not_marine_species)

nrow(my.sites.environment_not_marine)


my.sites.environment_not_marine <- my.sites.environment_not_marine[,1:3] 

# write.csv(my.sites.environment_not_marine, "Data/plancton_occurence_not_marine_2.csv")




# Оценка температуры для тех точек, которые предположитеьно не являются морскими

# dir <- "D:/Data_LMBE/Obskaya Bay additional data/freshwater_variables/"
# dir.create(dir)
# setwd(dir)

library(raster); 
library(ncdf4); 
library(maps); 
library(foreach); 
library(doParallel)

temp_avg <- brick("D:/Data_LMBE/Obskaya Bay additional data/freshwater_variables/monthly_tmin_average.nc")

### Check the number of layers
nlayers(temp_avg)


### Check the metadata for units, scale factors etc.
nc_open("monthly_tmin_average.nc")

### Add layer names. See Table S1 or the ReadMe for the sequence of the single layers 
names(temp_avg) <- paste(c("temp_avg"), sprintf("%02d", seq(1:12)), sep="_")


library(raster)

# extract(temp_avg,my.sites.environment_not_marine[1000:1200,2:3])

my.sites.stream <- data.frame(species = my.sites.environment_not_marine$species, extract(temp_avg,my.sites.environment_not_marine[,2:3]))

my.sites.stream_cleaned <- cbind(my.sites.environment_not_marine, my.sites.stream[,-1])



my.sites.stream_cleaned <- 
  my.sites.stream_cleaned %>% 
  filter(!is.na(temp_avg_01) + !is.na(temp_avg_02) + !is.na(temp_avg_03) + !is.na(temp_avg_04) + !is.na(temp_avg_05) + !is.na(temp_avg_06) + !is.na(temp_avg_07) + !is.na(temp_avg_08) + !is.na(temp_avg_09) + !is.na(temp_avg_10) + !is.na(temp_avg_11) + !is.na(temp_avg_12) == 0)

my.sites.stream_cleaned[,-c(1:3)] <- my.sites.stream_cleaned[,-c(1:3)]/10

my.sites.stream_cleaned_final <- my.sites.stream_cleaned[,c(1:3)]
my.sites.stream_cleaned_final$Temp <- rowMeans(my.sites.stream_cleaned[,-c(1:3)])
my.sites.stream_cleaned_final$Sal <- 0
  
  
  
write.table(my.sites.stream_cleaned_final, file = "clipboard", sep = "\t")


####### Получение данных для Обской губы для будущего для точек, соответствующих станциям 2020 года в Обской губе

library(raster); 
library(ncdf4); 
library(maps); 
library(foreach); 
library(doParallel)
library(readxl)


stations_2020 <- read_excel("Data/Obskaya_bay_2020.xlsx", sheet = "Station parameters")

my.sites <- 
  stations_2020 %>% 
  dplyr::select(Station, Long, Lat)
  

temp <- brick("D:/Data_LMBE/BIO_Oracle_predictors/Future.Temperature.Mean.Depth.nc")
sal <- brick("D:/Data_LMBE/BIO_Oracle_predictors/Future.Salinity.Mean.Depth.nc")


ob_temp_sal_future_data <- 
  data.frame(forecast_points, Temp = extract(temp,forecast_points), Sal = extract(sal,forecast_points))

names(ob_temp_sal_future_data) <- c("lon", "lat", "Future_Temp", "Future_Sal")

ggplot(ob_temp_sal_future_data, aes(x = Future_Temp, y = Future_Sal)) + 
  geom_point()
