library(ncdf4)
library(lubridate)
library(dplyr)
library(ggplot2)



# При условии гидротехнически сооружений ################### 

path = "D:/Data_LMBE/Obskaya Bay additional data/nc_files_from_model/After_construction_building"

files <- list.files(path)

files_selected <- files[round(seq(1,365, length.out = 365))]
pb = txtProgressBar(min = 0, max = length(files_selected), initial = 0) 

hotspot <- read.table("Data/Obskaya_bay_ports.csv", sep = ",", header = T) 


df_hotspot <- NULL
i = 0
for(name in files_selected){
  i <- i +1
  ncin <- nc_open(paste(path, "/",name, sep = ""))
  # Широта и долгота точек в модели
  lon <- ncvar_get(ncin,"lon")
  lat <- ncvar_get(ncin,"lat")
  
  # Дата прдесказания модели
  time <- ncvar_get(ncin,"time")
  date_from_model <- as.POSIXct(time, origin = "1900-01-01 00:00:00")
  
  
  # Извлекаем температуру
  
  ## Третье измерение в массиве - это Горизонты глубины: 0, 5, 10, 15, 20 м
  
  tmp_array <- ncvar_get(ncin,"temp")
  
  tmp_slice_0 <- tmp_array[,,1]
  tmp_slice_5 <- tmp_array[,,2]
  tmp_slice_10 <- tmp_array[,,3]
  tmp_slice_15 <- tmp_array[,,4]
  tmp_slice_20 <- tmp_array[,,5]
  
  
  
  # Извлекаем Соленость
  
  salt_array <- ncvar_get(ncin,"salt")
  
  salt_slice_0 <- salt_array[,,1]
  salt_slice_5 <- salt_array[,,2]
  salt_slice_10 <- salt_array[,,3]
  salt_slice_15 <- salt_array[,,4]
  salt_slice_20 <- salt_array[,,5]
  
  # Формируем датафрейм
  
  lonlat <- as.matrix(expand.grid(lon = lon,lat = lat))
  
  
  df_model <- data.frame(cbind(lonlat, T_0 = as.vector(tmp_slice_0), T_5 = as.vector(tmp_slice_5), T_10 = as.vector(tmp_slice_10), T_15 = as.vector(tmp_slice_15), T_20 = as.vector(tmp_slice_20), S_0 = as.vector(salt_slice_0), S_5 = as.vector(salt_slice_5), S_10 = as.vector(salt_slice_10), S_15 = as.vector(salt_slice_15), S_20 = as.vector(salt_slice_20)), Date = date_from_model)
  
  df_model <- df_model %>% filter(!is.na(T_0))
  
  df_model <- df_model %>% mutate(
    Dist_Sabetta = sqrt( (lon - hotspot$Lon[1])^2 + (lat - hotspot$Lat[1])^2 ), 
    Dist_Terminal = sqrt((lon - hotspot$Lon[2])^2 + (lat - hotspot$Lat[2])^2 ) 
    ) 
  
  # Отбор точек ближайшх к портам
  df_model <- df_model %>% filter(Dist_Sabetta <= quantile(Dist_Sabetta, probs = 0.001) | Dist_Terminal <= quantile(Dist_Terminal, probs = 0.001) )
  
  # df_model <- df_model[c(which.min(df_model$Dist_Sabetta), which.min(df_model$Dist_Terminal)), ]
    
    
  df_hotspot <- rbind(df_hotspot, df_model)
  setTxtProgressBar(pb,i)
  
}


nrow(df_hotspot)
# ggplot(df_hotspot, aes(x = lon, y = lat)) + geom_point()


df_hotspot_construction <- df_hotspot




# Без гидротехнически сооружений ################### 

path = "D:/Data_LMBE/Obskaya Bay additional data/nc_files_from_model/No_construction_building/"

files <- list.files(path)

files_selected <- files[round(seq(1,365, length.out = 365))]
pb = txtProgressBar(min = 0, max = length(files_selected), initial = 0) 

hotspot <- read.table("Data/Obskaya_bay_ports.csv", sep = ",", header = T) 


df_hotspot <- NULL
i = 0
for(name in files_selected){
  i <- i +1
  ncin <- nc_open(paste(path, "/",name, sep = ""))
  # Широта и долгота точек в модели
  lon <- ncvar_get(ncin,"lon")
  lat <- ncvar_get(ncin,"lat")
  
  # Дата прдесказания модели
  time <- ncvar_get(ncin,"time")
  date_from_model <- as.POSIXct(time, origin = "1900-01-01 00:00:00")
  
  
  # Извлекаем температуру
  
  ## Третье измерение в массиве - это Горизонты глубины: 0, 5, 10, 15, 20 м
  
  tmp_array <- ncvar_get(ncin,"temp")
  
  tmp_slice_0 <- tmp_array[,,1]
  tmp_slice_5 <- tmp_array[,,2]
  tmp_slice_10 <- tmp_array[,,3]
  tmp_slice_15 <- tmp_array[,,4]
  tmp_slice_20 <- tmp_array[,,5]
  
  
  
  # Извлекаем Соленость
  
  salt_array <- ncvar_get(ncin,"salt")
  
  salt_slice_0 <- salt_array[,,1]
  salt_slice_5 <- salt_array[,,2]
  salt_slice_10 <- salt_array[,,3]
  salt_slice_15 <- salt_array[,,4]
  salt_slice_20 <- salt_array[,,5]
  
  # Формируем датафрейм
  
  lonlat <- as.matrix(expand.grid(lon = lon,lat = lat))
  
  
  df_model <- data.frame(cbind(lonlat, T_0 = as.vector(tmp_slice_0), T_5 = as.vector(tmp_slice_5), T_10 = as.vector(tmp_slice_10), T_15 = as.vector(tmp_slice_15), T_20 = as.vector(tmp_slice_20), S_0 = as.vector(salt_slice_0), S_5 = as.vector(salt_slice_5), S_10 = as.vector(salt_slice_10), S_15 = as.vector(salt_slice_15), S_20 = as.vector(salt_slice_20)), Date = date_from_model)
  
  df_model <- df_model %>% filter(!is.na(T_0))
  
  df_model <- df_model %>% mutate(
    Dist_Sabetta = sqrt( (lon - hotspot$Lon[1])^2 + (lat - hotspot$Lat[1])^2 ), 
    Dist_Terminal = sqrt((lon - hotspot$Lon[2])^2 + (lat - hotspot$Lat[2])^2 ) 
  ) 
  
  # Отбор точек ближайшх к портам
  df_model <- df_model %>% filter(Dist_Sabetta <= quantile(Dist_Sabetta, probs = 0.001) | Dist_Terminal <= quantile(Dist_Terminal, probs = 0.001) )
  
  
  # df_model <- df_model[c(which.min(df_model$Dist_Sabetta), which.min(df_model$Dist_Terminal)), ]
  
  
  df_hotspot <- rbind(df_hotspot, df_model)
  setTxtProgressBar(pb,i)
  
}


nrow(df_hotspot)

df_hotspot_no_construction <- df_hotspot


# Merging both data sets

df_hotspot_no_construction$Scenario <- "No construction"

df_hotspot_construction$Scenario <- "Construction present"


df_hotspot_2 <- rbind(df_hotspot_no_construction, df_hotspot_construction)








theme_set(theme_bw())

df_hotspot_2 <- df_hotspot_2  %>% mutate(Temp = rowMeans(select(., starts_with("T_")), na.rm = TRUE), Sal = rowMeans(select(., starts_with("S_")), na.rm = TRUE))

df_hotspot_2 %>% filter(Scenario == "Construction present") %>% 
  ggplot(., aes(x = Temp, y = Sal)) + geom_point(size =1, alpha = 0.2)  + geom_density2d(n = 1000) 



str(df_hotspot_2)


df_hotspot_2$Month <- month(as.POSIXlt(df_hotspot_2$Date, format="%Y-%m-%d"))

 
df_hotspot_2$Season <- case_when(df_hotspot_2$Month %in% c(12, 1, 2) ~ "Winter",
            df_hotspot_2$Month %in% c(3, 4, 5) ~ "Spring",
            df_hotspot_2$Month %in% c(6, 7, 8) ~ "Summer",
            df_hotspot_2$Month %in% c(9, 10, 11) ~ "Autumn")



df_hotspot_2$Season <- factor(df_hotspot_2$Season, levels = c("Winter", "Spring", "Summer", "Autumn")) 

hist(df_hotspot_2$Dist_Sabetta)
hist(df_hotspot_2$Dist_Terminal)


df_hotspot_2$Port <- case_when(df_hotspot_2$Dist_Sabetta < 1  ~ "Sabetta",
                                 df_hotspot_2$Dist_Sabetta > 1 ~ "Terminal")



ggplot(df_hotspot_2, aes(x = Scenario, y = Sal)) + geom_boxplot(notch = T) + facet_grid(Season ~ Port)

ggplot(df_hotspot_2, aes(x = Scenario, y = Temp )) + geom_boxplot(notch = F) + facet_grid(Season~ Port)

df_hotspot_2  %>%  group_by(Season, Scenario) %>% summarise(Mean_Sal = mean(Sal), Mean_Temp = mean(Temp))



