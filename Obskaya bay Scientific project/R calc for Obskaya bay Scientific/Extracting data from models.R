
library(ncdf4)
library(lubridate)
library(dplyr)
library(ggplot2)
library(readxl)



# detach("package:raster", unload = TRUE)

# All Station position
stations <- read_excel("Data/Obskaya_bay_2020.xlsx", sheet = "Station parameters", na = "NA")

# Stations with biological samples
stat_full <- stations %>% filter(Exclude == 0)

stat_full <- as.data.frame(stat_full)




# Data on benthic communities ######
bent <- read_excel("Data/Obskaya_bay_2020.xlsx", sheet = "Benthos")


bent_B <- bent %>% filter(Type == "Biomass") %>% select(-Type)

bent_N <- bent %>% filter(Type == "Abundance") %>% select(-Type)


Spec_abund <- bent_N %>% select(-Station) %>% colSums(.) %>% t() %>% as.vector() 

spec_total_abund <- data.frame(Sp = bent_N %>% select(-Station) %>% colnames(), Spec_abund)

N_porog <- 200 

spec_selected <- spec_total_abund %>% filter(Spec_abund > N_porog)

bent_short <- bent_N %>% select(Station, spec_selected$Sp)


bent_short <-
  bent_short %>% mutate(Oligochaeta = Oligochaeta_gen._sp. + Oligochaeta_gen._spp.) %>% 
  select(-c(Oligochaeta_gen._sp., Oligochaeta_gen._spp., Oligochaeta_gen._sp._cocons,Senecella_siberica) )


xycoords <- stat_full %>% select(Station, Long, Lat) %>% filter(Station %in% bent_short$Station) %>% select(-Station) %>% as.matrix(.)


stat_included <- stat_full %>%  filter(Station %in% bent_short$Station) 

stat_included_coord <- stat_included[,1:3]

stat_included_coord$Depth <- with(stat_included, (Depth_aug_20 + Depth_sep_20)/2)


 

n <- nrow(bent_short) # Число станций 
colnames(xycoords) = c("x-coordinate","y-coordinate")
rownames(xycoords) = 1:n






# При условии, что нет гидротехнических сооружений ################### 

path = "D:/Data_LMBE/Obskaya Bay additional data/nc_files_from_model/No_construction_building"

files <- list.files(path)

# Отбираем только данные по августу-сентябрю
files_selected <- files #[305:365]  
 

# Формируем датафрейм с данными по каждой станции для сценария без гидротехнических сооружений

library(raster)

no_construction_salinity <- NULL

for(name in files_selected){
  name = paste(path, "/", name, sep = "")
  Sal_layer <- brick(name, var="salt")
  Salinity_model <- as.data.frame(extract(Sal_layer,xycoords))
  
  ncin <- nc_open(name)
  time <- ncvar_get(ncin,"time")
  date_from_model <- as.POSIXct(time, origin = "1900-01-01 00:00:00")
  
  stat_included_coord$Date <- date_from_model
  stat_included_coord$Scenario <- "No"
  df <- cbind(stat_included_coord, Salinity_model)
  no_construction_salinity <- rbind(no_construction_salinity, df)
  print(name)
}




# При условии, что ЕСТЬ гидротехнические сооружения ################### 

path = "D:/Data_LMBE/Obskaya Bay additional data/nc_files_from_model/After_construction_building"

files <- list.files(path)

# Отбираем только данные по августу-сентябрю
files_selected <- files #[305:365]  


# Формируем датафрейм с данными по каждой станции для сценария, когда построены гидротехнические сооружения

library(raster)

present_construction_salinity <- NULL

for(name in files_selected){
  name = paste(path, "/", name, sep = "")
  Sal_layer <- brick(name, var="salt")
  Salinity_model <- as.data.frame(extract(Sal_layer,xycoords))
  
  ncin <- nc_open(name)
  time <- ncvar_get(ncin,"time")
  date_from_model <- as.POSIXct(time, origin = "1900-01-01 00:00:00")
  
  stat_included_coord$Date <- date_from_model
  stat_included_coord$Scenario <- "Constructed"
  df <- cbind(stat_included_coord, Salinity_model)
  present_construction_salinity <- rbind(present_construction_salinity, df)
  print(name)
}


present_construction_salinity



#######################

# detach("package:RStoolbox", unload = TRUE)
# detach("package:raster", unload = TRUE)


all_model_data <- rbind(no_construction_salinity, present_construction_salinity)



all_model_data <-
  all_model_data %>% mutate(Sal = case_when(Depth < 2.5 ~ X1, 
                                          Depth >= 2.5 & Depth < 7.5 ~ X5,
                                          Depth >= 7.5 & Depth < 12.5 ~ X10,
                                          Depth >= 12.5 & Depth <17.5 ~ X15, 
                                          Depth >= 17.5 ~ X20)) 
  


all_model_data <-
  all_model_data %>% rowwise() %>% mutate(Sal_mean = mean(c(X1, X5, X10, X15, X20), na.rm = T)) 


ggplot(all_model_data, aes(x = Sal, y = Sal_mean)) + geom_point() + geom_abline()

all_model_data$Sal_predicted<- all_model_data$Sal

all_model_data$Sal_predicted [is.na(all_model_data$Sal)] = all_model_data$Sal_mean[is.na(all_model_data$Sal)]


all_model_data %>% filter(is.nan(Sal_predicted)) %>% pull(Station) %>% table()





all_model_data_diff <- all_model_data %>% select(Station, Long, Lat, Scenario, Depth, Date, Sal_predicted)


library(reshape2)

all_model_data_diff <- dcast(Station + Long + Lat + Depth + Date ~ Scenario, data = all_model_data_diff, value.var = "Sal_predicted" )


all_model_data_diff <-
all_model_data_diff %>% mutate(R_Sal = (Constructed - No) )

all_model_data_diff_mean <- all_model_data_diff  %>% group_by(Station) %>% summarize(Long = mean(Long), Lat = mean(Lat), R_Sal_mean = mean(R_Sal, na.rm = T), R_Sal_sd = sd(R_Sal, na.rm = T)) 


hist(all_model_data_diff_mean$R_Sal_mean)

ggplot(all_model_data_diff_mean, aes(x = Long, y= Lat, color = R_Sal_mean)) + geom_point(size = 4) + scale_color_gradient(low = "yellow", high = "red") 


qplot(stat_included$Bottom_Salinity_Aug_20, stat_included$Bottom_Salinity_Aug_20 + all_model_data_diff_mean$R_Sal_mean) + geom_abline()
