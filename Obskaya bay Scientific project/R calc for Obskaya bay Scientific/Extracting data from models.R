
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



n <- nrow(bent_short) # Число станций 
colnames(xycoords) = c("x-coordinate","y-coordinate")
rownames(xycoords) = 1:n






# При условии гидротехнически сооружений ################### 

path = "D:/Data_LMBE/Obskaya Bay additional data/nc_files_from_model/No_construction_building"

files <- list.files(path)

files_selected <- "inmom_20170901.nc" 
  
name = paste(path, "/", files_selected, sep = "")
  
# files[round(seq(1,365, length.out = 365))]

# pb = txtProgressBar(min = 0, max = length(files_selected), initial = 0) 

# hotspot <- read.table("Data/Obskaya_bay_ports.csv", sep = ",", header = T) 


ncin <- nc_open(name)


# library(conflicted)
# conflict_prefer(name = "select", winner = "dplyr")

library(raster)

# Вытягиваем из nc файла данные по слоям 
Sal_layer <- brick(name, var="salt")
Sea_level_layer <- brick(name, var="ssh")

Lat_layer <- brick(name, var="latitude_UTM_43N")
Lon_layer <- brick(name, var="longitude_UTM_43N")


Curent_layer <- brick(name, var="vocn")



Salinity_model <- extract(Sal_layer,xycoords)
Sea_level_model <- extract(Sea_level_layer,as.data.frame(xycoords))
Lat_model <- extract(Lat_layer,as.data.frame(xycoords), method = "bilinear")
Lon_model <- extract(Lon_layer,as.data.frame(xycoords))
Curent_model <- extract(Curent_layer,as.data.frame(xycoords))




plot( stat_included$Bottom_Salinity_Sep_20, (Salinity_model[, 1]) )

qplot( stat_included$Long, (Lon_model/10000) )+ geom_abline(slope = 1) 

qplot( stat_included$Lat, (Lat_model/100000) ) 


qplot(Curent_model[,1], stat_included$Bottom_Susp_Sep_20) 

