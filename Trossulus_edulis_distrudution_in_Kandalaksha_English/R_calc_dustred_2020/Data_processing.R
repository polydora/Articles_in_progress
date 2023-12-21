library(ggplot2)
library(lme4)
library(glmmADMB)
library(reshape2)
library(dplyr)
library(patchwork)
library(broom)
library(broom.mixed)


##### Data reading
myt <- read.table("data/Distred_samples_fetch_corrected_2021.csv", header = T, sep = ";", dec = ",")

sal <- read.table("data/Distred_samples_salinity_2021.csv", header = T, sep = ";", dec = ",")

myt <- merge(myt, sal, all = T)

river_full <- read.table("data/Rivers_2021.csv", sep = ",", header = T)

river <- river_full %>% select(-Source)


ports <- data.frame(Shore = c("Kand", "Karel", "Kand", "Karel", "Karel", "Karel"), Port = c("Kandalaksha", "Vitino", "Umba", "Chupa", "Keret", "Kovda"), Status = c("Active", "Active", "Abandoned", "Abandoned", "Abandoned", "Abandoned"  ), Lat = c(67.137283, 67.076570,  66.677970, 66.269964, 66.294178, 66.696754), Lon = c(32.407995, 32.333630, 34.357655, 33.069534, 33.640656, 32.875396))


sites_fetch_df <- read.table("data/Distred_samples_fetch_values_2021.csv", sep = ",", header = T)


######################

nearest_dist <- function(XY, objects = river, x.name = "Lon", y.name = "Lat"){
  
  XY1 <-as.numeric(XY[,1])
  XY2 <- as.numeric(XY[,2])
  dist <- (acos(sin(XY1*pi/180)*sin(objects[ ,y.name]*pi/180) + cos(XY1*pi/180)*cos(objects[ ,y.name]*pi/180)*cos(XY2*pi/180 - objects[ ,x.name]*pi/180)) * 6371)
  
  MD <- data.frame(Min_dist = min(dist))
  cbind(objects[which(dist == min(dist)), ], MD)
  
}


# The distance to mouth of the neares river 

df_river <- nearest_dist(XY = myt[1, c("Lat", "Lon")], objects = river)

df_river[1,] <- NA

for(i in 1:nrow(myt)) {
  df_river[i,] <- nearest_dist(XY = myt[i, c("Lat", "Lon")], objects = river)
  df_river$Site[i] <- as.character(myt$Site)[i]
}


names(df_river) <- c( "Shore_river", "River",  "Drainage_Area", "River_Size", "Lat_river", "Lon_river", "Min_dist_river", "Site" )



# The distance to mouth of the neares Large river 

river_Large <- river %>% filter(River_Size == "Large")

df_river_Large <- nearest_dist(XY = myt[1, c("Lat", "Lon")], objects = river_Large)

df_river_Large[1,] <- NA

for(i in 1:nrow(myt)) {
  df_river_Large[i,] <- nearest_dist(XY = myt[i, c("Lat", "Lon")], objects = river_Large)
  df_river_Large$Site[i] <- as.character(myt$Site)[i]
}

names(df_river_Large) <- c( "Shore_river_Large", "River_Large",  "Drainage_Area_Large", "River_Size_Large", "Lat_river_Large", "Lon_river_Large", "Min_dist_river_Large", "Site" )



# The distance to the neares port 

df_port <- nearest_dist(XY = myt[1, c("Lat", "Lon")], objects = ports)

df_port[1,] <- NA

for(i in 1:nrow(myt)) {
  df_port[i,] <- nearest_dist(XY = myt[i, c("Lat", "Lon")], objects = ports)
  df_port$Site[i] <- as.character(myt$Site)[i]
}

names(df_port) <- c("Shore_port",  "Port","Port_Status", "Lat_port", "Lon_port", "Min_dist_port", "Site")

# Merging all data in one data set

d1 <- cbind(myt, df_river %>% select( - Site))

d2 <- cbind(d1, df_river_Large %>% select( - Site))

d3 <- cbind(d2, df_port %>% select(-Site))

d4 <- merge(d3, sites_fetch_df, by = "Site")

myt_full_all_data <- d4 %>% select(-River_Size_Large)


myt_full

# Создание датасета по тем сайтам из Канадалкшского залива, которые не вошли в основной training data set
myt_full_all_data %>% filter(! Site %in% unique(myt_full$Site)) %>% write.table(., "data/myt_White_Sea_testing.csv", sep =";", row.names = F, dec = ",")



