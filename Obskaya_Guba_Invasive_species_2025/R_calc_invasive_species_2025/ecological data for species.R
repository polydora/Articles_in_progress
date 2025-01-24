
library(dplyr)


# Загрузка данных с координатами встреч видов


df_species <- read.csv("Data/native_and_PNIS_occurence.csv")
df_species <- unique(df_species)

df_species$lon <- as.numeric(df_species$lon)
df_species$lat <- as.numeric(df_species$lat)
df_species <- df_species[complete.cases(df_species), ]


native_species <- c("Limnodrilus hoffmeisteri", 
                    "Mysis relicta",
                    "Halicryptus spinulosus",
                    "Saduria entomon",
                    "Saduria sabini",
                    "Saduria sibirica",
                    "Gammaracanthus lacustris",
                    "Gammaracanthus",
                    "Monoporeia affinis",
                    "Pontoporeia femorata",
                    "Portlandia aestuariorum",
                    "Marenzelleria",
                    "Ampharete vega",
                    "Mysis oculata"
)



df_species$Status <- ifelse(df_species$species %in% native_species, "Native", "PNIS") 






## Plancton

df_species <- read.csv("Data/plancton_occurence_all_final.csv")

df_species <- df_species %>% filter(species == "Acartia bifilosa") 

df_species <- unique(df_species)

# write.csv(df_species, "Data/Acrtia_bifilosa.csv")



df_species$lon <- as.numeric(df_species$lon)
df_species$lat <- as.numeric(df_species$lat)
df_species <- df_species[complete.cases(df_species), ]


  
library(sdmpredictors)
library(leaflet)

# options(sdmpredictors_datadir="C:\\Users\\polyd\\AppData\\Local\\Temp\\RtmpuyXZRU/sdmpredictors")


# Скачиваем данные по средней температуре воды на срденей глубине и срденей солености на срденей глубине. 
environment.bottom <- load_layers( layercodes = c("BO2_tempmean_bdmean" ,  "BO2_salinitymean_bdmean") , equalarea=FALSE, rasterstack=TRUE)



# bathymetry <- load_layers("BO_bathymean")


my.sites <- data.frame(species = df_species$species, Lon=df_species$lon, Lat= df_species$lat)
my.sites



library(raster)
my.sites.environment <- data.frame(species = my.sites$species, Lat = my.sites$Lat, Lon = my.sites$Lon, extract(environment.bottom,my.sites[,2:3]) )

my.sites.environment_marine <- my.sites.environment %>%  filter(!is.na(BO2_tempmean_bdmean) & !is.na(BO2_salinitymean_bdmean))

names(my.sites.environment_marine) <- c("species", "Lat", "Lon", "Temp", "Sal")

write.csv(my.sites.environment_marine, "Data/Acartia_environment_marine.csv")


my.sites.environment_not_marine <- my.sites.environment %>%  filter(is.na(BO2_tempmean_bdmean) & is.na(BO2_salinitymean_bdmean))

my.sites.environment_not_marine <- my.sites.environment_not_marine[,1:3] 

write.csv(my.sites.environment_not_marine, "Data/plancton_occurence_not_marine.csv")




####################################

df_species <- read.csv("Data/plancton_occurence_2.csv")
df_species <- unique(df_species)

df_species$lon <- as.numeric(df_species$lon)
df_species$lat <- as.numeric(df_species$lat)
df_species <- df_species[complete.cases(df_species), ]



library(sdmpredictors)
library(leaflet)

# options(sdmpredictors_datadir="C:\\Users\\polyd\\AppData\\Local\\Temp\\RtmpuyXZRU/sdmpredictors")


# Скачиваем данные по средней температуре воды на срденей глубине и срденей солености на срденей глубине. 
environment.bottom <- load_layers( layercodes = c("BO2_tempmean_bdmean" ,  "BO2_salinitymean_bdmean") , equalarea=FALSE, rasterstack=TRUE, datadir = NULL)



# bathymetry <- load_layers("BO_bathymean")


my.sites <- data.frame(species = df_species$species, Lon=df_species$lon, Lat= df_species$lat)
my.sites



library(raster)
my.sites.environment <- data.frame(species = my.sites$species, Lat = my.sites$Lat, Lon = my.sites$Lon, extract(environment.bottom,my.sites[,2:3]) )

my.sites.environment_marine <- my.sites.environment %>%  filter(!is.na(BO2_tempmean_bdmean) & !is.na(BO2_salinitymean_bdmean))

names(my.sites.environment_marine) <- c("species", "Lat", "Lon", "Temp", "Sal")

write.csv(my.sites.environment_marine, "Data/plancton_environment_marine_2.csv")


my.sites.environment_not_marine <- my.sites.environment %>%  filter(is.na(BO2_tempmean_bdmean) & is.na(BO2_salinitymean_bdmean))

my.sites.environment_not_marine <- my.sites.environment_not_marine[,1:3] 

write.csv(my.sites.environment_not_marine, "Data/plancton_occurence_not_marine_2.csv")




# Оценка температуры для тех точек, которые предположитеьно не являются морскими


my.sites.environment_not_marine <- my.sites.environment %>%  filter(is.na(BO2_tempmean_bdmean) & is.na(BO2_salinitymean_bdmean))

my.sites.environment_not_marine <- my.sites.environment_not_marine[, 1:3]


dir <- "D:/Data_LMBE/Obskaya Bay additional data/freshwater_variables/"
dir.create(dir)
setwd(dir)

library(raster); library(ncdf4); library(maps); library(foreach); library(doParallel)

temp_avg <- brick("monthly_tmin_average.nc")

### Check the number of layers
nlayers(temp_avg)


### Check the metadata for units, scale factors etc.
nc_open("monthly_tmin_average.nc")

### Add layer names. See Table S1 or the ReadMe for the sequence of the single layers 
names(temp_avg) <- paste(c("temp_avg"), sprintf("%02d", seq(1:12)), sep="_")


library(raster)
my.sites.stream <- data.frame(species = my.sites.environment_not_marine$species, extract(temp_avg,my.sites.environment_not_marine[,2:3]))


#######









library(ggplot2)

ggplot(my.sites.environment_marine, aes(x = Temp, y = Sal)) + geom_point() + geom_density_2d()


PNIS_selected <- my.sites.environment %>% filter(BO2_tempmean_bdmean<8 & BO2_salinitymean_bdmean < 10) %>% group_by(species) %>% summarise(N = n())


unique(PNIS_selected$species)








