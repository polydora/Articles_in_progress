
library(dplyr)


# Загрузка данных с координатами встреч видов

path = "D:/Data_LMBE/Obskaya Bay additional data/Species characteristics/"


files <- list.files(path)

df_species <- NULL

for(name in files){
  df <- read.table(paste(path, "/",name, sep = ""), sep = "\t", header = T)
  df <- df %>% select(-gbifID)
  df <- unique(df)
  if(sum (is.na(df$species)) > 0) df$species <- df$genus
  df_species <- rbind(df_species, df)
}


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
)

# exclude <- c("Paralithodes camtschaticus")

# df_species <- df_species %>% filter(!(species %in% exclude))


df_species$Status <- ifelse(df_species$species %in% native_species, "Native", "PNIS") 


df_examp <- df_species %>% filter(Status == "PNIS")  



  
  
  
  
library(sdmpredictors)
library(leaflet)

options(sdmpredictors_datadir="C:\\Users\\polyd\\AppData\\Local\\Temp\\RtmpuyXZRU/sdmpredictors")


# Скачиваем данные по средней температуре воды на срденей глубине и срденей солености на срденей глубине. 
environment.bottom <- load_layers( layercodes = c("BO2_tempmean_bdmean" ,  "BO2_salinitymean_bdmean",) , equalarea=FALSE, rasterstack=TRUE)

bathymetry <- load_layers("BO_bathymean")


my.sites <- data.frame(species = df_examp$species, Lon=df_examp$decimalLongitude, Lat= df_examp$decimalLatitude)
my.sites

# m <- leaflet()
# m <- addTiles(m)
# m <- addMarkers(m, lng=my.sites$Lon, lat=my.sites$Lat, popup=NULL)
# m

library(raster)
my.sites.environment <- data.frame(species = my.sites$species, Depth=extract(bathymetry,my.sites[,2:3]) , extract(environment.bottom,my.sites[,2:3]) )
my.sites.environment

my.sites.environment <- my.sites.environment %>%  filter(!is.na(BO2_tempmean_bdmean) | !is.na(BO2_salinitymean_bdmean))



library(ggplot2)

ggplot(my.sites.environment, aes(x = BO2_tempmean_bdmean, y = BO2_salinitymean_bdmean)) + geom_point() + geom_density_2d() + facet_wrap(~species)


PNIS_selected <- my.sites.environment %>% filter(BO2_tempmean_bdmean<8 & BO2_salinitymean_bdmean < 10) %>% group_by(species) %>% summarise(N = n())


unique(PNIS_selected$species)








