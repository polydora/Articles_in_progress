library(dplyr)
library(broom)
library(readxl)
library(ggmap)
library(maps)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(reshape2)
library(scatterpie)

library(sp)
library(mapproj)



Murmansk_region <- st_read("Data/Maps/Murmanskaya_obl/boundary-polygon-land-lvl4.shp", quiet =  TRUE)

Pl_kola <- 
ggplot() +
  geom_sf(data = Murmansk_region, fill = "gray50") +
  coord_sf(
    xlim = c(32.96, 33.62),  
    ylim = c(68.9, 69.3)) +
  theme_bw()+
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())
  

kola_cancer <- read_excel("Data/Кольский рак было-стало.xlsx")

kola_cancer <-
  kola_cancer %>% 
  arrange((BTN))

Pl_kola + 
  geom_point(data = kola_cancer, aes( x = Longitude, y = Latitude, fill = BTN), shape = 21, size = 4, position = position_jitter(width = 0.02, height = 0.001)) +
  facet_wrap(~Period, ncol = 2) +
  scale_fill_manual(values = c("yellow", "red")) +
  labs(x = "", y = "")
