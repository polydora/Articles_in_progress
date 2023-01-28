library(ggmap)
library(readxl)
library(ggrepel)
library(dplyr)



dn <- read_excel("Data/Таблица всех ДН.xlsx", sheet = "Лист2")
sites <- dn %>% group_by(Site, Species) %>% summarise(Lon = mean(Lon), Lat = mean(Lat))

sites$Number <- 1:nrow(sites)

world <- map_data("world")

Pl_map <- 
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    fill = "gray"
  ) 


Pl_map + 
  geom_point(data = dn, aes(x = Lon, y = Lat, fill = Species, size = Proportion_ill), shape = 21) 

ggplot(dn, aes(x = Proportion_ill)) + geom_histogram()         

  
dn2 <- dn %>% filter(Proportion_ill > 0.05 & Proportion_ill < 0.5 )  

Pl_map + 
  geom_point(data = dn2, aes(x = Lon, y = Lat, fill = Species, size = Proportion_ill), shape = 21) 
