# Packages ######################
library(ggmap)
library(mapproj)
library(maps)

library(rgeos) 

library(mapdata)
library(maptools) # Rgshhs
library(PBSmapping)

library(readxl)
library(dplyr)
library(cowplot)
library(mgcv)
library(reshape2)
library(ggplot2)
library(ggrepel)

library(png)

library(vegan)

library(broom)
library( broom.mixed)


library(ggvegan)
library(car)

# Data reading ####################

Ob_df <- read.csv("Data/Obskaya_bay_map.csv") # Map poligones

# All Station position
stations <- read_excel("Data/Obskaya_bay_2020.xlsx", sheet = "Station parameters", na = "NA")

# Stations with biological samples
stat_full <- stations %>% filter(Exclude == 0)

stat_full <- as.data.frame(stat_full)


# Данные для карт ###############################################

# Coordinate limites 
# Ob_x <- c(71, 79)

Ob_x <- c(71, 75)
Ob_y <- c(70, 73.2)

# Sabetta
Sabetta_y <- 71.235220
Sabetta_x <- 72.126754

# Terminal

Terminal_y <- 71.010886
Terminal_x <- 73.793525


grid_data <- expand.grid(Lat = seq(from = 70.01, to =72.89, length.out = 100), Long =  seq(from = 71, to =75.2 , length.out = 100))





Pl_Obskaya_bay <- 
  ggplot(Ob_df, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill = "gray90", colour = "gray20") + 
  coord_map(xlim = Ob_x, ylim = Ob_y) + 
  theme_bw() +  
  theme(axis.title.x = element_blank(),  axis.title.y = element_blank(), plot.background = element_blank(), panel.border = element_blank(), panel.grid = ) + 
  theme(axis.ticks = element_blank())  + 
  geom_point(aes(x = Terminal_x, y = Terminal_y), size = 3, shape = 22, fill = "yellow") + 
  geom_point(aes(x = Sabetta_x, y = Sabetta_y), size = 3, shape = 22, fill = "yellow") 


Pl_Obskaya_bay + geom_point(data = stat_full, aes(x = Long, y = Lat, group =1))



# извлекаем данные из nc-файла модели 










