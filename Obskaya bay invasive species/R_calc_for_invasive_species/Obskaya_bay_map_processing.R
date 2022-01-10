# Hot spots of potential biotic invasion in the Ob bay

library(ggplot2)
library(readxl)



Ob_df <- read.csv("Data/Obskaya_bay_map.csv") # Map poligones


Ob_x <- c(71, 75)
Ob_y <- c(70, 73.2)


hotspot <- read.table("Data/Obskaya_bay_ports.csv", sep = ",", header = T) 

grid_data <- expand.grid(Lat = seq(from = 70.01, to =72.89, length.out = 100), Long =  seq(from = 71, to =75.2 , length.out = 100))





Pl_Obskaya_bay <- 
  ggplot(Ob_df, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill = "gray90", colour = "gray20") + 
  coord_map(xlim = Ob_x, ylim = Ob_y) + 
  theme_bw() +  
  theme(axis.title.x = element_blank(),  axis.title.y = element_blank(), plot.background = element_blank(), panel.border = element_blank(), panel.grid = element_blank()) + 
  theme(axis.text.x =element_blank(), axis.text.y= element_blank()) + 
  theme(axis.ticks = element_blank())


Pl_Obskaya_bay + geom_point(data = hotspot,  aes(x = Lon, y = Lat, group =1), size = 4, shape = 22, fill = "yellow")





