library(ggmap)
library(mapproj)
library(dplyr)


# Shape file reading
ggWhite_Sea <- read.csv("data/ggWhite_Sea_2021.csv")
ggKand_upper <- read.csv("data/ggKand_upper_2021.csv")



# Данные

samples <- read.csv("data/Координаты проб.csv", header = T)

myt <- read.csv("data/Данные по количеству точек на раковине мидий.csv", header = T)






## Coordinates for maps

Full_x <- c(3, 43)
Full_y <- c(55, 72)


White_sea_x <- c(31, 45) 
White_sea_y <- c(63,68.9)

Kand_upper_x <- c(min(samples$Lon) - 0.1, max(samples$Lon) + 0.1)
Kand_upper_y <- c(min(samples$Lat) - 0.1, max(samples$Lat) + 0.1)



Plot_White_sea <- 
  ggplot(ggWhite_Sea, aes(x=Lon, y=Lat, group=group)) + 
  geom_polygon(fill = "gray90", colour = "gray20") + 
  coord_map(xlim = White_sea_x, ylim = White_sea_y) +  
  theme(axis.text.x =element_blank(), axis.text.y= element_blank(), axis.ticks=element_blank(), axis.title.x =element_blank(),  axis.title.y= element_blank(), plot.background = element_rect(fill = "white"), panel.grid = element_blank(), panel.background = element_rect(fill = "white"))

pol<-data.frame(Lon = Kand_upper_x, Lat = Kand_upper_y, group = 0.1)

Plot_White_sea2 <- 
  Plot_White_sea + 
  geom_rect(data = pol, aes(xmin = Lon[1], xmax = Lat[2], ymin = Lat[1], ymax = Lat[2]), color = "black", fill = NA, size = 1) + 
  ggtitle("White Sea")



Plot_Kand_upper <- 
  ggplot(ggKand_upper, aes(x=Lon, y=Lat, group=group)) + 
  geom_polygon(fill = "gray70", colour = "black") + 
  coord_map(xlim = Kand_upper_x, ylim = Kand_upper_y) + 
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white")) +
  theme(plot.title = element_text(hjust = 0.5))

Plot_Kand_upper + geom_point(data = samples %>% filter(Sample %in% c("Buytred 1", "Buytred 8", "Buytred 10", "Kuz1", "Kuz3")) ,aes(group = 1), color = "blue", size = 3)


Plot_Kand_upper + geom_point(data = samples %>% filter(Sample %in% c( "Buytred 10", "Buytred 9", "Kuz1", "Kuz3")) ,aes(group = 1), color = "blue", size = 3)


ggplot(myt%>% filter(Sample !="Kuz1"), aes(x = Sample , y = N_dots)) + geom_boxplot()



