library(ggmap)
library(mapproj)


# Shape file reading
ggWhite_Sea <- read.csv("data/ggWhite_Sea_2021.csv")
ggKand_upper <- read.csv("data/ggKand_upper_2021.csv")

ggKand_upper2 <- read.csv("data/ggKand_upper.csv")


## Coordinates for maps

Full_x <- c(3, 43)
Full_y <- c(55, 72)


White_sea_x <- c(31, 45) 
White_sea_y <- c(63,68.9)

Kand_upper_x <- c(min(myt_site$Lon) - 0.1, max(myt_site$Lon) + 0.1)
Kand_upper_y <- c(min(myt_site$Lat) - 0.1, max(myt_site$Lat) + 0.1)



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


ggsave(filename = "figures/Plot_White_Sea.eps", plot = Plot_White_sea)  


Plot_Kand_upper <- 
  ggplot(ggKand_upper, aes(x=Lon, y=Lat, group=group)) + 
  geom_polygon(fill = "gray70", colour = "black") + 
  coord_map(xlim = Kand_upper_x, ylim = Kand_upper_y) + 
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white")) +
  theme(plot.title = element_text(hjust = 0.5))

Plot_Kand_upper_2 <- 
  ggplot(ggKand_upper2, aes(x=long, y=lat, group=group)) + 
  geom_polygon(fill = "gray70", colour = "black") + 
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white")) +
  theme(plot.title = element_text(hjust = 0.5))



myt_site2 <- myt_site %>% arrange((Prop_T))

Plot_Kand_upper_PT <-
  Plot_Kand_upper +
  geom_point(data = myt_site2, aes(x = Lon, y = Lat, group = 1, fill = Prop_T), shape = 21, size = 3) +
  scale_fill_gradient(low = "yellow", high = "red") +
  guides(size = "none") +
  theme(legend.direction = "horizontal", legend.position = c(0.8,0.1), legend.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.text.x = element_blank(), axis.ticks = element_blank()) +
  labs(fill = "PT")






Kand_upper_x2 <- c(32.2, 33.06)
Kand_upper_y2 <- c(66.85, 67.16)

Plot_Kand_upper_PT_2 <-
  Plot_Kand_upper +
  geom_point(data = myt_site2, aes(x = Lon, y = Lat, group = 1, fill = Prop_T), shape = 21, size = 5) +
  coord_map(xlim = Kand_upper_x2, ylim = Kand_upper_y2)+
  scale_fill_gradient(low = "yellow", high = "red") +
  guides(size = "none") +
  theme(legend.direction = "horizontal", legend.position = c(0.8,0.1), legend.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.text.x = element_blank(), axis.ticks = element_blank()) +
  labs(fill = "PT") +
  guides(fill = "none")


ggsave(filename = "figures/Plot_PT_no_size_insertion.svg", plot = Plot_Kand_upper_PT_2)  

  
ggsave(filename = "figures/Plot_PT_no_size.svg", plot = Plot_Kand_upper_PT)  






Plot_Kand_upper2 <- 
  Plot_Kand_upper + 
  geom_point(data = ports, aes(x = Lon, y = Lat, group = 1), size = 3)



Plot_Kand_upper2 <- 
  Plot_Kand_upper + 
  geom_point(data = myt_site, aes(x = Lon, y = Lat, group = 1, size = Average_Fetch, fill = Salinity), shape = 21) +
  scale_fill_gradient(low = "yellow", high = "blue") +
  guides(size = "none") +
  theme(legend.direction = "horizontal", legend.position = c(0.8,0.1), legend.background = element_blank())

ggsave(filename = "figures/Plot_Kand_upper.eps", plot = Plot_Kand_upper2)  


library(grid)
grid.newpage()
v1<-viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
v2<-viewport(width = 0.5, height = 0.5, x = 0.78, y = 0.75) #plot area for the inset map
print(Plot_Kand_upper2,vp=v1) 
print(Plot_White_sea2,vp=v2)

