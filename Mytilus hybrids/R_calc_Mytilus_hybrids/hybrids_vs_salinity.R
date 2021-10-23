library(readxl)
library(ggplot2)
library(dplyr)
library(mgcv)



hybr <- read_excel("Data/hybrids_BS.xlsx")
hybr$region <- factor(hybr$region)


rivers <- read_excel("Data/rivers_BS.xlsx")
rivers$Drainage_Area <- as.numeric(rivers$Drainage_Area)
rivers$Lat <- as.numeric(rivers$Lat)
rivers$Lon <- as.numeric(rivers$Lon)



# Доля гибридов в сборах

hybr_pop <- hybr %>% group_by(ID) %>% summarise(lat = mean(N), lon = mean(E), Year = mean(year), Region = unique(region), Ptros = mean(str), P_hybr = mean(str > 0.1 & str < 0.9), md = 1 - (sd(str))^2/(Ptros*(1-Ptros)), md_1 = 1-md, Salinity = unique(salinity) ) 

# %>% filter(Ptros<0.9 & Ptros>0.1)



hybr_pop$Region <- factor(hybr_pop$Region, levels = c("West_coast",  "Kola",  "Tyuva", "East_coast" ))










nearest_dist <- function(XY, objects = river, x.name = "Lon", y.name = "Lat"){
  
  XY1 <-as.numeric(XY[,1])
  XY2 <- as.numeric(XY[,2])
  dist <- (acos(sin(XY1*pi/180)*sin(objects[ ,y.name]*pi/180) + cos(XY1*pi/180)*cos(objects[ ,y.name]*pi/180)*cos(XY2*pi/180 - objects[ ,x.name]*pi/180)) * 6371)
  
  MD <- data.frame(Min_dist = min(dist))
  cbind(objects[which(dist == min(dist)), ], MD)
  
}


# The distance to mouth of the neares river 

df_river <- nearest_dist(XY = hybr_pop[1, c("lat", "lon")], objects = rivers)

df_river[1,] <- NA

for(i in 1:nrow(hybr_pop)) {
  df_river[i,] <- nearest_dist(XY = hybr_pop[i, c("lat", "lon")], objects = rivers)
  df_river$ID[i] <- as.character(hybr_pop$ID)[i]
}




names(df_river) <- c( "River",  "Drainage_Area",  "Lat_river", "Lon_river", "Min_dist_river", "ID" )

hist(df_river$Min_dist_river)



hybr_pop_riv <- merge(hybr_pop, df_river, by = "ID")


hybr_pop_riv$Drainage_Area_1 <- 1/hybr_pop_riv$Drainage_Area



hybr_pop_riv$Sal_index <- with(hybr_pop_riv, sqrt(rank(Min_dist_river) * rank(Drainage_Area_1)) )
  
  
  
ggplot(hybr_pop_riv, aes(x = Salinity, y = Sal_index )) + geom_boxplot()


Mod <-  gam(md_1 ~ s(Ptros) + Sal_index, data = hybr_pop_riv %>% filter(Region !="Tyuva"), family = "betar" ) 
summary(Mod)

plot(Mod)
gam.check(Mod)



Tuv_Mod_predicted <- predict(Mod, newdata = hybr_pop_riv %>% filter(Region =="Tyuva"), type = "response")

qplot(x = Tuv_Mod_predicted, y = hybr_pop_riv %>% filter(Region =="Tyuva") %>% pull(md_1) )

cor.test(x = Tuv_Mod_predicted, y = hybr_pop_riv %>% filter(Region =="Tyuva") %>% pull(md_1) )





Mod2 <-  gam(P_hybr ~ s(Ptros)+ Sal_index, data = hybr_pop_riv %>% filter(Region !="Tyuva"), family = "betar" ) 
plot(Mod2)
summary(Mod2)
gam.check(Mod2)





# Рисуем карту

murm_shape <- read.csv("Data/Murman.csv")




Murm_x <-  c(30, 40)
Murm_y <- c(67.8, 69.8)



hybr2 <- merge(hybr, hybr_pop)



Murm_region_map <- 
  ggplot(murm_shape, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill = "gray90", colour = "gray20") + 
  coord_map(xlim = Murm_x, ylim = Murm_y) + 
  theme_bw() +  
  theme(axis.title.x = element_blank(),  axis.title.y = element_blank(), plot.background = element_blank(), panel.border = element_blank()) 



Murm_x_small <-  c(32.5, 34)
Murm_y_small <- c(68.85, 69.5)

Kola_bay_map <- 
  ggplot(murm_shape, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill = "gray90", colour = "gray20") + 
  coord_map(xlim = Murm_x_small, ylim = Murm_y_small) + 
  theme_bw() +  
  theme(axis.title.x = element_blank(),  axis.title.y = element_blank(), plot.background = element_blank(), panel.border = element_blank()) 



Tuva_x <-  c(33.55, 33.65)
Tuva_y <- c(69.17, 69.2)


Tuva_map <-
  ggplot(murm_shape, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill = "gray90", colour = "gray20") + 
  coord_map(xlim = Tuva_x, ylim = Tuva_y) + 
  theme_bw() +  
  theme(axis.title.x = element_blank(),  axis.title.y = element_blank(), plot.background = element_blank(), panel.border = element_blank()) 




# + theme(panel.grid = element_blank(), axis.text.x =element_blank(), axis.text.y= element_blank()) + theme(axis.ticks = element_blank()) 


Murm_region_map + 
  geom_point(data = hybr_pop_riv, aes(x = lon, y = lat, group = 1, fill = Sal_index), shape = 21, size = 3) +
  scale_fill_gradient(low = "yellow", high = "red")


Kola_bay_map + 
  geom_point(data = hybr_pop_riv, aes(x = lon, y = lat, group = 1, fill = Sal_index), shape = 21, size = 3) +
  scale_fill_gradient(low = "yellow", high = "red")

Tuva_map +
  geom_point(data = hybr_pop_riv, aes(x = lon, y = lat, group = 1, fill = Sal_index), shape = 21, size = 3) +
  scale_fill_gradient(low = "yellow", high = "red")


