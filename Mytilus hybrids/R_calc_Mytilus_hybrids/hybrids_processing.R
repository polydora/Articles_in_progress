library(readxl)
library(ggplot2)
library(dplyr)

hybr <- read_excel("Data/hybrids_BS.xlsx")
murm_shape <- read.csv("Data/Murman.csv")


# Доля гибридов в сборах

hybr_pop <- hybr %>% group_by(pop) %>% summarise(lat = mean(N), lon = mean(E), Year = mean(year), Ptros = mean(str), P_hybr = mean(str > 0.1 & str < 0.9), md = 1 - (sd(str))^2/(Ptros*(1-Ptros)) )


ggplot(hybr_pop, aes(x = P_hybr, y = md)) + geom_point()



# Рисуем карту

Murm_x <-  c(30, 40)
Murm_y <- c(67.8, 69.8)





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
  geom_point(data = hybr_pop, aes(x = lon, y = lat, group = 1, fill = P_hybr, size = P_hybr), shape = 21) +
  scale_fill_gradient(low = "yellow", high = "red")


Kola_bay_map + 
  geom_point(data = hybr_pop, aes(x = lon, y = lat, group = 1, fill = md, size = md), shape = 21) +
  scale_fill_gradient(low = "yellow", high = "red")

Tuva_map +
  geom_point(data = hybr_pop, aes(x = lon, y = lat, group = 1, fill = P_hybr, size = P_hybr), shape = 21) +
  scale_fill_gradient(low = "yellow", high = "red")





hybr %>% filter(pop %in% hybr_pop$pop[hybr_pop$md > 0.7] ) %>% 
  ggplot(., aes(x = str)) + geom_histogram() + facet_wrap(~pop)
