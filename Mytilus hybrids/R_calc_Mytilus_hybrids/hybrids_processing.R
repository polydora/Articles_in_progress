library(readxl)
library(ggplot2)
library(dplyr)
library(mgcv)



hybr <- read_excel("Data/hybrids_BS.xlsx")
murm_shape <- read.csv("Data/Murman.csv")

str(hybr)


hybr$region <- factor(hybr$region)

hybr$Sea <- with(hybr, ifelse(region == "WS", "WS", "BS"))


# Доля гибридов в сборах

hybr_pop <- hybr %>% group_by(ID) %>% summarise(lat = mean(N), lon = mean(E), Year = mean(year), Region = unique(region), Ptros = mean(str), P_hybr = mean(str > 0.05 & str < 0.95), md = 1 - (sd(str))^2/(Ptros*(1-Ptros)), md_1 = 1-md, Salinity = unique(salinity) ) 
# %>% filter(Ptros<0.9 & Ptros>0.1)


hybr_pop$Region <- factor(hybr_pop$Region, levels = c("West_coast",  "Kola",  "Tyuva", "East_coast" ))

ggplot(hybr_pop, aes(x = Ptros, y = md_1)) +  geom_point() + geom_smooth()

ggplot(hybr_pop, aes(x = Region, y = md_1))  + geom_hline(yintercept = median(hybr_pop$md)) + geom_boxplot()



ggplot(hybr_pop, aes(x = P_hybr, y = md_1)) +  geom_point()



Mod <-  gam(md_1 ~ s(Ptros, k = 5)  + Salinity , data = hybr_pop, family = "betar" ) 
summary(Mod)



My_data <- expand.grid(Ptros = seq(0, 1, 0.01),  Salinity = c("normal", "brackish"))


Predicted <- predict(Mod, newdata = My_data, type = "response", se.fit = T) 

My_data$fit <- Predicted$fit
My_data$se <- Predicted$se.fit

ggplot(My_data, aes(x = Ptros, y = fit, color = Salinity)) + geom_line() + geom_point(data = hybr_pop %>%  filter(Region == "East_coast"), aes(y = md_1), size = 3)  

ggplot(My_data, aes(x = Ptros, y = fit, color = Salinity)) + geom_line() + geom_point(data = hybr_pop %>%  filter(Region == "Kola"), aes(y = md_1), size = 3)  

ggplot(My_data, aes(x = Ptros, y = fit, color = Salinity)) + geom_line() + geom_point(data = hybr_pop %>%  filter(Region == "Tyuva"), aes(y = md_1), size = 3)  

ggplot(My_data, aes(x = Ptros, y = fit, color = Salinity)) + geom_line() + geom_point(data = hybr_pop %>%  filter(Region == "West_coast"), aes(y = md_1), size = 3)  



Mod2 <-  gam(P_hybr ~ s(Ptros, k = 5)  + Salinity , data = hybr_pop, family = "betar" ) 
summary(Mod2)


plot(Mod2)


Predicted <- predict(Mod2, newdata = My_data, type = "response", se.fit = T) 

My_data$fit <- Predicted$fit
My_data$se <- Predicted$se.fit

ggplot(My_data, aes(x = Ptros, y = fit, color = Salinity)) + geom_line() + geom_point(data = hybr_pop, aes(y = P_hybr), size = 3)  + geom_point(data = hybr_pop %>% filter(Region %in% c("Tyuva")), aes(y = P_hybr), color = "black")




ggplot(My_data, aes(x = Ptros, y = fit, color = Salinity)) + geom_line() + geom_point(data = hybr_pop %>%  filter(Region == "Kola"), aes(y = md_1), size = 3)  

ggplot(My_data, aes(x = Ptros, y = fit, color = Salinity)) + geom_line() + geom_point(data = hybr_pop %>%  filter(Region == "Tyuva"), aes(y = md_1), size = 3)  

ggplot(My_data, aes(x = Ptros, y = fit, color = Salinity)) + geom_line() + geom_point(data = hybr_pop %>%  filter(Region == "West_coast"), aes(y = md_1), size = 3)  





Mod_resid <- residuals(Mod)

ggplot(hybr_pop, aes(x = factor(Year), group = Year, y = Mod_resid)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90))   


  
qplot(x = hybr_pop$P_hybr, y = Mod_resid)


# + geom_ribbon(aes(ymin = fit - 1.96*se, ymax = fit+1.96*se), alpha = 0.1)  






# Рисуем карту

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
  geom_point(data = hybr_pop, aes(x = lon, y = lat, group = 1, fill = Mod_gam_resid, size = Mod_gam_resid), shape = 21) +
  scale_fill_gradient(low = "yellow", high = "red")


Kola_bay_map + 
  geom_point(data = hybr_pop, aes(x = lon, y = lat, group = 1, fill = md_1, size = md_1), shape = 21) +
  scale_fill_gradient(low = "yellow", high = "red")

Tuva_map +
  geom_point(data = hybr_pop, aes(x = lon, y = lat, group = 1, fill = P_hybr, size = P_hybr), shape = 21) +
  scale_fill_gradient(low = "yellow", high = "red")


ggplot(hybr_pop, aes(x = Ptros, y = 1-md)) + geom_density2d() + geom_point()

ggplot(hybr_pop, aes(x = Ptros)) + geom_density() 


