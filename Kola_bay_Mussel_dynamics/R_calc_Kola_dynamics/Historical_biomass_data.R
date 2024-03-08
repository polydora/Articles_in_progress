# Создание карты

library(sp)
library(dplyr)
library(ggplot2)
library(ggmap)
library(mapproj)
library(maps)

library(rgeos) #этот пакет содержит какую-то хрень, которая позволяет обойти проблему пр чтении фалов средсвами maptools. 
#Att! Этот пакет должен быть загружен до maptools

library(mapdata)
library(maptools) # Rgshhs
library(PBSmapping)
library(gridExtra)
library(grid)

library(akima)
library(car)
library(waver)
library(readxl)
library(reshape2)


# Данные по историческим точкам на Мурманском побережье #############
myt <- read_excel("data/data_history.xlsx")

myt <- 
  myt %>% 
  rename(Year = year)



# read shape file into R
Murm_shape <- readShapeSpatial("Maps/Murmanskaya_obl/boundary-polygon-land-lvl4.shp", proj4string = CRS("+proj=longlat +datum=WGS84"))

# plot(Murm_shape)



Murm_x <- c(min(myt$Longitude)-0.15, max(myt$Longitude)+0.15)
Murm_y <- c(min(myt$Latitude)-0.1, max(myt$Latitude)+0.1)


gg_murm <- fortify(Murm_shape)


Murm_map <- 
  ggplot(gg_murm, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "gray20") +
  # coord_map(xlim = c(150., 151.52), ylim = c(59.45, 59.8) )+
  coord_map(xlim = Murm_x, ylim = Murm_y) +
  theme_bw()



# Разделяем датсеты по сборщикам ##############

myt_Roman <- myt %>% filter(source == "Romanova")

myt_Mil_Ant <- myt %>% filter(source %in% c("Antipova et al", "Milyutin and Sokolov") )

myt_other <-  myt %>% filter(source != "Romanova")




###### Данные с метеостанций Мурмана 1950-2020 гг. ################

Tw <- read_excel("Data/данные 1958-2020 со всех метеостанций.xlsx", na = "NA", sheet = "вода 58-20")
Ta <- read_excel("Data/данные 1958-2020 со всех метеостанций.xlsx", na = "NA", sheet = "воздух 58-20")

meteo <- merge(Tw, Ta)




meteo %>% 
  group_by(Platform) %>% 
  summarise_all(.funs = function(x) mean(x, na.rm = T)) ->
  meteo_means


## Долготная зависимость температур в разные периоды ######

meteo %>% 
  filter(Year %in% unique(myt_Roman$Year)) %>% 
  ggplot(aes(x = Longitude, y = Tw)) +
  geom_point() + 
  geom_smooth()
  
  

meteo %>% 
  filter(Year %in% unique(myt_Mil_Ant$Year)) %>% 
  ggplot(aes(x = Longitude, y = Tw)) +
  geom_point() + 
  geom_smooth()






# Строим модель пространственной изменчивости температуры воды по данным метеостанций 

library(mgcv)
library(gratia)

Mod_Tw <- gam(Tw ~ s(Year, bs = "cr", k = 10) + s(Longitude, Latitude, bs = "tp", k = 12), data = meteo)

summary(Mod_Tw)

draw(Mod_Tw)

# Строим модель пространственной изменчивости температуры воздуха по данным метеостанций 

Mod_Ta <- gam(Ta ~ s(Year, bs = "cr", k = 10) + s(Longitude, Latitude, bs = "tp", k = 12), data = meteo)

summary(Mod_Ta)

draw(Mod_Ta)




myt_Roman <- 
myt_Roman %>% 
  mutate(Year = year)

myt_Roman <- 
  myt_Roman %>% 
  mutate(Tw = predict(Mod_Tw, newdata = myt_Roman),
         Ta = predict(Mod_Ta, newdata = myt_Roman))





# Строим SDM для мидий

unique(myt$source)

myt_other2 <- 
  myt_other %>% 
  filter(source %in% c("Antipova et al", "Milyutin and Sokolov"))


ggplot(myt_other2, aes(Ta)) + 
  geom_density() +
  geom_density(data = myt_Roman, color = "blue")
  


myt_SDM <- gam(log(B_kg) ~ s(Tw, Ta, bs = "tp"), data = myt_Roman)

myt_SDM <- gam(log(B_kg) ~ s(Tw, Ta, bs = "tp", k = 20), data = myt_other)


summary(myt_SDM)

appraise(myt_SDM)
draw(myt_SDM)



###########################

myt_other <- 
  myt_other %>% 
  mutate(Year = year)


myt_other <- 
  myt_other %>%   
  mutate(Tw = predict(Mod_Tw, newdata = myt_other),
         Ta = predict(Mod_Ta, newdata = myt_other))



myt_other$B_predicted <- predict(myt_SDM, newdata = myt_other)

myt_other$region2 <- ifelse(myt_other$region == "Dalnye Zelenetsy", "Eastern Murman", myt_other$region)

ggplot(myt_other, aes(x = log(B_kg), y = B_predicted)) +
  geom_point() +
  facet_wrap(~region2)


ggplot(myt_other, aes(x = log(B_kg), y = B_predicted)) +
  geom_point() 



myt_other %>% 
  group_by(region2) %>% 
  summarise(R_spearman = cor(log(B_kg), B_predicted, method = "spearman" ))


ggplot(myt_other, aes(x = Year, y = log(B_kg) )) +
  geom_point(color = "blue") +
  facet_wrap(~region2, scales = "free_y") +
  geom_line(aes(y = B_predicted)) +
  geom_point(aes(y = B_predicted), fill = "yellow", shape = 21)


ggplot(myt_other, aes(x = Year, y = log(B_kg) )) +
  geom_point(color = "blue") +
  geom_smooth() +
  geom_point(aes(y = B_predicted), fill = "yellow", shape = 21) +
  geom_smooth(aes(y = B_predicted), color = "red")


ggplot(myt_other %>% filter(source == "Antipova et al"), aes(x = log(B_kg), y = B_predicted)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm")

unique(myt_other$source)


ggplot(myt_other %>% filter(source == "Milyutin and Sokolov"), aes(x = log(B_kg), y = B_predicted)) + geom_point(color = "blue") +
  geom_smooth(method = "lm")




### Строим предсказания на основе SDM для Ярнышной

###### Данные по климатическим параметрам
clim <- read_excel("Data/climate_Murman.xlsx", sheet = "data_transposed", na = "NA")

# переводим в "длинный! формат 
clim_long <- 
  melt(clim, id.vars = c("Param",	"Param_Type",	"Month"), variable.name = "Year") %>% 
  filter(complete.cases(.)) %>% 
  mutate(Year = as.numeric(as.character(Year)))

clim_long %>% 
  filter(Param_Type %in% c("Ta_mean", "Tw_mean")) %>% 
  filter(Month != 12) %>% 
  group_by(Year, Param_Type) %>% 
  summarise(value = mean(value)) %>% 
  dcast(Year ~ Param_Type) %>% 
  rename(Ta = Ta_mean, Tw = Tw_mean) -> 
  temp_Teriberka 


##### Данные по биомассе в Ярнышной

YaB <- read_excel("Data/Ya_lit_initial_data_B.xlsx")

YaB %>% 
  select(Year, Biomass) %>% 
  group_by(Year) %>% 
  summarise(Biomass=mean(Biomass))->
  Biomass

temp_Teriberka <-
  temp_Teriberka %>% 
  filter(Year %in% YaB$Year)

Biomass$Log_Biomass <- log(Biomass$Biomass/1000)  

Biomass$Biomass_predicted <- predict(myt_SDM, newdata = temp_Teriberka)

qplot(Biomass$Log_Biomass, Biomass$Biomass_predicted)


ggplot(Biomass, aes(x = Year))

Biomass %>% 
  melt(id.vars = "Year") %>% 
  filter(variable != "Biomass") %>% 
  ggplot(aes(x = Year, y = value, color = variable)) +
  geom_line() 



###################





# Частотное Распределение значений биомасс

low_biomass <- quantile(log(myt_Roman$B_kg), probs = 0.10)

Pl_roman <- 
  myt %>%
  filter(source == "Romanova") %>%
  ggplot(aes(log(B_kg) )) +
  geom_histogram() +
  xlim(-11, 6) +
  geom_vline(xintercept = low_biomass, color = "blue")

Pl_other <- 
  myt %>%
  filter(source != "Romanova") %>%
  ggplot(aes(log(B_kg) )) +
  geom_histogram() +
  xlim(-11, 6)+
  geom_vline(xintercept = low_biomass, color = "blue")

library(cowplot)

plot_grid(Pl_roman, Pl_other, ncol = 1)

######################### Бинаризованные данные по биомассе

myt_Roman <- 
  myt_Roman %>% 
  mutate(Abundance_class = ifelse(log(B_kg) <= low_biomass, "Low", "High"))


Murm_map +
  geom_point(data = myt_Roman, aes(x = Longitude, y = Latitude, group = 1, fill = Abundance_class), shape = 21, size = 3) +
  scale_fill_manual(values = c("red", "yellow"))

