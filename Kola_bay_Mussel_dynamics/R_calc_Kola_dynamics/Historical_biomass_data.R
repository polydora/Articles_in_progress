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

gg_murm <- 
  gg_murm 



Murm_map <- 
  ggplot(gg_murm, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "gray20") +
  # coord_map(xlim = c(150., 151.52), ylim = c(59.45, 59.8) )+
  coord_map(xlim = Murm_x, ylim = Murm_y) +
  theme_bw()



# Разделяем датсеты по сборщикам ##############

myt_Roman <- myt %>% filter(source == "Romanova")
unique(myt_Roman$Year)

myt_Mil <- myt %>% filter(source %in% c("Milyutin and Sokolov") )
unique(myt_Mil$Year)

myt_Ant <- myt %>% filter(source %in% c("Antipova et al") )
unique(myt_Ant$Year)



myt_other <-  myt %>% filter(!source %in% c("Antipova et al", "Milyutin and Sokolov", "Romanova")) 



###### Данные с метеостанций Мурмана 1950-2020 гг. ################

Tw <- read_excel("Data/данные 1958-2020 со всех метеостанций.xlsx", na = "NA", sheet = "вода 58-20")
Ta <- read_excel("Data/данные 1958-2020 со всех метеостанций.xlsx", na = "NA", sheet = "воздух 58-20")

meteo <- merge(Tw, Ta)




meteo %>% 
  group_by(Platform) %>% 
  summarise_all(.funs = function(x) mean(x, na.rm = T)) ->
  meteo_means


## Долготная зависимость температур в разные периоды ######

Murm_map + geom_point(data = myt_Roman, aes(x = Longitude, y = Latitude, group = 1), color = "yellow", size = 2)  

Murm_map + geom_point(data = myt_Mil_Ant, aes(x = Longitude, y = Latitude, group = 1), color = "yellow", size = 2) 


meteo %>% 
  filter(Year %in% unique(myt_other$Year)) %>% 
  ggplot(aes(x = Longitude, y = Tw)) +
  geom_point() + 
  geom_smooth()

Murm_map + geom_point(data = myt_other, aes(x = Longitude, y = Latitude, group = 1), color = "yellow", size = 2) 







# Строим модель пространственной изменчивости температуры воды по данным метеостанций 

library(mgcv)
library(gratia)

meteo_reduced <-
  meteo %>% 
  filter(!Platform %in% c("Port_Vladinir", "Drozdovka"))

Mod_Tw <- gam(Tw ~ s(Year, bs = "cr", k = 4) + ti(Longitude, Latitude, bs = "tp"), data = meteo_reduced)

summary(Mod_Tw)

draw(Mod_Tw)

# Строим модель пространственной изменчивости температуры воздуха по данным метеостанций 

Mod_Ta <- gam(Ta ~ s(Year, bs = "cr", k = 4) + ti(Longitude, Latitude, bs = "tp", k = 5), data = meteo_reduced)

summary(Mod_Ta)

draw(Mod_Ta)


meteo_means <-
  meteo_reduced %>%
  filter(Year %in% c(1971, 1981)) %>% 
  group_by(Year, Platform) %>% 
  summarise(Longitude = mean(Longitude), Latitude = mean(Latitude), Tw = mean(Tw))
  
  

meteo_means$Tw_predicted <-  predict(Mod_Tw, newdata = meteo_means)

ggplot(meteo_means, aes(x = Tw_predicted, y = Tw)) +
  geom_point() +
  geom_abline() +
  facet_wrap(~Platform)



myt_Roman <- 
  myt_Roman %>% 
  mutate(Tw = predict(Mod_Tw, newdata = myt_Roman),
         Ta = predict(Mod_Ta, newdata = myt_Roman))


myt_Mil <- 
  myt_Mil %>% 
  mutate(Tw = predict(Mod_Tw, newdata = myt_Mil),
         Ta = predict(Mod_Ta, newdata = myt_Mil))

myt_Ant <- 
  myt_Ant %>% 
  mutate(Tw = predict(Mod_Tw, newdata = myt_Ant),
         Ta = predict(Mod_Ta, newdata = myt_Ant))


myt_other <- 
  myt_other %>% 
  mutate(Tw = predict(Mod_Tw, newdata = myt_other),
         Ta = predict(Mod_Ta, newdata = myt_other))


# Долготный градиент температур на разных данных

ggplot(myt_Roman, aes(x = Longitude, y = Tw)) + 
  geom_point() +
  geom_smooth(method = "lm", color = "black") +
  geom_point(data = myt_Mil, color = "red") + 
  geom_smooth(data = myt_Mil, color = "red", method = "lm")  +
  geom_point(data = myt_Ant, color = "blue") + 
  geom_smooth(data = myt_Ant, color = "blue", method = "lm") 


myt_Ant %>%  
  group_by(Year) %>% 
  summarise(Mean_Tw = mean(Tw))





library(ggrepel)

ggplot(data = myt_Ant) +
  geom_polygon(data = gg_murm, aes(x = long, y = lat, group = group), fill = "gray") +
  geom_point(aes(x = Longitude, y = Latitude, group = 1), color = "yellow", size = 2) +
  facet_wrap(~Year) +
  coord_map(xlim = Murm_x, ylim = Murm_y) +
  geom_text_repel(aes(x = Longitude, y = Latitude, label = site))





# Строим SDM для мидий

unique(myt$source)

myt_train <- rbind(myt_Mil, myt_Ant, myt_Roman)


myt_SDM <- gam(log(B_kg) ~ s(Tw, Ta, k = 10), data = myt_train)

summary(myt_SDM)

appraise(myt_SDM)
draw(myt_SDM)



###########################


myt_train$B_predicted <- predict(myt_SDM, newdata = myt_train)

ggplot(myt_train, aes(x = B_predicted, y = log(B_kg))) + geom_point() +
  geom_abline()


myt_other$region2 <- ifelse(myt_other$region == "Dalnye Zelenetsy", "Eastern Murman", myt_other$region)

myt_other$Period <- ifelse(myt_other$Year < 2000, "XX", "XXI")


myt_other$B_predicted <- predict(myt_SDM, newdata = myt_other)

ggplot(myt_other, aes(x = B_predicted, y = log(B_kg))) + 
  geom_point(aes(color = Period)) +
  geom_abline() +
  facet_wrap(~region2)


##### Анализ динамики по выделам ################

myt <- read_excel("data/data_history.xlsx")

myt <- 
  myt %>% 
  rename(Year = year)

hist(myt$B_kg)


as.data.frame(table(myt$Year, myt$site)) %>% 
  group_by(Var2) %>% 
  summarise(N_Years = sum(Freq)) ->
  observations

site_include <- c("Kharlov Island_N", "Kharlov Island_S", "Tyuva", "Ura", "Klimkovka", "Dolgaya", "Zelenetskaya Zapadnaya", "Yarnyshnaya_lit", "Dal'niy Plyazh")

myt %>% 
  filter(site %in% site_include) ->
  monitor



ggplot(monitor, aes(x = Year, y = log(B_kg))) +
  geom_point() +
  facet_wrap(~ region)



temp_KM <- read_excel("Data/Temperature_KM.xlsx")


lag_temp <- function(Y, lag = 0){
  require(dplyr)
  df <- 
    temp_KM %>% 
    filter(Year <= Y & Year >= (Y - lag))
  mean(df$Temp)
}


lag_temp_2 <- function(Y, lag = 0){
  require(dplyr)
  df <- 
    temp_KM %>% 
    filter(Year == (Y - lag))
  df$Temp
}



monitor$Temp_lagged <- NA
for(i in 1:nrow(monitor)){
  monitor$Temp_lagged[i] <- lag_temp(monitor$Year[i], lag = 5)
}


monitor <-
  monitor %>% 
  mutate(Period = ifelse(Year < 2000, "XX", "XXI"))

ggplot(monitor, aes(x = Temp_lagged, y = log(B_kg), color = Period)) +
  geom_point(size = 4) +
  geom_smooth(method = "lm")

monitor$site <- factor(monitor$site)
monitor$Period <- factor(monitor$Period)


Mod_lag_temp <- gam(log(B_kg) ~ s(Temp_lagged, by = Period, bs = "cr", k = 3) + Period + s(site, bs = "re"), method = "REML", data = monitor, family = "gaussian")

summary(Mod_lag_temp)

appraise(Mod_lag_temp)

draw(Mod_lag_temp)


Mydata <- monitor %>% select(Temp_lagged, Period, site, Year) %>% unique() 

predicted <- predict(Mod_lag_temp, newdata = Mydata, se.fit = TRUE, exclude = "s(site)", newdata.guaranteed=TRUE)

Mydata$Log_B_predicted <- predicted$fit
Mydata$SE <- predicted$se.fit

ggplot(Mydata, aes(x = Temp_lagged, y = Log_B_predicted, color = Period )) + 
  geom_line() +
  geom_ribbon(aes(ymin = Log_B_predicted - 1.96*SE, ymax = Log_B_predicted + 1.96*SE), alpha = 0.2) +
  geom_point(data = monitor, aes(y = log(B_kg))) +
  theme_bw() +
  scale_color_manual(values = c("red", "blue"))
