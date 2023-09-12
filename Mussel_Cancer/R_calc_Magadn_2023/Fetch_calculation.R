library(lattice)
library(sp)
library(dplyr)
library(mgcv)
library(reshape2)
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
library(gamm4)

library(akima)
library(car)
library(waver)
library(MuMIn)
library(readxl)



Path <- "D:/Data_LMBE/Maps/"
gshhs.l.b <- paste(Path, "/GSHHS/gshhs_l.b", sep = "")

gshhs.f.b <- paste(Path, "/GSHHS/gshhs_f.b", sep = "")
wdb_rivers.f.b <- paste(Path, "/GSHHS/wdb_rivers_f.b", sep = "")
wdb_borders.f.b <- paste(Path, "/GSHHS/wdb_borders_f.b", sep = "")


# Задаем пределы координат для карт

Magadan_x <- c(149.8, 151.6)
Magadan_y <- c(59.3, 59.8)



# Читаем файлы с контурными картами

Magadan_map <- getRgshhsMap(fn = gshhs.f.b, xlim = Magadan_x, ylim = Magadan_y)

plot(Magadan_map)


gg_Magadan_large <- fortify(Magadan_map)

# gg_Magadan <- read.csv("Data/gg_Magadan_map.csv")





## Данные по поселениям мидий

cancer <- read_excel("Data/Magadan_2023.xlsx", sheet = "Точки взятия проб")
cancer$long <- as.numeric(cancer$E)
cancer$lat <- as.numeric(cancer$N)

cancer$long_corrected <- as.numeric(cancer$Lon_corrected)
cancer$lat_corrected <- as.numeric(cancer$Lat_corrected)

# Создем датафрейм с координатами точек 
fetch.df = data.frame(
  lon = cancer$long_corrected, 
  lat = cancer$lat_corrected,
  name = cancer$ID_eng)


fetch_locs = SpatialPoints(fetch.df[, 1:2], 
                           CRS(proj4string(Magadan_map)))

fetch_len_multi(pts = fetch_locs, bearings = c(0, 45, 90, 135, 180, 225, 270 ), shoreline = Magadan_map,  dmax = 100000, spread = 0,  method = "btree", projected = FALSE)







############################














fetch.df <- fetch.df[complete.cases(fetch.df),]
nrow(fetch.df)

#########################
# Проверка на принадлежность полигонам
# Точки не должны лежать в пределах полигона суши


ggplot(gg_Magadan, aes(x = long, y = lat, group = group)) + geom_polygon() + 
  coord_map(xlim = c(150.88, 150.950), ylim = c(59.55, 59.59))+
  geom_point(data = fetch.df[3,], aes(x = lon, y = lat,group = 1), color = "yellow", size =2)
  

#######################



# Переводим в GIS овскую систему
fetch_locs = SpatialPoints(fetch.df[, 1:2], 
                           CRS(proj4string(Magadan_map)))


is.projected(fetch_locs)

# Приводим к какой-то общей проекции

# p <- "+proj=tmerc +lat_0=0 +lon_0=27 +k=1 +x_0=3500000 +y_0=0 +ellps=intl +units=m +no_defs"
# 
# p <- "+proj=gn_sinu +n=6 +m=3"

p <- "+proj=cc"

Magadan_proj = spTransform(Magadan_map, p)

is.projected(Magadan_proj)

plot(Magadan_proj)



fetch_locs_proj = spTransform(fetch_locs, p)


is.projected(fetch_locs_proj)






# # Применяем функцию fetch ко всем данным разом 
fetch_len_multi(pts = fetch_locs, bearings = c(0, 45, 90, 135, 180, 225, 270 ), shoreline = Magadan_map,  dmax = 100000, spread = 0,  method = "btree", projected = FALSE)



sites_fetch_proj = fetch(polygon_layer = Magadan_proj,
                         site_layer = fetch_locs_proj,
                         max_dist = 300,
                         n_directions = 9,
                         site_names = fetch.df$name)


over(x = fetch_locs_proj, y = Magadan_proj)


plot(sites_fetch_proj)



# Переводим данные в датафрейм
sites_fetch_df  <- as.data.frame(summary(sites_fetch_proj))

sites_fetch_df$Site_ID <- row.names(sites_fetch_df)

write.table(sites_fetch_df, "clipboard", sep = "\t")






cancer_fetch <- merge(sites_fetch_df, cancer, by = "Site_ID")

cancer_fetch$Aver_W_E <- (cancer_fetch$West + cancer_fetch$East)/2


ggplot(gg_Magadan, aes(x = long, y = lat, group = group)) + geom_polygon() + 
  coord_map(xlim = c(150.4, 151.5), ylim = Magadan_y)+
  geom_point(data = cancer_fetch, aes(y = lat, x =  long, group = 1, size = Average), fill = "yellow", shape = 21)



ggplot(gg_Magadan, aes(x = long, y = lat, group = group)) + geom_polygon() + 
  coord_map(xlim = c(150.4, 151.5), ylim = Magadan_y)+
  geom_point(data = cancer_fetch, aes(y = lat, x =  long, group = 1, size = BTN), fill = "yellow", shape = 21)


qplot(cancer_fetch$Prop_cancer, cancer_fetch$BTN)


qplot(cancer_fetch$Abundance, cancer_fetch$BTN)

qplot(cancer_fetch$Biomass, cancer_fetch$BTN)


str(cancer_fetch)

library(betareg)

cancer_fetch$BTN[cancer_fetch$BTN == 0] <- 0.000001

cancer_fetch$B_N <- cancer_fetch$Biomass/cancer_fetch$Abundance 

mod <- betareg(Prop_cancer ~  Average +  Distance_to_town + B_N ,  data = cancer_fetch)



qplot(cancer_fetch$Average, cancer_fetch$Biomass/cancer_fetch$Abundance)

vif(mod)

plot(mod, which = 1)

summary(mod)




ggplot(cancer_fetch, aes(x = Average, y = BTN)) + geom_point()

cor.test(cancer_fetch$Average, cancer_fetch$Prop_cancer)

cor.test(cancer_fetch$Distance_to_town, cancer_fetch$Prop_cancer)
