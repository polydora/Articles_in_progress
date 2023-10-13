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

# Вычисляем fetch для точек сбора 2023 2021 #################

Path <- "D:/Data_LMBE/Maps/"
gshhs.l.b <- paste(Path, "/GSHHS/gshhs_l.b", sep = "")

gshhs.f.b <- paste(Path, "/GSHHS/gshhs_f.b", sep = "")
wdb_rivers.f.b <- paste(Path, "/GSHHS/wdb_rivers_f.b", sep = "")
wdb_borders.f.b <- paste(Path, "/GSHHS/wdb_borders_f.b", sep = "")


# Задаем пределы координат для карт

Magadan_x <- c(149.8, 152.4)
Magadan_y <- c(58.8, 59.8)

# Magadan_x <- c(149.8, 151.6)
# Magadan_y <- c(59.3, 59.8)


# Читаем файлы с контурными картами

Magadan_map <- getRgshhsMap(fn = gshhs.f.b, xlim = Magadan_x, ylim = Magadan_y)

plot(Magadan_map)


gg_Magadan_large <- fortify(Magadan_map)

# save(gg_Magadan_large, file =  "Data/gg_Magadan_large.RData")
# gg_Magadan <- read.csv("Data/gg_Magadan_map.csv")





## Данные по поселениям мидий

points <- read_excel("Data/Magadan_2021_2023_ecology.xlsx", sheet = "Точки взятия проб 2023 2021")
points$long <- as.numeric(points$E)
points$lat <- as.numeric(points$N)

points$long_corrected <- as.numeric(points$Lon_corrected)
points$lat_corrected <- as.numeric(points$Lat_corrected)

# Создем датафрейм с координатами точек 
fetch.df = data.frame(
  lon = points$long_corrected, 
  lat = points$lat_corrected,
  Site = points$Site, 
  Year = points$Year,
  Salinity = points$Salinity,
  Dist_Port = points$Distance_to_Nerest_Port)


fetch_locs = SpatialPoints(fetch.df[, 1:2], 
                           CRS(proj4string(Magadan_map)))


      
fetch <- fetch_len_multi(pts = fetch_locs, bearings = seq(0, 315, 45), shoreline = Magadan_map,  dmax = 100000, spread = 0,  method = "btree", projected = FALSE)

# fetch <- fetch_len_multi(pts = fetch_locs, bearings = c(0, 45, 90, 135, 180, 225, 270 ), shoreline = Magadan_map,  dmax = 100000, spread = 0,  method = "btree", projected = FALSE)


# fetch <- fetch_len_multi(pts = fetch_locs, bearings = c(45, 90,  225, 270 ), shoreline = Magadan_map,  dmax = 100000, spread = 0,  method = "btree", projected = FALSE)


fetch.df$fetch <- as.data.frame(fetch) %>% rowMeans()


load(file =  "Data/gg_Magadan_large.RData")

 
ggplot(gg_Magadan_large, aes(x = long, y = lat, group = group)) + 
  geom_polygon() + 
  coord_map(xlim = c(150., 151.52), ylim = c(59.4, 59.8) )+
  geom_point(data = fetch.df, aes(x = lon, y = lat, group = 1, size = (fetch) ), fill = "yellow", shape = 21) 
############################


# fetch.df <- fetch.df[complete.cases(fetch.df),]
# nrow(fetch.df)
# 
# #########################
# # Проверка на принадлежность полигонам
# # Точки не должны лежать в пределах полигона суши
# 
# 
# ggplot(gg_Magadan_large, aes(x = long, y = lat, group = group)) + geom_polygon() + 
#   # coord_map(xlim = c(150., 151.52), ylim = c(59.45, 59.8) )+
#   coord_map(xlim = c(150.05, 150.1), ylim = c(59.67, 59.68) )+
#   geom_point(data = fetch.df[14,], aes(x = lon, y = lat,group = 1), color = "blue", size =2)
  

#######################
# 
# # Вычислем покрытие мидий и переводим в абсолютные обилия ###################3
# 
# cover <- read_excel("Data/Magadan_2023.xlsx", sheet = "Покрытия миидий")
# biomass <- read_excel("Data/Magadan_2023.xlsx", sheet = "Обилие мидий")
# size <- read_excel("Data/Magadan_2023.xlsx", sheet = "Размерная струкутра поселений")
# 
# Area <- 6 #Площадь, которую охватывает фотоснимок (кв. м)
# 
# 
# abund <-
# size %>% 
#   filter(Year == 2023) %>%  
#   group_by(Site, Sample) %>% 
#   summarise(Abund = n())
# 
# NB <- merge(abund, biomass)
# 
# NB <-
# NB %>% 
#   mutate(N = round(Abund/Area *10000, 0), B = round(Biomass/Area*10000, 1))
# 
# mean_NB <- 
#   NB %>% 
#   group_by(Site) %>% 
#   summarise(N = mean(N), B = mean(B))
# 
# 
# mean_cover <- 
# cover %>% 
#   group_by(Site) %>%
#   summarise(Cover = mean(`Number of squares`/30))
#   
# total_NB <-
#   merge(mean_NB, mean_cover) %>% 
#   mutate(N_total = N * Cover/Area, B_total = B * Cover/Area)
#   
# 
# plot(total_NB$N, total_NB$N_total)
# 
# plot(total_NB$B, total_NB$B_total)
# 
# 
# abund_fetch <- 
# merge(total_NB, fetch.df)
# 
# 
#Обилие Раковых мидий #######################3

cancer <- read_excel("Data/Cancer_Magadan_2021-2023.xlsx", na = "NA", sheet = "Magadan_cancer_prevalence")

cancer2 <-
cancer %>%
  group_by(Year, Site) %>%
  summarise(N_processed = sum(N_Processed), N_cancer = sum(Aneuploid))


mean_cancer <-
cancer2 %>%
  group_by(Year, Site) %>%
  summarise(N_processed = sum(N_processed), N_cancer = sum(N_cancer)) %>%
  mutate(Prop_cancer = N_cancer/N_processed)


# Размерная структура #####################

library(vegan)

size <- read_excel("Data/Magadan_2021_2023.xlsx", na = "NA", sheet = "Размерная струкутра 2023 2021")
size <- size[complete.cases(size), ]

unique(size$Site)

scam <- dcast(Year + Site ~ Size_class, data = size)

area <- read_excel("Data/Magadan_2021_2023.xlsx", na = "NA", sheet = "Площадь проб на размер")

sample_area <- 
area %>% group_by(Year, Site) %>% summarise(Total_area = sum(Area))

scam <- 
scam %>% group_by(Year, Site)

scam [ ,3:ncol(scam)] <- 
scam[ ,3:ncol(scam)] / sample_area$Total_area *10000



# scam_23 <- scam %>% filter(Year == 2023) %>% select(-Year)

scam_r <- decostand(scam[,-c(1,2)], method = "hellinger")
scam_log <- decostand(scam[,-c(1,2)], method = "log")


pca_scam <- cca(scam[,-c(1,2)], scale = F) 
summary(pca_scam)

plot(pca_scam, display = "species")
plot(pca_scam, display = "sites")


pca_scores_scam <- data.frame(Year = scam$Year, Site = scam$Site, as.data.frame(scores(pca_scam, choices = 1:2)$sites))



#Соединяем все данные #######################

mean_NB_cancer <- 
  merge(fetch.df, mean_cancer, all.x = T)
  
  # merge(abund_fetch, mean_cancer, all.x = T)

mean_NB_cancer <- merge(mean_NB_cancer, pca_scores_scam)

unique(mean_NB_cancer$Site)

mean_NB_cancer$fetch <- mean_NB_cancer$fetch/1000
# mean_NB_cancer <- mean_NB_cancer %>% mutate(Prop_cancer = Aneuploid/N_Processed)

str(mean_NB_cancer)

mean_NB_cancer$Prop_cancer[mean_NB_cancer$Prop_cancer == 0] <- 0.00000001
mean_NB_cancer$fYear <- factor(mean_NB_cancer$Year)

mean_NB_cancer <- mean_NB_cancer %>% mutate(fPort = factor(ifelse(Site %in% c("KHOL", "MAM", "MAR_II", "MCHK", "PORT"), "Close", "Distant")))

mean_NB_cancer <- mean_NB_cancer %>% mutate(fSal = factor(ifelse(Site %in% c("ARM", "UM", "DUC"), "Close", "Distant")))



# 
# mean_NB_cancer$Fi <- 2*asin(sqrt(mean_NB_cancer$Prop_cancer))*180/pi

library(mgcv)
library(gratia)



# mod <- betareg(Prop_cancer  ~ fetch + PC1 +  PC2 + PC3 + PC4 + Salinity + Dist_Port , data =  mean_NB_cancer)




mod_1 <- gam(Prop_cancer ~ s(fetch,  k = 4) + s(CA2, k=4) + fPort,  data =  mean_NB_cancer, family = "betar")

# At!!!
# Salinity and fetch seems to be concurvative

# mod_1 <- gam(Prop_cancer ~ s(fetch, k=4, by = fYear, bs = "cr") + s(PC1, k=3, by = fYear, bs = "cr") + fSal   + fPort + fYear,  data =  mean_NB_cancer, family = "betar")


mod_2 <- gam(Prop_cancer ~ s(fetch, k=4, by = fYear, bs = "cr") + s(Salinity, k=4, by = fYear, bs = "cr") + CA1 + CA2 + fPort + fYear,  data =  mean_NB_cancer, family = "betar")

# mod_2 <- gam(Prop_cancer ~ s(fetch, k=4, by = fYear, bs = "cr") +  fSal + PC1  + fPort + fYear,  data =  mean_NB_cancer, family = "betar")



AIC(mod_1, mod_2)

appraise(mod_1)
concur <- concrvity(mod_1)
draw(concur)



draw(mod_1, parametric = T, residuals = T)

summary(mod_1)

ggplot(mean_NB_cancer, aes(x = CA2, y = Prop_cancer)) +
  geom_point() +
  facet_wrap(~fYear)



ggplot(mean_NB_cancer, aes(x = factor(Year), y = Prop_cancer)) +
  geom_boxplot()



## Рисуем ординацию сайтов в осях PC1 и PC2

library(reshape2)
library(tidyr)

df<-
mean_NB_cancer %>% 
  filter(Site %in% c("KHOL", "MAM", "MAR_II", "NUK_I", "UM", "VES")) %>% 
  melt(., id.vars = c("Site", "Year") ) %>% 
  filter(variable %in% c("PC1", "PC2"))

df$value <- as.numeric(df$value)


df <- 
df %>%  spread(Site, value) %>% select(-variable) %>% t() %>% as.data.frame() 

df <- df[-1, ]

names(df) <- c("PC1_21", "PC2_21","PC1_23", "PC2_23" )



ggplot(mean_NB_cancer, aes(x = PC1, y = PC2)) +
  geom_point(aes(size = (Prop_cancer) )) +
  geom_segment(data = df, aes(x = PC1_21, y = PC2_21, xend = PC1_23, yend = PC2_23), color = "blue", arrow = arrow(type = "closed", angle = 10))


df2 <- merge(pca_scores_scam, scam)
  

ggplot(df2, aes(x = PC1, y = log(L8+1) )) +
  geom_point()


## Анализ только на основе данных 2023 года ##############

#Обилие Раковых мидий #######################3

cancer <- read_excel("Data/Cancer_Magadan_2021-2023.xlsx", na = "NA", sheet = "Magadan_cancer_prevalence")

cancer2 <-
  cancer %>%
  group_by(Year, Site) %>%
  summarise(N_processed = sum(N_Processed), N_cancer = sum(Aneuploid))


mean_cancer <-
  cancer2 %>%
  group_by(Year, Site) %>%
  summarise(N_processed = sum(N_processed), N_cancer = sum(N_cancer)) %>%
  mutate(Prop_cancer = N_cancer/N_processed) %>% 
  filter(Year == 2023)


# Размерная структура 2023 #############

library(vegan)

size <- read_excel("Data/Magadan_2021_2023_ecology.xlsx", na = "NA", sheet = "Размерная струкутра 2023 2021")
size <- size[complete.cases(size), ]

scam <- dcast(Year + Site ~ Size_class, data = size)

area <- read_excel("Data/Magadan_2021_2023_ecology.xlsx", na = "NA", sheet = "Площадь проб на размер")

sample_area <- 
  area %>% group_by(Year, Site) %>% summarise(Total_area = sum(Area))

scam <- 
  scam %>% group_by(Year, Site)


sum(scam$Site != sample_area$Site)



scam [ ,3:ncol(scam)] <- 
  round((scam[ ,3:ncol(scam)] / sample_area$Total_area) *10000, 0)






scam_23 <- scam %>% filter(Year == 2023)

# scam_23_log <- decostand(scam_23[ , -c(1,2)], method = "log")


library(vegan)

pca_scam_23 <- cca(scam_23[ , -c(1,2)])

summary(pca_scam_23)

plot(pca_scam_23, display = "species")
plot(pca_scam_23, display = "sites")

pca_scam_23_size_scores <- as.data.frame(scores(pca_scam_23)$species)

pca_scam_23_size_scores %>% arrange(CA1)


pca_scores_scam_23 <- data.frame(Year = scam_23$Year, Site = scam_23$Site, as.data.frame(scores(pca_scam_23)$sites))



pca_scores_scam_23 %>% arrange(desc(CA1))

ggplot(pca_scores_scam_23, aes(CA1, CA2)) +
  geom_text(aes(label = Site))


mean_NB_cancer_23 <- 
  merge(fetch.df, mean_cancer, all.x = T) %>% 
  filter(Year == 2023)


mean_NB_cancer_23 <- merge(mean_NB_cancer_23, pca_scores_scam_23)

unique(mean_NB_cancer_23$Site)

# mean_NB_cancer <- mean_NB_cancer %>% mutate(Prop_cancer = Aneuploid/N_Processed)

str(mean_NB_cancer_23)

mean_NB_cancer_23$Prop_cancer[mean_NB_cancer_23$Prop_cancer == 0] <- 0.00000001

mean_NB_cancer_23 <- mean_NB_cancer_23 %>% mutate(fPort = factor(ifelse(Site %in% c("KHOL", "MAM", "MAR_II", "MCHK", "PORT"), "Close", "Distant")))


cover <- read_excel("Data/Magadan_2021_2023_ecology.xlsx", sheet = "Покрытия миидий 2023")

mean_cover <-
cover %>%
  group_by(Site) %>%
  summarise(Cover = mean(`Number of squares`/30))

mean_NB_cancer_23 <- 
merge(mean_NB_cancer_23, mean_cover)


library(mgcv)
library(gratia)
library(betareg)

mod <- gam(Prop_cancer ~ s(fetch, k = 4, bs = "cr") + s(Salinity, k = 4, bs = "cr") + s(Cover, k = 4, bs = "cr") + CA1 + CA2 + fPort,  data =  mean_NB_cancer_23, family = "betar")


appraise(mod, type = "deviance")

draw(mod, parametric = T)

# vif(mod)
summary(mod)

ggplot(mean_NB_cancer_23, aes(x = Salinity, y = Prop_cancer)) +
  geom_point()

concrvity(mod)


mod_beta <- betareg(Prop_cancer ~ scale(Cover) + scale(CA1) + fPort + scale(fetch),  data =  mean_NB_cancer_23)


vif(mod_beta)

summary(mod_beta)

plot(mod_beta)

plot(mean_NB_cancer_23$CA1, mean_NB_cancer_23$Prop_cancer)
plot(mean_NB_cancer_23$fetch, mean_NB_cancer_23$Prop_cancer)




