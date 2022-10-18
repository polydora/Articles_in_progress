
library(sdmpredictors)
library(leaflet)
library(readxl)
# library(dplyr)



sites <- read_excel("Data/Points_coordinates.xlsx")

sites$Lat <- as.numeric(gsub(",", ".", sites$Lat))
sites$Lon <- as.numeric(gsub(",", ".", sites$Lon))
sites$Left_prop <- as.numeric(gsub(",", ".", sites$Left_prop))





library(raster)

salinity = raster("D:/Data_LMBE/BIO_Oracle_predictors/Present.Surface.Salinity.Mean.asc")
temp = raster("D:/Data_LMBE/BIO_Oracle_predictors/Present.Surface.Temperature.Mean.asc")
silicate = raster("D:/Data_LMBE/BIO_Oracle_predictors/Present.Surface.Silicate.Mean.asc")
prim_prod = raster("D:/Data_LMBE/BIO_Oracle_predictors/Present.Surface.Primary.productivity.Mean.asc")
phytoplancton = raster("D:/Data_LMBE/BIO_Oracle_predictors/Present.Surface.Phytoplankton.Mean.asc")
phosphate = raster("D:/Data_LMBE/BIO_Oracle_predictors/Present.Surface.Phosphate.Mean.asc")
ph = raster("D:/Data_LMBE/BIO_Oracle_predictors/Present.Surface.pH.BOv2_2.asc")
nitrate = raster("D:/Data_LMBE/BIO_Oracle_predictors/Present.Surface.Nitrate.Mean.asc")
dissolved_o2 = raster("D:/Data_LMBE/BIO_Oracle_predictors/Present.Surface.Dissolved.oxygen.Mean.asc")
current_velocity = raster("D:/Data_LMBE/BIO_Oracle_predictors/Present.Surface.Current.Velocity.Mean.asc.BOv2_1.asc")
chlorophyll = raster("D:/Data_LMBE/BIO_Oracle_predictors/Present.Surface.Chlorophyll.Mean.asc")



aoi <- extent(32,34,66.2,66.4)

sal_crp <- crop(salinity,aoi)

plot(sal_crp)








# Скачиваем данные по средней температуре воды на срденей глубине и срденей солености на срденей глубине. 
# environment.bottom <- load_layers( layercodes = c("BO2_tempmean_bdmean" ,  "BO2_salinitymean_bdmean",  "BO2_curvelmax_bdmean", 	"BO2_dissoxmin_bdmean", "BO2_ironmean_bdmean", "BO2_phosphatemean_bdmean", "BO2_carbonphytomean_bdmean", "BO2_ppmean_bdmean") , equalarea=FALSE, rasterstack=TRUE)


# Explore layers in a dataset
# layers <- list_layers()
# str(layers)

# layers_correlation(layers)



# bathymetry <- load_layers("BO_bathymean")

# points <- read.csv("Data/point_proportion.csv")
 



# initial_sites <- data.frame(Lon1=sites$Long + sites$Long_m/60, Lat1= sites$Lat + sites$Lat_m/60)



my.sites <- as.matrix(sites[, 4:3])


my.sites



sites.environment <- data.frame(Lat = sites$Lat, Lon = sites$Lon,
                                Site = sites$Site,
                                Left_prop = sites$Left_prop/100,
                                Temp = extract(temp,my.sites[,1:2]),
                                Sal = extract(salinity,my.sites[,1:2]),
                                Prim_prod = extract(prim_prod,my.sites[,1:2]),
                                Chlorophyll = extract(chlorophyll,my.sites[,1:2]),
                                Silicate = extract(silicate,my.sites[,1:2]),
                                Phytopl = extract(phytoplancton,my.sites[,1:2]),
                                Phosph = extract(phosphate,my.sites[,1:2]),
                                pH = extract(ph,my.sites[,1:2]),
                                Nitr = extract(nitrate,my.sites[,1:2]),
                                Dissolv_O2 = extract(dissolved_o2,my.sites[,1:2]),
                                Current_V = extract(current_velocity,my.sites[,1:2])
                                )





library(dplyr)

wsbs <- c("S14", "S21", "S16", "S02", "S01", "S32", "S33", "S03", "S34", "S05", "S35", "S24", "S04")

wsbs_sites.env <- sites.environment %>% filter(Site %in% wsbs)


ws <- c("S01", "S02","S03", "S04", "S05", "S24", "S33", "S34", "S35")


sites.env <- sites.environment %>% filter(!(Site %in% wsbs))





library(betareg)
library(car)


mod <- betareg(Left_prop ~  Sal +  Lon, data = sites.environment)

vif(mod)

# plot(mod)
summary(mod)






# Расстояние от географичского центра точек, вовлеченных в метаанализ. 

# Вычисление географического центра  
sites.environment %>% filter(!Site %in% c("S01", "S03", "S04", "S05")) %>% select(Lat, Lon) %>% summarise_all(.funs = mean)

sites.environment %>% filter(!Site %in% wsbs) %>% select(Lat, Lon) %>% summarise_all(.funs = mean)


# В качестве точки отсчета берется центр области, где изучена частота левосторонних
dist_1 <- acos(sin(55.36654   * pi/180)*sin(sites.environment$Lat*pi/180) + cos(55.36654  *pi/180)*cos(sites.environment$Lat*pi/180)*cos(9.046538*pi/180 - sites.environment$Lon*pi/180)) * 6371



# В качестве точки отсчета берется центр Европейской части ареала по GBIF
dist_2 <- acos(sin(54.29339  * pi/180)*sin(sites.environment$Lat*pi/180) + cos(54.29339 *pi/180)*cos(sites.environment$Lat*pi/180)*cos(8.772293 * pi/180 - sites.environment$Lon*pi/180)) * 6371


sites.environment$Dist_1 <- dist_1

sites.environment$Dist_2 <- dist_2



sites.environment_bsns <- sites.environment %>% filter(!Site %in% wsbs)



# 
# sites.environment_meta <- sites.environment %>% filter(!Site %in% c("S01", "S03", "S04", "S05"))

names(sites.environment)
  
# Полная модель
# mod <- betareg(Left_prop ~  Temp + Sal + Prim_prod + Chlorophyll + Silicate + Phytopl + Phosph + pH + Nitr + Dissolv_O2 + Current_V + Dist_2, data = sites.environment)


mod <- betareg(Left_prop ~  Sal +  Dist_1, data = sites.environment_bsns)

summary(mod)




# mod <- betareg(Left_prop ~   Prim_prod + Silicate + Phosph  + Dist_2, data = sites.environment)

vif(mod)






our_data <- sites.environment %>% filter(Site %in% c("S01", "S03", "S04", "S05"))

predict(mod, newdata = our_data)


####







my.sites$Name <- paste("S",1:nrow(my.sites), sep = "_")

m <- leaflet()
m <- addTiles(m)
m <- addMarkers(m, lng=sites$Lon, lat=sites$Lat, popup=sites$Site)

m <- addMarkers(m, lng=sites$Lon, lat=sites$Lat, popup=sites$Site)

m <- addCircleMarkers(m, lng=occurence$decimalLongitude, lat=occurence$decimalLatitude, radius = 5)

addCircleMarkers(m, lng=initial_sites$Lon1, lat=initial_sites$Lat1, radius = 5, color = "green")






#' GBIF.org (28 September 2022) GBIF Occurrence Download  https://doi.org/10.15468/dl.qe7bf3
#' 
#' @misc{https://doi.org/10.15468/dl.qe7bf3,
#'   doi = {10.15468/DL.QE7BF3},
#'   url = {https://www.gbif.org/occurrence/download/0026557-220831081235567},
#'   author = {{GBIF.Org User}},
#'   keywords = {GBIF, biodiversity, species occurrences},
#'   title = {Occurrence Download},
#'   publisher = {The Global Biodiversity Information Facility},
#'   year = {2022},
#'   copyright = {Creative Commons Attribution Non Commercial 4.0 International}
#' } 
#' 
#' 


# Анализ ареала камбалы

# Загружаем датасет с координатами встреч вида, загруженного из GBIF. 


library(dplyr)

gbif <- read.table("Data/GBIF_flunder_occurences_coordinates.csv", sep = "\t", header =  T)

nrow(gbif)

gbif %>% unique(.) %>%  nrow()

occurence <- gbif %>% unique(.) 

Mean_coordinates <- gbif %>% unique(.) %>%  summarise_all(.funs = mean)


m <- leaflet()
m <- addTiles(m)
m <- addMarkers(m, lng=sites$Lon, lat=sites$Lat, popup=sites$Site)

m <- addCircleMarkers(m, lng=occurence$decimalLongitude, lat=occurence$decimalLatitude, radius = 0.1)
m




# Изменение частоты левосторонних при движении с запада на восток


wsbs_order <- data.frame(Site = c("S14", "S21", "S16", "S02", "S01", "S32", "S33", "S03", "S34", "S35", "S05", "S04", "S24"))

wsbs_order$Order <- 1:nrow(wsbs_order)

sites.environment_wsbs <- sites.environment %>% filter(Site %in% wsbs_order$Site)

ddd <- merge(sites.environment_wsbs, wsbs_order)



mod2 <- betareg(Left_prop ~ Sal +  Dist_1, data = sites.environment_wsbs)

summary(mod2)
vif(mod2)



# mod <- betareg(Left_prop ~   Prim_prod + Silicate + Phosph  + Dist_2, data = sites.environment)

vif(mod)


wsbs <- c("S14", "S21", "S16", "S02", "S01", "S32", "S33", "S03", "S34", "S05", "S35", "S24", "S04")


sites.environment$Area <- ifelse(sites.environment$Site %in% wsbs, "Arctic", "European")


ggplot(sites.environment, aes(x = Area, y = Left_prop)) + geom_boxplot()


mod3 <- betareg(Left_prop ~  Area / Dist_1, data = sites.environment)

summary(mod3)
plot(mod3)

vif(mod3)
