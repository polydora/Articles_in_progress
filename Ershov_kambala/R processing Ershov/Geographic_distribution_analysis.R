
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






wsbs <- c("S14", "S21", "S16", "S02", "S01", "S32", "S33", "S03", "S34", "S05", "S35", "S24", "S04")

wsbs_sites.env <- sites.environment %>% filter(Site %in% wsbs)

sites.env <- sites.environment %>% filter(!(Site %in% wsbs))





library(betareg)
library(car)


mod <- betareg(Left_prop ~  Sal +  Lon, data = sites.environment)

vif(mod)

# plot(mod)
summary(mod)







my.sites$Name <- paste("S",1:nrow(my.sites), sep = "_")

m <- leaflet()
m <- addTiles(m)
m <- addMarkers(m, lng=sites$Lon, lat=sites$Lat, popup=sites$Site)
m <- addCircleMarkers(m, lng=sites$Lon, lat=sites$Lat, radius = 5)

addCircleMarkers(m, lng=initial_sites$Lon1, lat=initial_sites$Lat1, radius = 5, color = "green")

