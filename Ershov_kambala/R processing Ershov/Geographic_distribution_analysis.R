
library(sdmpredictors)
library(leaflet)

# options(sdmpredictors_datadir="C:\\Users\\polyd\\AppData\\Local\\Temp\\RtmpuyXZRU/sdmpredictors")


# Скачиваем данные по средней температуре воды на срденей глубине и срденей солености на срденей глубине. 
environment.bottom <- load_layers( layercodes = c("BO2_tempmean_bdmean" ,  "BO2_salinitymean_bdmean",  "BO2_curvelmax_bdmean", 	"BO2_dissoxmin_bdmean", "BO2_ironmean_bdmean", "BO2_phosphatemean_bdmean", "BO2_carbonphytomean_bdmean", "BO2_ppmean_bdmean") , equalarea=FALSE, rasterstack=TRUE)



# Explore layers in a dataset
layers <- list_layers()
str(layers)



# bathymetry <- load_layers("BO_bathymean")

points <- read.csv("Data/point_proportion.csv")
 


sites <- read.csv("Data/point_coordinates.csv")




my.sites <- data.frame(Lon=sites$Long + sites$Long_m/60, Lat= sites$Lat + sites$Lat_m/60)

my.sites[2,] <- c(37.898529, 64.020950)

my.sites[8,] <- c(5.404947, 53.530350) 

my.sites[11,] <- c(-4.428864, 52.229355) 

my.sites[13,] <- c(22.639058, 58.182786)

my.sites[17,] <- c(-4.257812, 50.334937) 

my.sites[18,] <- c(8.327916, 54.593484) 

my.sites[22,] <- c(-4.182711, 50.305612)

my.sites[23,] <- c(1.007030, 51.605326)

my.sites[26,] <- c(10.836218, 58.905289)

my.sites[35,] <- c(40.098553, 64.879995)


library(raster)
my.sites.environment <- data.frame(Lat = my.sites$Lat, Lon = my.sites$Lon, extract(environment.bottom,my.sites[,1:2]) )


names(my.sites.environment) <- c("Lat", "Lon", "Temp", "Sal", "Curve", "Dissox", "Iron", "Phosph", "Carb_phyto", "PrProd")




my.sites.environment$Left_prop <- points$Left_prop/100


library(ggplot2)

ggplot(my.sites.environment, aes(x = PrProd, y = Left_prop )) + geom_point()


library(betareg)
library(car)

mod <- betareg(Left_prop ~   Temp + Sal + Curve  +  Phosph + PrProd, data = my.sites.environment)


my.sites.environment$Fi_left <- 2*asin(sqrt(my.sites.environment$Left_prop))*180/3.14



mod2 <- lm(Fi_left ~   Temp + Sal + Curve  +  Phosph + PrProd, data = my.sites.environment)



vif(mod)
summary(mod)

plot(mod)


plot(mod2)
summary(mod2)
