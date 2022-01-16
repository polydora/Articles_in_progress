install.packages("sdmpredictors")
install.packages("leaflet")

# Load package
library(sdmpredictors)

# Explore datasets in the package
list_datasets()

# Explore layers in a dataset
layers <- list_layers()
str(layers)

# Download specific layers to the current directory
bathy <- load_layers(c("BO_bathymin", "BO_bathymean", "BO_bathymax"))

str(bathy)

# Check layer statistics
layer_stats()

# Check Pearson correlation coefficient between layers
layers_correlation()




# Load package
library(sdmpredictors)

library(raster)
options(sdmpredictors_datadir="<directory>")

# Easy download of raster file (Maximum Temperature at the sea bottom)
temp.max.bottom <- load_layers("BO2_tempmax_bdmax")


# Easy download of raster file (Salinity)
sal <- load_layers("BO_salinity")



# Crop raster to fit the North Atlantic
ne.atlantic.ext <- extent(50, 100, 64, 77)
temp.max.bottom.crop <- crop(temp.max.bottom, ne.atlantic.ext)

sal.crop <- crop(sal, ne.atlantic.ext)


# Generate a nice color ramp and plot the map
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127"))
plot(temp.max.bottom.crop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(cex.sub = 1.25, sub = "Maximum temperature at the sea bottom (ºC)")

# Generate a nice color ramp and plot the map
my.colors = colorRampPalette(c("#5E85B8","#EDF0C0","#C13127"))
plot(sal.crop,col=my.colors(1000),axes=FALSE, box=FALSE)
title(cex.sub = 1.25, sub = "Salinity (PSU)")


#######################

library(sdmpredictors)
library(leaflet)

# List layers avaialble in Bio-ORACLE v2
layers.bio2 <- list_layers( datasets="Bio-ORACLE" )
layers.bio2

# Download environmental data layers (Max. Temperature, Min. Salinity and Min. Nitrates at the sea bottom)
environment.bottom <- load_layers( layercodes = c("BO2_tempmax_bdmean" , "BO2_salinitymin_bdmean", "BO2_nitratemin_bdmean") , equalarea=FALSE, rasterstack=TRUE)

# Download bathymetry
bathymetry <- load_layers("BO_bathymean")

# Generate a data.frame with the sites of interest
my.sites <- data.frame(Name=c("Faro, Portugal, NE Atlantic" , "Maspalomas, Spain, NE Atlantic" , "Guadeloupe, France, Caribbean Sea" , "Havana, Cuba, Caribbean Sea") , Lon=c(-7.873,-15.539,-61.208,-82.537) , Lat=c(37.047, 27.794,15.957,23.040 ) )
my.sites

# Visualise sites of interest in google maps
m <- leaflet()
m <- addTiles(m)
m <- addMarkers(m, lng=my.sites$Lon, lat=my.sites$Lat, popup=my.sites$Name)
m

# Extract environmental values from layers
my.sites.environment <- data.frame(Name=my.sites$Name , depth=extract(bathymetry,my.sites[,2:3]) , extract(environment.bottom,my.sites[,2:3]) )
my.sites.environment


########### Data on strams


### Install the libraries
if (!require("raster")) {  install.packages("raster", dependencies = TRUE) ; library(raster)}
if (!require("rgdal")) {  install.packages("rgdal", dependencies = TRUE) ; library(rgdal)}
if (!require("ncdf4")) {  install.packages("ncdf4", dependencies = TRUE) ; library(ncdf4)} # manual install below
if (!require("maps")) {  install.packages("maps", dependencies = TRUE) ; library(maps)}
if (!require("foreach")) {  install.packages("foreach", dependencies = TRUE) ; library(foreach)}
if (!require("doParallel")) {  install.packages("doParallel", dependencies = TRUE) ; library(doParallel)}



### Set working directory
# dir <- "c:/freshwater_variables"

dir <- "D:/Data_LMBE/Obskaya Bay additional data/freshwater_variables/"
dir.create(dir)
setwd(dir)

library(raster); library(ncdf4); library(maps); library(foreach); library(doParallel)

download.file("http://data.earthenv.org/streams/landcover_average.nc", 
              paste(getwd(), "landcover_average.nc", sep="/"), mode = "wb")


### Load the  layers into a raster brick
temp_avg <- brick("monthly_tmin_average.nc")

### Check the number of layers
nlayers(temp_avg)


### Check the metadata for units, scale factors etc.
nc_open("monthly_tmin_average.nc")


### Add layer names. See Table S1 or the ReadMe for the sequence of the single layers 
names(temp_avg) <- paste(c("temp_avg"), sprintf("%02d", seq(1:12)), sep="_")


x11(10.3); #открывает графическое окно 
map('world'); 
abline(h=60, lty=5, lwd=2, col="red"); 
text(-176, 64, "60°N", col="red")


### Crop to smaller extent e.g. by clicking the upper left and lower right corners 
(ext <- drawExtent())

ext <- extent(c(-180,180,0,90))

temp_avg_crop <- foreach(i=names(temp_avg), lc = unstack(temp_avg) ,  .final=stack, .packages = c("raster", "ncdf4")) %dopar% {
  options(rasterNCDF4 = TRUE)
  tmp <- crop(lc, ext, snap="in")
  filename=paste0(i, ".tif")
  writeRaster(tmp, filename=filename, overwrite=TRUE) # write cropped raster files to disk
  print(i)
}



my.sites <- data.frame(species = df_species$species, Lon=df_species$decimalLongitude, Lat= df_species$decimalLatitude, Status = df_species$Status)

library(raster)
my.sites.stream <- data.frame(species = my.sites$species, extract(temp_avg,my.sites[,2:3]), Status = my.sites$Status)






### Convert raster stack into a dataframe
temp_avg_df <- foreach(i=names(temp_avg), lc = unstack(lc_avg_crop), .combine=cbind.data.frame, .packages = c("raster")) %dopar% {
  as.data.frame(lc_avg_crop[[i]], na.rm=T)
}


### Check the layers in the new cropped stack
plot(lc_avg_crop)


### Convert raster stack into a dataframe
lc_avg_crop_df <- foreach(i=names(lc_avg_crop), lc = unstack(lc_avg_crop), .combine=cbind.data.frame, .packages = c("raster")) %dopar% {
  as.data.frame(lc_avg_crop[[i]], na.rm=T)
}


### Check output
head(lc_avg_crop_df); summary(lc_avg_crop_df)
stopCluster(cl) # stop parallel backend
### Remove temporary raster-files on the hard disk
showTmpFiles()
removeTmpFiles()



