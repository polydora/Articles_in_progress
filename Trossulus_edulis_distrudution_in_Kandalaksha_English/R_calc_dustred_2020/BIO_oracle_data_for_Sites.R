library(raster)

salinity = raster("D:/Data_LMBE/BIO_Oracle_predictors/Present.Surface.Salinity.Mean.tif")
temp = raster("D:/Data_LMBE/BIO_Oracle_predictors/Present.Surface.Temperature.Mean.asc")


my.sites <- as.matrix(myt_site[, 2:3])


sal <- extract(salinity, my.sites[, 2:1])

temper <- extract(temp, my.sites[, 2:1])

hist(temper)
hist(sal)
hist(myt_site$Salinity)


qplot(myt_site$Salinity, sal)
