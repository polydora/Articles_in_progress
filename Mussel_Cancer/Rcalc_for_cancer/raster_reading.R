

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



plot(current_velocity)


my.sites <- as.matrix(dn[, 3:4])


current <- extract(current_velocity, my.sites[, 2:1])

sal <- extract(salinity, my.sites[, 2:1])

temper <- extract(temp, my.sites[, 2:1])

chl <- extract(chlorophyll, my.sites[, 2:1])



dn3 <- data.frame(current, sal, temper, chl, Proportion_ill = dn$Proportion_ill)

dn3 <- 
dn3 %>% mutate(Proportion_ill_cor = case_when(Proportion_ill == 1 ~ 0.9999,
                                              Proportion_ill == 0 ~ 0.0001,
                                              Proportion_ill >0 & Proportion_ill < 1 ~ Proportion_ill))



library(betareg)


M <- betareg(Proportion_ill_cor ~ current  + sal + temper + chl, data = dn3)

summary(M)


qplot(x = current, y = dn$Proportion_ill) + geom_smooth()

qplot(x = sal, y = dn$Proportion_ill) + geom_smooth()

qplot(x = chl, y = dn$Proportion_ill) + geom_smooth()


plot(myt$Fetch, current)

