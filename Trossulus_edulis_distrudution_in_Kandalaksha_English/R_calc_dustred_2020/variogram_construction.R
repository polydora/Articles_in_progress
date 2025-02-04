library(gstat)
library(sp)
library(ncf)






summary(Mod_gam)


E <- resid(Mod_gam, type = "pearson")

######################################
# Создаем искусственный датасет со встроенной пространственной автокорреляцией

library(DHARMa)
testData = createData(sampleSize = 100, family = poisson(), spatialAutocorrelation = 2)

fittedModel <- glmer(observedResponse ~ Environment1 + (1|group), data = testData, family = poisson() )

simulationOutput2 <- simulateResiduals(fittedModel = fittedModel, plot = T)

testSpatialAutocorrelation(simulationOutput = simulationOutput2, x = testData$x, y= testData$y)

E <- resid(fittedModel, type = "pearson")
########################################

# E <- myt_full$Prop_T




df <- data.frame(x = myt_full$Lon, y = myt_full$Lat, z = E)

df <- data.frame(x = testData$x, y = testData$y, z = E)

coordinates(df)= ~ x+y

bubble(df, zcol='z', fill=TRUE, do.sqrt=F, maxsize=3)

TheVariogram=variogram(z~1, data=df)
plot(TheVariogram)

library(mgcViz)
gam



TheVariogramModel <- vgm(psill=0.15, model="Sph", nugget=0.0001, range=5)


FittedModel <- fit.variogram(TheVariogram, model=TheVariogramModel)    
plot(TheVariogram, model=FittedModel)



fit_lisa <- lisa(df$x, df$y, df$z, neigh = 3)

plot(fit_lisa)

summary(fit_lisa)


fit_corelogr <- spline.correlog(df$x, df$y, df$z)
plot(fit_corelogr)

summary(fit_corelogr)


library(ncf)



fit_corelogr <- spline.correlog(myt_full$Lon, myt_full$Lat, myt_full$Ptros)
plot(fit_corelogr)

summary(fit_corelogr)


fit_corelogr <- spline.correlog(myt_full$Lon, myt_full$Lat, residuals(Mod_gam, type = "pearson"))
plot(fit_corelogr)

