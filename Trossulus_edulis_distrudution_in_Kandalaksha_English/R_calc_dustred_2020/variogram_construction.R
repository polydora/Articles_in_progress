library(gstat)
library(sp)

summary(Mod_gam)


E <- resid(Mod_gam, type = "pearson")

# E <- myt_full$Prop_T


df <- data.frame(x = myt_full$Lon, y = myt_full$Lat, z = E)


coordinates(df)= ~ x+y

bubble(df, zcol='z', fill=TRUE, do.sqrt=F, maxsize=3)

TheVariogram=variogram(z~1, data=df)
plot(TheVariogram)

TheVariogramModel <- vgm(psill=0.15, model="Gau", nugget=0.0001, range=5)
plot(TheVariogram, model=TheVariogramModel)


FittedModel <- fit.variogram(TheVariogram, model=TheVariogramModel)    
plot(TheVariogram, model=FittedModel)

#######################3

E <- resid(Model_2, type = "pearson")

# E <- myt_full$Prop_T


df <- data.frame(x = myt_site_substr$Lon, y = myt_site_substr$Lat, z = E)


coordinates(df)= ~ x+y

bubble(df, zcol='z', fill=TRUE, do.sqrt=F, maxsize=3)

TheVariogram=variogram(z~1, data=df)
plot(TheVariogram)

TheVariogramModel <- vgm(psill=0.15, model="Gau", nugget=0.0001, range=5)
plot(TheVariogram, model=TheVariogramModel)


FittedModel <- fit.variogram(TheVariogram, model=TheVariogramModel)    
plot(TheVariogram, model=FittedModel)



