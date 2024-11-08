library(gstat)
library(sp)

library(ncf)






summary(Mod_btn1_btn2)


E <- resid(Mod_btn1_btn2, type = "pearson")


df <- data.frame(x = cancer_2023_long$lon, y = cancer_2023_long$lat)


coordinates(df)= ~ x+y

bubble(df, zcol= E, fill=TRUE, do.sqrt=F, maxsize=3)

TheVariogram=variogram(z~1, data=df)
plot(TheVariogram)

TheVariogramModel <- vgm(psill=0.15, model="Sph", nugget=0.0001, range=5)


FittedModel <- fit.variogram(TheVariogram, model=TheVariogramModel, fit.method = 6)    
plot(TheVariogram, model=FittedModel)



library(ncf)


# df <- data.frame(x = cancer_2023$lon, y = cancer_2023$lat, z = cancer_2023$Prop_BTN1)


myt <- read_excel("Data/summary table_Magadan_itog.xlsx", sheet = "data")

myt %<>%
  mutate(BTN2 = BTN2.1 + BTN2.2 + BTN2.SAM)


myt <-
  myt %>% 
  select(-Site) %>% 
  mutate(Site = Site_code, Prop_BTN1 = BTN1/N, Prop_BTN2 = BTN2/N)

myt_23 <-
  myt %>% 
  filter(Year == 2023)

library(reshape2)

df_Prop_BTN1 <- dcast(data = myt_23, formula = Site_code ~ Sample, value.var = "Prop_BTN1") 

df_Prop_BTN2 <- dcast(data = myt_23, formula = Site_code ~ Sample, value.var = "Prop_BTN2") 



df_coord <- 
  myt_23 %>% 
  group_by(Site_code) %>% 
  summarise(Lat = mean(Lat), Lon = mean(Lon))




df <- data.frame(x = myt$Lon, y = myt$Lat, z_BTN1 = myt$Prop_BTN1, z_BTN2 = myt$Prop_BTN2 )


fit_lisa <- lisa(df$x, df$y, df$z_BTN1, neigh = 3, latlon = TRUE)

plot(fit_lisa)

summary(fit_lisa)


fit_corelogr_BTN1 <- spline.correlog(x = df_coord$Lon, y = df_coord$Lat, z = df_Prop_BTN1[,-1], latlon = T, type = "perm", resamp = 10000, na.rm = T)

plot(fit_corelogr_BTN1)


fit_corelogr_BTN2 <- spline.correlog(x = df_coord$Lon, y = df_coord$Lat, z = df_Prop_BTN2[,-1], latlon = T, type = "perm", resamp = 10000, na.rm = T)

plot(fit_corelogr_BTN2)



summary(fit_corelogr)




# first generate some sample data
x <- expand.grid(1:20, 1:5)[, 1]
y <- expand.grid(1:20, 1:5)[,2]

# z data from an exponential random field
z <- cbind(
  rmvn.spa(x = x, y = y, p = 2, method = "exp"), 
  rmvn.spa(x = x, y = y, p = 2, method = "exp")
)

# lisa.nc analysis
fit1 <- lisa.nc(x = x, y = y, z = z, neigh = 3)
## Not run: plot(fit1)

qplot(x = fit1$dmean, y = fit1$correlation)



x <- myt$Lon
y <- myt$Lat


z <- myt %>% mutate(Prop_BTN1 = BTN1/N, Prop_BTN2 = BTN2/N)  %>% select(Prop_BTN1, Prop_BTN2)

fit <- lisa.nc(x = x, y = y, z = z, neigh = 5, latlon = TRUE, na.rm = TRUE)
