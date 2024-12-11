

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


my.sites <- as.matrix(btn[, 7:8])

# Примерные координаты 
# Залив Восток, МБС Восток
my.sites[3,1] <- 42.89397 
my.sites[3,2] <- 132.7337

# Abram Mys, AB, Kola Bay
my.sites[62, 1] <- 68.98204 
my.sites[62, 2] <- 33.02860 



current <- extract(current_velocity, my.sites[, 2:1])

sal <- extract(salinity, my.sites[, 2:1])

temper <- extract(temp, my.sites[, 2:1])

chl <- extract(chlorophyll, my.sites[, 2:1])

o2 <-  extract(dissolved_o2, my.sites[, 2:1])

phosphate <- extract(phosphate, my.sites[, 2:1])





btn1_predictors <- data.frame(current, sal, temper, chl, o2, phosphate, N_FC = btn$N_FC, N = btn$BTN1*btn$N_FC, BTN_Type = "BTN1")
btn2_predictors <- data.frame(current, sal, temper, chl,o2, phosphate, N_FC = btn$N_FC, N = btn$BTN2*btn$N_FC, BTN_Type = "BTN2")








btn1 <- 
  btn1_predictors %>% 
  mutate(Prop_corrected = case_when(Prop_BTN == 1 ~ 0.9999999,
                                              Prop_BTN == 0 ~ 0.0000001,
                                              Prop_BTN >0 & Prop_BTN < 1 ~ Prop_BTN),
         BTN_Type = "BTN1")


btn2 <- 
  btn2_predictors %>% 
  mutate(Prop_corrected = case_when(Prop_BTN == 1 ~ 0.9999999,
                                    Prop_BTN == 0 ~ 0.0000001,
                                    Prop_BTN >0 & Prop_BTN < 1 ~ Prop_BTN),
         BTN_Type = "BTN2")



btns <- rbind(btn1_predictors, btn2_predictors)

btns$BTN_Type <- factor(btns$BTN_Type)

# btns$Fi_BTN <- 2*asin(sqrt(btns$Prop_BTN )) * 180/pi
  
cor(btns[,1:8])
  
library(mgcv)
library(gratia)

btns$Prop <- btns$N / btns$N_FC
btns$Fi_BTN <- 2*asin(sqrt(btns$Prop)) * 180/pi


mod <- gam(Prop ~ s(current, by = BTN_Type, bs = "cs") + s(sal, by = BTN_Type, bs = "cs") + s(temper, by = BTN_Type, bs = "cs") + s(temper, by = BTN_Type, bs = "cs") + s(chl, by = BTN_Type, bs = "cs")  + BTN_Type, family = tw(link = "log"), method = "REML", data = btns) 


gam.check(mod)

appraise(mod)

summary(mod)


draw(mod, overall_uncertainty = F)

draw(mod,overall_uncertainty = F,  grouped_by = F, select = c(1, 5, 7, 8), residuals = T)


ggplot(btn2_predictors, aes(x = temper, y = N/N_FC )) +
  geom_point()



#########################################

# SDM для данных из базы World_DN_prevalence.xlsx


world_dn <- read_excel("Data/World_DN_prevalence.xlsx", na = "NA")

world_dn %>% 
  filter(Cancer_Type == "BTN") -> btn_hummel

my.sites_hum <- 
  btn_hummel %>% 
  dplyr::select(Lon, Lat) %>% 
  as.matrix()

current2 <- extract(current_velocity, my.sites_hum)

sal2 <- extract(salinity, my.sites_hum)

temper2 <- extract(temp, my.sites_hum)

chl2 <- extract(chlorophyll, my.sites_hum)

o22 <-  extract(dissolved_o2, my.sites_hum)

# phosphate2 <- extract(phosphate, my.sites[, 2:1])


btn_hummel_predictors <- data.frame(current = current2, sal = sal2, temper = temper2, chl = chl2, o2 = o22,   Prop = btn_hummel$Prevalence, Lon = btn_hummel$Lon, Lat = btn_hummel$Lat, BTN_Type = "BTN2")




library(mgcv)
library(gratia)

mod_BTN2_humm <- gam(Prop ~ s(current, bs = "cs") + s(sal,  bs = "cs") + s(temper,  bs = "cs") + s(temper, bs = "cs") + s(chl,  bs = "cs"), family = tw(link = "log"), method = "REML", data = btn_hummel_predictors)

appraise(mod_BTN2_humm)

summary(mod_BTN2_humm)


predicted <- predict(mod_BTN2, newdata = btn_hummel_predictors, type = "response")

qplot(btn_hummel_predictors$Prop, predicted)
