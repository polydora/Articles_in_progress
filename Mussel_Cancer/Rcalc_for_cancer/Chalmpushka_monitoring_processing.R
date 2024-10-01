library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)


myt <- read_excel("Data/Chalmpushka_Monitoring.xlsx")


myt$DOY <- yday(myt$Date)
myt$Year <- year(myt$Date)
myt$Prop_Cancer <- myt$Cancer/myt$N 


myt$Prop_Cancer 



library(mgcv)
library(gratia)


mod <- gam(cbind(Cancer, Healthy) ~ s(DOY, bs = "cr") + s(Year, k = 5), data = myt, family = "binomial", method = "REML") 

mod <- gam(Prop_Cancer ~ s(DOY) + s(Year, k = 3), data = myt, family = "betar", method = "REML") 

mod <- gam(Prop_Cancer ~ s(DOY) + s(Year, k = 3), data = myt, method = "REML") 


appraise(mod)

summary(mod)

draw(mod, residuals = T)

ggplot(myt, aes(DOY, y = Cancer/N)) + geom_point()

