library(reshape2)
library(dplyr)
library(stringr)
library(ggplot2)
library(mgcv)
library(gridExtra)
library(broom)
library(tidyr)
library(car)
library(vegan)
library(ggvegan)


cal <- read.table("Data/Calanus_tidy.csv", sep = ",", header = T)


cal$Date <- as.POSIXct(as.Date(cal$Date_text, format = "%d/%m/%Y"))

cal$DOY <- as.numeric(strftime(cal$Date, format = "%j"))

cal_full <- cal %>% filter(complete.cases(cal)) 

cal_full <- cal_full[rowSums(cal_full %>% select("Female", "Male", "cop_V", "cop_IV", "cop_III", "cop.II", "cop.I", "nauplii")) > 0, ]  


nrow(cal_full)
nrow(cal)



predictors <- cal_full %>% select("DOY", "Column_width" , "Depth", "Station", "Temp",  "Sal", "Chl")

cal_abund <- cal_full %>% select("Female", "Male", "cop_V", "cop_IV", "cop_III", "cop.II", "cop.I", "nauplii")/predictors$Column_width 




# # Try MDS
# cal_mds <- metaMDS(cal_abund, autotransform = T)
# 


# plot(cal_mds, display = "sp", type = "t")
# 
# cal_env_fit <- envfit(cal_mds ~ Depth + Temp + Sal + Chl, data = predictors)
# 
# plot(cal_env_fit)
# 
# (cal_env_fit$vectors)
# (cal_env_fit$factors)


# Try RDA

cal_constr <- cca(log(cal_abund + 1)  ~ Depth  + Temp + Station + Sal + Chl, data = predictors, scale = T)

# cal_dbrda <- dbrda((cal_abund + 1)  ~ Depth  + Temp + Sal + Chl, data = predictors, distance = "bray")

plot(cal_constr)

vif.cca(cal_constr)
anova(cal_constr)

anova(cal_constr, by = "axis")

anova(cal_constr, by = "margin")


plot(cal_constr,  display = c("sp", "cn"),  choices = c(1, 2), scaling = "species")


plot(cal_cca,  display = c("sp", "cn"),  choices = c(2, 3), scaling = "species")


inert_comp <- as.data.frame(inertcomp(cal_cca, proportional = T, unity = F))

str(inert_comp)
qplot(inert_comp$CA, inert_comp$CCA)



ggplot(predictors, aes(x = DOY, y = Chl )) + geom_point()  + geom_smooth(method = "loess") 




ggplot(scores, aes(x = DOY, y = PC2)) + geom_point()  + geom_smooth() + facet_wrap(~Station)

ggplot(scores, aes(x = DOY, y = CCA2)) + geom_point()  + geom_smooth()  + facet_wrap(~Station)

ggplot(scores, aes(x = DOY, y = CA1)) + geom_point()  + geom_smooth() + facet_wrap(~Station)

ggplot(scores, aes(x = DOY, y = CCA4)) + geom_point()  + geom_smooth() + facet_wrap(~Station)



stage_inertia <- inertcomp(cal_cca, display = c("species"), proportional = FALSE) 

stage_inertia <- as.data.frame(stage_inertia)

stage_inertia$Stage <- row.names(stage_inertia)


ggplot(stage_inertia, aes(y = CCA, x = CA)) + geom_abline() + geom_text(aes(label = Stage)) + xlim(0, 0.3) + ylim(0, 0.3) 


alpha <- predictors$DOY * 2*pi/365


qplot(predictors$Temp, cal_abund$Female) + geom_smooth(method = "lm")





autoplot(cal_cca)


