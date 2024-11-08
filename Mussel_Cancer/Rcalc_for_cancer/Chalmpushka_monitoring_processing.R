library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)


myt <- read_excel("Data/Chalmpushka_Monitoring.xlsx")


myt$DOY <- yday(myt$Date)
myt$Year <- year(myt$Date)
myt$Prop_Cancer <- myt$Cancer/myt$N 

myt <-
myt %>% 
  mutate(Month = month(Date)) %>% 
  mutate(Season = case_when(
    Month %in% c(11, 12, 1:4) ~ "Winter",
    Month %in% c(5, 6) ~ "Spring",
    Month %in% c(7,8) ~ "Summer",
    Month %in% c(9, 10) ~ "Autumn"
  ))

myt$Prop_Cancer 



library(mgcv)
library(gratia)


mod <- gam(cbind(Cancer, Healthy) ~ s(DOY, bs = "cr") + s(Year, k = 5), data = myt, family = "binomial", method = "REML") 

mod <- gam(Prop_Cancer ~ s(DOY) + s(Year, k = 3), data = myt, family = "betar", method = "REML") 

mod <- gam(Prop_Cancer ~ s(DOY) + s(Year, k = 3), data = myt, method = "REML") 

#####################

mod <- gam(cbind(Cancer, Healthy) ~ Season + s(Year, k = 5), data = myt, family = "binomial", method = "REML") 


mod <- glm(cbind(Cancer, Healthy) ~ Season, data = myt, family = "binomial") 

summary(mod)

# mod <- gam(Prop_Cancer ~ Season + s(Year, k = 3), data = myt, family = "betar", method = "REML") 

# mod <- gam(Prop_Cancer ~ Season + s(Year, k = 3), data = myt, method = "REML") 

MyData <- data.frame(Season = unique(myt$Season))

Predict <- predict(mod, newdata = MyData, se.fit = T)

MyData$fit <- Predict$fit
MyData$SE <- Predict$se.fit
MyData$upr <- Predict$fit + 1.96*MyData$SE
MyData$lwr <- Predict$fit - 1.96*MyData$SE

logit_back <- function(x) exp(x)/(1+exp(x))

MyData$fit_pi <- logit_back(MyData$fit)
MyData$lwr_pi <- logit_back(MyData$lwr)
MyData$upr_pi <- logit_back(MyData$upr)


ggplot(MyData, aes(x = Season, y = fit_pi)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr_pi, ymax = upr_pi), width = 0.2)

appraise(mod)

car::Anova(mod)

draw(mod, residuals = T, parametric = T)

ggplot(myt, aes(DOY, y = Cancer/N)) + geom_point()

