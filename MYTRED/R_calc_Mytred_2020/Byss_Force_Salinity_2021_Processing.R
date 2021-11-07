library(ggplot2)
library(dplyr)
library(readxl)
library(nlme)
library(reshape2)


myt <- read_excel("Data/Mytred_Byss_Salinity_2021.xlsx", na = "NA", col_names = T)
str(myt)

nrow(myt)




myt2 <- myt %>% filter(!is.na(Force)) 
nrow(myt2)




myt2 %>%  
ggplot(., aes(x = Salinity, y = Force, color = Morphotype)) + geom_point()  + geom_smooth(method = "lm")

Mod <- gls(Force  ~ Salinity *Morphotype  + Wtotal, data = myt2)


Mod2 <- gls(Force  ~ Salinity *Morphotype  + Wtotal, data = myt2, weights = varExp(form = ~Wtotal))

AIC(Mod, Mod2)

plot(Mod2)

library(car)
qqPlot(Mod)

# plot(Mod, which = 1)

summary(Mod2)

Mod3 <- gls(Force  ~ Salinity * Morphotype  + Wtotal, data = myt2, weights = varExp(form = ~Wtotal|Morphotype))

AIC(Mod, Mod2, Mod3)

# Изменяем фиксированную часть модели

Mod3_ML <- update(Mod3, method = "ML") 
drop1(Mod3_ML, test = "Chi")  

#Не будем ничего выкидывать  
  
  

summary(Mod3)

plot(Mod4, which = 1)


MyData <- expand.grid(Salinity = seq(min(myt2$Salinity), max(myt2$Salinity),  0.1), Morphotype = c("e", "t"), Wtotal = mean(myt2$Wtotal))


MyData$Fit <- predict(Mod4, newdata = MyData)


X <- model.matrix(~   Salinity * Morphotype  + Wtotal, data = MyData)
b <- coef(Mod3)

MyData$fit_eta <- X %*% b 
MyData$se_eta <- sqrt(diag(X %*% vcov(Mod3) %*% t(X)))

MyData$fit <- (MyData$fit_eta)
MyData$lwr <- MyData$fit_eta - 1.96 * MyData$se_eta
MyData$upr <- MyData$fit_eta + 1.96 * MyData$se_eta

ggplot(MyData, aes(x = Salinity, y = fit)) + 
  geom_line(aes(color = Morphotype), size = 1) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr, group = Morphotype), alpha = 0.2) +
  geom_point(data = myt2, aes(x = Salinity, y = Force, fill = Morphotype, size = Wtotal), position = position_jitter(width = 0.2), shape = 21, color = "black") +
  scale_color_manual(values = c("blue", "red")) +
  scale_fill_manual(values = c("blue", "red"))

# Another visualiation

ggplot(myt2, aes(x = factor(Salinity), y = Force, fill = Morphotype)) + 
  geom_boxplot()+
  scale_fill_manual(values = c("blue", "red"))




# Важно! Сила прикрепления биссуса в условиях опреснения снижается у Me, но у Mt она не зависит от солености.


# проверка валидности
Mod3_dummy <- gls(Force  ~ Salinity + Morphotype  + Wtotal, data = myt2)
vif(Mod3_dummy)


plot(Mod3)
