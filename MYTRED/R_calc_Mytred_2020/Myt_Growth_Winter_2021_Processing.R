library(ggplot2)
library(dplyr)
library(readxl)
library(nlme)


myt <- read_excel("Data/Mytred_Growth_Winter_2021.xlsx", na = "NA", col_names = T)
str(myt)

nrow(myt)

marked_myt <- myt %>% filter(Status %in% c("Alive", "Dead"))  %>% group_by(Cage_ID, Morphotype, Status) %>% summarise(N = n()) %>% dcast(Cage_ID ~ Morphotype + Status) 

marked_myt[is.na(marked_myt)] <- 0
marked_myt <- marked_myt[order(marked_myt$Cage_ID), ]



fone <- read_excel("Data/Mytred_Growth_Winter_2021.xlsx", na = "NA", col_names = T, sheet = "Fone_mussel")

fone <- fone[order(fone$Cage_ID), ]



fone[1:20,3:6] <- fone[1:20,3:6] + marked_myt[1:20,2:5]



fone2 <- fone %>% mutate(PropT = T_alive/(E_alive+T_alive), N_total = E_alive+T_alive)



myt2 <- myt %>% filter( (Sample_site == "2R" & Morphotype == "e") | (Sample_site == "Kanal" & Morphotype == "t")) %>%  filter(Status == "Alive")

nrow(myt2)


myt3 <- merge(myt2, fone2, by = "Cage_ID" )

myt4 <- myt3 %>% mutate(DL = Fin_size - Init_size) %>% filter(!is.na(DL)) 


myt4$DL[myt4$DL < 0] <- 0

myt5 <- myt4 %>% filter(!(Cage_ID %in% c("K1", "K2", "K3", "K4")))




Mod <- lm(DL  ~ PropT *Morphotype * N_total + Init_size  , data = myt5)

Mod_gls <- gls(DL  ~ PropT *Morphotype * N_total + Init_size, data = myt5)

Mod_gls2 <- gls(DL  ~ PropT *Morphotype * N_total + Init_size, data = myt5, weights = varFixed( ~ Init_size))

Mod_gls3 <- gls(DL  ~ PropT *Morphotype * N_total + Init_size, data = myt5, weights = varExp(form =  ~ N_total))

Mod_gls4 <- gls(DL  ~ PropT *Morphotype * N_total + Init_size, data = myt5, weights = varExp(form =  ~ N_total|Morphotype))

AIC(Mod_gls, Mod_gls2, Mod_gls3, Mod_gls4)

plot(Mod_gls4)

Mod_gls4_ML <- update(Mod_gls4, method = "ML")

drop1(Mod_gls4_ML)



plot(Mod_gls4)

summary(Mod_gls4)

library(car)
qqPlot(Mod_gls4)

# plot(Mod, which = 1)


MyData <- expand.grid(PropT = seq(min(myt5$PropT), max(myt5$PropT), length.out = 10), Morphotype = c("e", "t"),  N_total = mean(myt5$N_total), Init_size = mean(myt5$Init_size) )


MyData$Fit <- predict(Mod_gls4, newdata = MyData)


X <- model.matrix(~   PropT * Morphotype * N_total + Init_size, data = MyData)
b <- coef(Mod_gls4)

MyData$fit_eta <- X %*% b 
MyData$se_eta <- sqrt(diag(X %*% vcov(Mod_gls5) %*% t(X)))

MyData$fit <- (MyData$fit_eta)
MyData$lwr <- (MyData$fit_eta - 1.96 * MyData$se_eta)
MyData$upr <- (MyData$fit_eta + 1.96 * MyData$se_eta)


DL_K <- myt4 %>% filter(Type == "K") %>% group_by(Morphotype) %>% summarise(Mean_DL = mean(DL), SD = sd(DL), SE = SD/sqrt(n())) 


ggplot(MyData, aes(x = PropT, y = fit)) + 
  geom_line(aes(color = Morphotype), size = 1) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr, group = Morphotype), alpha = 0.2) +
  geom_point(data = myt5, aes(x = PropT, y = (DL), color = Morphotype)) +
  geom_hline(data = DL_K, aes(yintercept = (Mean_DL), color = Morphotype), size = 1, linetype = 2)+
  scale_color_manual(values = c("blue", "red"))
  





### Analysis of CI #############

Mod_CI <- lm(log(W_tis) ~ PropT *Morphotype * N_total + Fin_size, data = myt5)

# plot(Mod_CI, which = 1)

summary(Mod_CI)

drop1(Mod_CI, test = "F")

Mod_CI2 <- update(Mod_CI, . ~.-PropT:Morphotype:N_total)
drop1(Mod_CI2, test= "F")

Mod_CI3 <- update(Mod_CI2, . ~.-PropT:N_total)
drop1(Mod_CI3, test = "F")


Mod_CI4 <- update(Mod_CI3, . ~.-Morphotype:N_total)  
drop1(Mod_CI4, test = "F")

Mod_CI5 <- update(Mod_CI4, . ~.-PropT:Morphotype)
drop1(Mod_CI5, test = "F")


plot(Mod_CI5, which = 1)
qqPlot(Mod_CI5)


summary(Mod_CI5)



MyData <- expand.grid(PropT = seq(min(myt5$PropT), max(myt5$PropT), length.out = 10), Morphotype = c("e", "t"),  N_total = mean(myt5$N_total), Fin_size = mean(myt5$Fin_size) )


MyData$Fit <- predict(Mod_CI, newdata = MyData)


X <- model.matrix(~   PropT * Morphotype * N_total + Fin_size, data = MyData)
b <- coef(Mod_CI)

MyData$fit_eta <- X %*% b 
MyData$se_eta <- sqrt(diag(X %*% vcov(Mod_CI) %*% t(X)))

MyData$fit <- exp(MyData$fit_eta)
MyData$lwr <- exp(MyData$fit_eta - 1.96 * MyData$se_eta)
MyData$upr <- exp(MyData$fit_eta + 1.96 * MyData$se_eta)

ggplot(MyData, aes(x = PropT, y = fit)) + 
  geom_line(aes(color = Morphotype), size = 1) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr, group = Morphotype), alpha = 0.2) +
  geom_point(data = myt5, aes(x = PropT, y = (W_tis), color = Morphotype), position = position_jitter(width = 0.01, height = 0.01)) +
  scale_color_manual(values = c("blue", "red"))






### Analysis of Force #############

myt6 <- myt5 %>% filter(!is.na(Force))


Mod_Foce <- lm(log(Force + 1) ~ PropT *Morphotype * N_total + Weight, data = myt6)

# plot(Mod_Foce, which = 1)

summary(Mod_Foce)

drop1(Mod_Foce, test = "F")

Mod_Foce2 <- update(Mod_Foce, . ~.-PropT:Morphotype:N_total)
drop1(Mod_Foce2, test= "F")

Mod_Foce3 <- update(Mod_Foce2, . ~.-PropT:N_total)
drop1(Mod_Foce3, test = "F")


Mod_Foce4 <- update(Mod_Foce3, . ~.-PropT:Morphotype)  
drop1(Mod_Foce4, test = "F")

Mod_Foce5 <- update(Mod_Foce4, . ~.-Morphotype:N_total)
drop1(Mod_Foce5, test = "F")


plot(Mod_Foce5, which = 1)
qqPlot(Mod_Foce5)


summary(Mod_Foce5)



MyData <- expand.grid(PropT = seq(min(myt5$PropT), max(myt5$PropT), length.out = 10), Morphotype = c("e", "t"),  N_total = mean(myt5$N_total), Weight = mean(myt6$Weight) )


MyData$Fit <- predict(Mod_Foce, newdata = MyData)


X <- model.matrix(~   PropT * Morphotype * N_total + Weight, data = MyData)
b <- coef(Mod_Foce)

MyData$fit_eta <- X %*% b 
MyData$se_eta <- sqrt(diag(X %*% vcov(Mod_Foce) %*% t(X)))

MyData$fit <- exp(MyData$fit_eta) -1
MyData$lwr <- exp(MyData$fit_eta - 1.96 * MyData$se_eta) -1
MyData$upr <- exp(MyData$fit_eta + 1.96 * MyData$se_eta) - 1

ggplot(MyData, aes(x = PropT, y = fit)) + 
  geom_line(aes(color = Morphotype), size = 1) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr, group = Morphotype), alpha = 0.2) +
  geom_point(data = myt5, aes(x = PropT, y = (Force), color = Morphotype), position = position_jitter(width = 0.01, height = 0.01)) +
  scale_color_manual(values = c("blue", "red"))





