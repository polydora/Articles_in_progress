library(ggplot2)
library(dplyr)
library(readxl)
library(nlme)
library(reshape2)


myt <- read_excel("Data/Mytred_Growth_Summer_2021.xlsx", na = "NA", col_names = T)
str(myt)

nrow(myt)

marked_myt <- myt %>% filter(Status %in% c("Alive", "Dead"))  %>% group_by(Cage_ID, Morphotype, Status) %>% summarise(N = n()) %>% dcast(Cage_ID ~ Morphotype + Status)

marked_myt[is.na(marked_myt)] <- 0

fone <- read_excel("Data/Mytred_Growth_Summer_2021.xlsx", na = "NA", col_names = T, sheet = "Fone_mussel")

fone[1:12,3:6] <- fone[1:12,3:6] + marked_myt[1:12,2:5]



fone2 <- fone %>% mutate(PropT = T_alive/(E_alive+T_alive), N_total = E_alive+T_alive)



myt2 <- myt %>% filter( (Sample_site == "Ed" & Morphotype == "e") | (Sample_site == "Tr" & Morphotype == "t")) %>% filter(is.na(Comment)) %>% filter(Status == "Alive") %>% filter(Mussel_ID != 898)

nrow(myt2)


myt3 <- merge(myt2, fone2, by = "Cage_ID" )

myt4 <- myt3 %>% mutate(DL = Fin_size - Init_size) %>% filter(!is.na(DL)) 

myt4$DL[myt4$DL < 0] <- 0

myt5 <- myt4 %>% filter(!(Cage_ID %in% c("K1", "K2", "K3", "K4")))



myt5 %>%  
ggplot(., aes(x = PropT, y = DL/Init_size , color = Morphotype)) + geom_point()  + geom_smooth(method = "lm")



myt5 %>%  
  ggplot(., aes(x = N_total, y = DL/Init_size , color = Morphotype)) + geom_point()  + geom_smooth(method = "lm")



Mod <- lm(DL  ~ PropT *Morphotype * N_total + Init_size, data = myt5)

library(car)
qqPlot(Mod)

# plot(Mod, which = 1)

summary(Mod)
Anova(Mod)

drop1(Mod, test = "F")

Mod2 <- update(Mod, . ~.-PropT:Morphotype:N_total)
drop1(Mod2, test = "F")

Mod3 <- update(Mod2, . ~.-PropT:N_total)
drop1(Mod3, test = "F")


Mod4 <- update(Mod3, . ~.-PropT:Morphotype)
drop1(Mod4, test = "F")

Mod5 <- update(Mod4, . ~.- Morphotype:N_total   )
drop1(Mod5, test = "F")

Mod6 <- update(Mod5, . ~.- N_total )
drop1(Mod6, test = "F")


Mod7 <- update(Mod6, . ~.- Init_size )
drop1(Mod7, test = "F")


plot(Mod7, which = 1)
qqPlot(Mod7)

summary(Mod7)

AIC(Mod, Mod7)




MyData <- expand.grid(PropT = seq(min(myt5$PropT), max(myt5$PropT),  0.1), Morphotype = c("e", "t"), Init_size = mean(myt5$Init_size), N_total = mean(myt5$N_total))


MyData$Fit <- predict(Mod, newdata = MyData)


X <- model.matrix(~   Init_size + PropT *Morphotype * N_total , data = MyData)
b <- coef(Mod)

MyData$fit_eta <- X %*% b 
MyData$se_eta <- sqrt(diag(X %*% vcov(Mod) %*% t(X)))

MyData$fit <- (MyData$fit_eta)
MyData$lwr <- MyData$fit_eta - 1.96 * MyData$se_eta
MyData$upr <- MyData$fit_eta + 1.96 * MyData$se_eta


DL_K <- myt4 %>% filter(Type == "K") %>% group_by(Morphotype) %>% summarise(Mean_DL = mean(DL), SD = sd(DL), SE = SD/sqrt(n())) 


ggplot(MyData, aes(x = PropT, y = fit_eta)) + 
  geom_line(aes(color = Morphotype), size = 1) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr, group = Morphotype), alpha = 0.2) +
  geom_point(data = myt5, aes(x = PropT, y = (DL), color = Morphotype), position = position_jitter(width = 0.01)) +
  geom_hline(data = DL_K, aes(yintercept = Mean_DL, color = Morphotype), size = 1, linetype = 2)+
  scale_color_manual(values = c("blue", "red"))
  





### Analysis of CI #############

Mod_CI <- lm((W_tis) ~ PropT *Morphotype * N_total + Fin_size, data = myt5)

# plot(Mod_CI, which = 1)

summary(Mod_CI)

drop1(Mod_CI, test = "F")

Mod_CI2 <- update(Mod_CI, . ~.-PropT:Morphotype:N_total)
drop1(Mod_CI2, test= "F")

Mod_CI3 <- update(Mod_CI2, . ~.-Morphotype:N_total)
drop1(Mod_CI3, test = "F")


Mod_CI4 <- update(Mod_CI3, . ~.-PropT:Morphotype)
drop1(Mod_CI4, test = "F")

Mod_CI5 <- update(Mod_CI4, . ~.-PropT:N_total)
drop1(Mod_CI5, test = "F")


plot(Mod_CI5, which = 1)

summary(Mod_CI5)



MyData <- expand.grid(PropT = seq(min(myt5$PropT), max(myt5$PropT),  0.1), Morphotype = c("e", "t"), N_total = mean(myt5$N_total), Fin_size = mean(myt5$Fin_size))


MyData$Fit <- predict(Mod_CI5, newdata = MyData)


X <- model.matrix(~   PropT + Morphotype + N_total + Fin_size, data = MyData)
b <- coef(Mod_CI5)

MyData$fit_eta <- X %*% b 
MyData$se_eta <- sqrt(diag(X %*% vcov(Mod_CI5) %*% t(X)))

MyData$fit <- (MyData$fit_eta)
MyData$lwr <- MyData$fit_eta - 1.96 * MyData$se_eta
MyData$upr <- MyData$fit_eta + 1.96 * MyData$se_eta

ggplot(MyData, aes(x = PropT, y = fit_eta)) + 
  geom_line(aes(color = Morphotype), size = 1) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr, group = Morphotype), alpha = 0.2) +
  geom_point(data = myt5, aes(x = PropT, y = (W_tis), color = Morphotype), position = position_jitter(width = 0.01, height = 0.01)) +
  scale_color_manual(values = c("blue", "red"))



