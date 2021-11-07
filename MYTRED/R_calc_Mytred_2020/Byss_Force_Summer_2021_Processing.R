library(ggplot2)
library(dplyr)
library(readxl)
library(nlme)
library(reshape2)


myt <- read_excel("Data/Mytred_2021_byssus_force.xlsx", na = "NA", col_names = T)
str(myt)

nrow(myt)

marked_myt <- myt %>% group_by(Cage_ID, Morphotype) %>% summarise(N = n()) %>% dcast(Cage_ID ~ Morphotype )

marked_myt[is.na(marked_myt)] <- 0

fone <- read_excel("Data/Mytred_2021_byssus_force.xlsx", na = "NA", col_names = T, sheet = "Fone_mussel")

fone[1:6,c(2, 4)] <- fone[1:6,c(2, 4)] + marked_myt[1:6,2:3]



fone2 <- fone %>% mutate(PropT = T_alive/(E_alive+T_alive), N_total = E_alive+T_alive)



myt2 <- myt %>% filter( (Label == "r" & Morphotype == "e") | (Label == "l" & Morphotype == "t")) 
nrow(myt2)


myt3 <- merge(myt2, fone2, by = "Cage_ID" ) 


myt5 <- myt3 %>% filter(!(Cage_ID %in% c("Z1", "Z2", "Z3"))) %>% filter(complete.cases(.)) 




myt5 %>%  
ggplot(., aes(x = PropT, y = Force, color = Morphotype)) + geom_point()  + geom_smooth(method = "lm")

myt5 %>%  
  ggplot(., aes(x = PropT, y = N_byss, color = Morphotype)) + geom_point()  + geom_smooth(method = "lm")


myt5 %>%  
  ggplot(., aes(x = N_total, y = Force , color = Morphotype)) + geom_point()  + geom_smooth(method = "lm")

myt5 %>%  
  ggplot(., aes(x = N_total, y = N_byss, color = Morphotype)) + geom_point()  + geom_smooth(method = "lm")



Mod <- lm(Force  ~ PropT *Morphotype * N_total + Weight  , data = myt5)

library(car)
qqPlot(Mod)

# plot(Mod, which = 1)

summary(Mod)

drop1(Mod, test = "F")

Mod2 <- update(Mod, . ~.-PropT:Morphotype:N_total)
drop1(Mod2, test = "F")

Mod3 <- update(Mod2, . ~.-Morphotype:N_total  )
drop1(Mod3, test = "F")


Mod4 <- update(Mod3, . ~.-PropT:N_total)
drop1(Mod4, test = "F")

Mod5 <- update(Mod4, . ~.- PropT:Morphotype )
drop1(Mod5, test = "F")

Mod6 <- update(Mod5, . ~.- PropT )
drop1(Mod6, test = "F")


Mod7 <- update(Mod6, . ~.- N_total )
drop1(Mod7, test = "F")


plot(Mod7, which = 1)
qqPlot(Mod7)

summary(Mod7)

AIC(Mod, Mod7)






### Analysis of N_bys #############

library(MASS)

Mod_CI <- glm.nb(N_byss ~ PropT *Morphotype * N_total + Size, data = myt5)

# plot(Mod_CI, which = 1)

summary(Mod_CI)

drop1(Mod_CI, test = "Chi")

Mod_CI2 <- update(Mod_CI, . ~.-PropT:Morphotype:N_total)
drop1(Mod_CI2, test= "Chi")

Mod_CI3 <- update(Mod_CI2, . ~.-Morphotype:N_total)
drop1(Mod_CI3, test = "Chi")


Mod_CI4 <- update(Mod_CI3, . ~.-PropT:Morphotype)
drop1(Mod_CI4, test = "Chi")

Mod_CI5 <- update(Mod_CI4, . ~.-PropT:N_total)
drop1(Mod_CI5, test = "Chi")

Mod_CI6 <- update(Mod_CI5, . ~.-N_total)
drop1(Mod_CI6, test = "Chi")

Mod_CI7 <- update(Mod_CI6, . ~.-PropT)
drop1(Mod_CI7, test = "Chi")



plot(Mod_CI7, which = 1)

summary(Mod_CI7)



MyData <- expand.grid(PropT = seq(min(myt4$PropT), max(myt4$PropT),  0.1), Morphotype = c("e", "t"), N_total = mean(myt5$N_total), Fin_size = mean(myt5$Fin_size))


MyData$Fit <- predict(Mod_CI4, newdata = MyData)


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



