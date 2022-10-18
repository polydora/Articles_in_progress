library(readxl)
library(dplyr)
library(betareg)
library(car)


myt <- read_excel("Data/Magadan_data.xlsx")

myt$Fetch <- as.numeric(myt$Fetch)

myt$Dist2 <- as.numeric(myt$Dist2)

myt$Lon <- as.numeric(myt$Lon)
myt$Lat <- as.numeric(myt$Lat)





myt <- myt %>% mutate(Prop_ill = N_ill/Sample_size)


myt$Prop_ill[myt$Prop_ill == 0] <-0.0001 


# myt$Current <- extract(current_velocity, my.sites[, 2:1])

Mod <- betareg(Prop_ill ~   N + S   + Fetch , data = myt )


vif(Mod)

plot(myt$Dist, myt$Fetch)


summary(Mod)



ggplot(myt, aes(x = Fetch ,  y = Prop_ill)) + geom_point()






myt_individ <- read_excel("Data/mussel_individual.xlsx")

myt_individ$BTN_presence <- as.numeric(myt_individ$BTN_presence)


myt_individ <- merge(myt_individ, myt, by = "Site", all.x = T )


library(lme4)

Mod_ind <- glm(BTN_presence ~  scale(L) + scale(DL)  + scale(S) + scale(Fetch) + Sex_ident, data = myt_individ, family = "binomial")

vif(Mod_ind)


summary(Mod_ind)





Mod_ind <- lm( DL ~  L + S + Fetch + Sex_ident + factor(BTN_presence) , data = myt_individ)

vif(Mod_ind)


summary(Mod_ind)



library(mgcv)

mod2 <- gam(Prop_ill ~ Fetch, data = myt, family = "betar")

MyData <- data.frame(Fetch = seq(0, max(myt$Fetch), length.out = 100))

predicted <- predict(mod2, newdata = MyData, se.fit = TRUE, type = "response")

MyData$Fitted <- predicted$fit

MyData$SE <- predicted$se.fit


ggplot(MyData, aes(x = Fetch, y = Fitted)) + 
  geom_line(color = "blue", size = 2) + 
  geom_ribbon(aes(ymin = Fitted - 1.96*SE, ymax = Fitted + 1.96*SE), alpha = 0.2) + 
  geom_point(data = myt, aes(y = Prop_ill), size = 2) + 
  theme_bw()

