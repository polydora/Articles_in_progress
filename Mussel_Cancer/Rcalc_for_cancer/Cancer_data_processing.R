library(readxl)
library(dplyr)
library(betareg)
library(car)
library(ggplot2)

#################### Групповой анализ #################

myt <- read_excel("Data/Magadan_data.xlsx")

myt$Fetch <- as.numeric(myt$Fetch)

myt$Dist2 <- as.numeric(myt$Dist2)

myt$Lon <- as.numeric(myt$Lon)
myt$Lat <- as.numeric(myt$Lat)

myt <- myt %>% mutate(Prop_ill = N_ill/Sample_size)


siz <- read_excel("Data/Ecological data on mussels.xlsx", na = "NA", sheet = "Magadan_Mussels")
siz$L <- as.numeric(siz$L)


hist(siz$L)

demogr <- siz %>% group_by(Site) %>% summarise(Prop_adult = mean(L>15, na.rm = T), N_dots_med = median(N_dots, na.rm = T), N_dots_mean = mean(N_dots, na.rm = T), L_mean = median(L, na.rm = T))


  plot(demogr$N_dots_mean, demogr$N_dots_med)



myt2 <- merge(myt, demogr)

myt2$Prop_ill[myt$Prop_ill == 0] <-0.00001 


# myt$Current <- extract(current_velocity, my.sites[, 2:1])


Mod <- betareg(Prop_ill ~   scale(N) +  scale(N_dots_mean)  + scale(S) + scale(Fetch), data = myt2 )



vif(Mod)

plot(Mod, which = 2)


qplot(x = fitted(Mod), y = residuals(Mod, type = "pearson")) + geom_smooth() + geom_hline(yintercept = 0)


summary(Mod) #Параметры модели



ggplot(myt2, aes(x = Fetch ,  y = Prop_ill)) + geom_point()

ggplot(myt2, aes(y = N_dots_mean ,  x = Prop_ill)) + geom_point()

siz$Site <- factor(siz$Site, levels = myt2 %>% arrange(Prop_ill) %>% pull(Site))
  
ggplot(siz, aes(x = Site, y = N_dots)) + geom_boxplot()


# Визуализация модели
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








########################### Индивидуальный анализ мидий ###################

myt_individ <- read_excel("Data/mussel_individual.xlsx")

myt_individ$BTN_presence <- as.numeric(myt_individ$BTN_presence)


myt_individ <- merge(myt_individ, myt, by = "Site", all.x = T )


library(lme4)

myt_individ$Sex <- factor(myt_individ$Sex)

Mod_ind_glmer <- glmer(BTN_presence ~  scale(L) + scale(DL) + scale(S) + scale(Fetch) + Sex + (1|Site), data = myt_individ, family = binomial(link = "logit"))


Mod_ind_glm <- glm(BTN_presence ~  scale(L) + scale(DL) + scale(S) + scale(Fetch) + Sex +N_dots      , data = myt_individ, family = binomial(link = "logit"))

AIC(Mod_ind_glmer, Mod_ind_glm)


vif(Mod_ind)


summary(Mod_ind_glm)




