library(vegan)
library(readxl)
library(dplyr)


# Данные по разновидности BTN
myt <- read_excel("Data/summary table_Magadan_itog.xlsx", sheet = "data")


# Характеристики точек
points <- read_excel("Data/Magadan_2021_2023_ecology.xlsx", sheet = "Points  characteristic 2021-23", na = "NA")

load(file =  "Data/gg_Magadan_large.RData")

points <- 
  points %>% 
  mutate(fPort = factor(ifelse(Site %in% c("KHOL", "MAM", "MAR_II", "MCHK", "PORT"), "Close", "Distant")))



# Данные по размерной структуре

size <- read_excel("Data/Magadan_2021_2023_ecology.xlsx", na = "NA", sheet = "Размерная струкутра 2023 2021")

size <- size[complete.cases(size), ]

library(reshape2)

scam <- dcast(Year + Site ~ Size_class, data = size)

area <- read_excel("Data/Magadan_2021_2023_ecology.xlsx", na = "NA", sheet = "Площадь проб на размер")

sample_area <- 
  area %>% group_by(Year, Site) %>% summarise(Total_area = sum(Area))

scam <- 
  scam %>% group_by(Year, Site)


scam [ ,3:ncol(scam)] <- 
  round((scam[ ,3:ncol(scam)] / sample_area$Total_area) *10000, 0)




scam_23 <- scam %>% filter(Year == 2023)


pca_scam_23 <- rda(decostand(scam_23[ , -c(1,2)], method = "hellinger" ))

plot(pca_scam_23, display = "sp")

sum_pca_scam_23 <- summary(pca_scam_23)

pca_scam_23_size_scores <- as.data.frame(scores(pca_scam_23)$species)

# pca_scam_23_size_scores %>% arrange(CA1)

pca_scam_23_scores <- as.data.frame(scores(pca_scam_23)$sites)

pca_scam_23_scores$N_Juv <- scam_23$L3 + scam_23$L8 + scam_23$L13 + scam_23$L18   

pca_scam_23_scores$N_Large =   scam_23$L23 + scam_23$L28 + scam_23$L33 + scam_23$L38 + scam_23$L43 + scam_23$L48 + scam_23$L53 + scam_23$L58

pca_scam_23_scores$N_Total <- scam_23$L3 + scam_23$L8 + scam_23$L13 + scam_23$L18 + scam_23$L23 + scam_23$L28 + scam_23$L33 + scam_23$L38 + scam_23$L43 + scam_23$L48 + scam_23$L53 + scam_23$L58

pca_scores_scam_23 <- data.frame(Year = scam_23$Year, Site = scam_23$Site, pca_scam_23_scores)




# Проективное покрытие мидий

cover_23 <- read_excel("Data/Magadan_2021_2023_ecology.xlsx", sheet = "Покрытия миидий 2023")

mean_cover_23 <-
  cover_23 %>%
  group_by(Site) %>%
  summarise(Cover = mean(`Number of squares`/30))





# 
# Карта с общей характеристикой точек

library(ggplot2)

ggplot(gg_Magadan_large, aes(x = long, y = lat, group = group)) + 
  geom_polygon() + 
  # coord_map(xlim = c(150., 151.52), ylim = c(59.4, 59.8) )+
  geom_point(data = points %>% filter(Year == 2023), aes(x = lon, y = lat, group = 1, size = (fetch), fill = fPort ), shape = 21) +
  scale_fill_manual(values =  c("gray", "yellow"))






# Анализ без объедиения проб сайтов

cancer <- 
  myt %>% 
  mutate(Prop_BTN1 = BTN1/N, Prop_BTN2 = (BTN2.1 + BTN2.2 + BTN2.SAM)/N) 


cancer2 <-
  cancer %>% 
  filter(Year == 2023) %>% 
  select(-Site) %>% 
  rename(Site = Site_code)




cancer_2023 <- 
  merge(points, cancer2) %>% 
  filter(Year == 2023)

cancer_2023 <- 
merge(cancer_2023, pca_scores_scam_23)

cancer_2023 <- 
  merge(cancer_2023, mean_cover_23)



cancer_2023 <- 
  cancer_2023 %>% 
  mutate(BTN2 = BTN2.1 + BTN2.2 + BTN2.SAM)


# library(ggplot2)
qplot(cancer_2023$fetch,  cancer_2023$Prop_BTN1)


cancer_2023 %>% 
  group_by(Site) %>% 
  summarise(fetch = mean(fetch), Prop_BTN1 = sum(BTN1)/sum(N)) %>% 
  ggplot(., aes(fetch, Prop_BTN1)) +
  geom_point()


cancer_2023 %>% 
  group_by(Site) %>% 
  summarise(fetch = mean(fetch), Prop_BTN2 = sum(BTN2)/sum(N)) %>% 
  ggplot(., aes(fetch, Prop_BTN2)) +
  geom_point()


# Модель для Prop_BTN1 и Prop_BTN2 в одной модели #################

library(reshape2)

cancer_2023_long <- 
cancer_2023 %>% 
  select(Site, Salinity, Dist_Port, fetch, fPort, Sample, PC1, PC2, N_Large, N_Juv, N_Total, Cover, N, BTN1, BTN2) %>%
  mutate(Sample = 1:nrow(.)) %>% 
  melt(., id.vars = c("Site", "Salinity", "Dist_Port", "fetch", "fPort", "Sample", "PC1", "PC2", "N_Large", "N_Juv", "N_Total",  "Cover", "N"), variable.name = "Lineage", value.name = "N_cancer") %>% 
  mutate(N_helthy = N - N_cancer)




library(mgcv)
library(gratia)


Mod_btn1_btn2 <- gam(cbind(N_cancer, N_helthy) ~ s((Dist_Port), by = Lineage) + s(fetch, by = Lineage) + s(N_Large, by = Lineage) + Lineage + s(Sample, bs = "re"), data = cancer_2023_long, family = "binomial", method = "REML" )


Mod_btn1_btn2 <- gam(cbind(N_cancer, N_helthy) ~ s((Dist_Port), by = Lineage) + s(fetch, by = Lineage) + Lineage + s(Sample, bs = "re"), data = cancer_2023_long, family = "binomial", method = "REML" )



appraise(Mod_btn1_btn2)

summary(Mod_btn1_btn2)


draw(Mod_btn1_btn2)


############################## Соотвтствие предсказаний наблюдениям




scam_21 <- scam %>% filter(Year == 2021)


scam_21$N_Juv <- scam_21$L3 + scam_21$L8 + scam_21$L13 + scam_21$L18   

scam_21$N_Large =  scam_21$L28 + scam_21$L33 + scam_21$L38 + scam_21$L43 + scam_21$L48 + scam_21$L53 + scam_21$L58

cancer_21 <- 
  cancer %>% 
  filter(Year == 2021) %>% 
  select(-Site) %>% 
  rename(Site = Site_code)


cancer_21 <- 
  merge(cancer_21, scam_21)

cancer_21 <- 
  merge(cancer_21, points) 

cancer_21_summ <-
cancer_21 %>% 
  group_by(Site) %>% 
  summarise(BTN1 = sum(BTN1), BTN2 = sum(BTN2.1) + sum(BTN2.2) + sum(BTN2.SAM), N = sum(N), fetch = mean(fetch), N_Large = mean(N_Large), Dist_Port = mean(Dist_Port)) %>% 
  mutate(Prop_BTN1 = BTN1/N, Prop_BTN2 = BTN2/N)

cancer_21_summ$Lineage  = "BTN1"


################# 

predictions_2021 <-
  cancer_21_summ %>% 
  select(Site, Prop_BTN1, Prop_BTN2)

predictions_2021$Prop_BTN1_prdicted <-  predict(Mod_btn1_btn2, newdata = cancer_21_summ, exclude = "s(Sample)", newdata.guaranteed=TRUE, type = "response")

predictions_2021$Prop_BTN2_prdicted <-  predict(Mod_btn1_btn2, newdata = cancer_21, exclude = "s(Sample)", newdata.guaranteed=TRUE, type = "response")



qplot(predictions_2021$Prop_BTN1_prdicted, predictions_2021$Prop_BTN1) + geom_abline()



cor.test(predictions_2021$Prop_BTN1, predictions_2021$Prop_BTN1_prdicted) 

qplot(x = predictions_2021$Prop_BTN2_prdicted, y = predictions_2021$Prop_BTN2) + geom_abline()


#########################################







library(lme4)
library(glmmADMB)
library(glmmTMB)


# Модель для BTN1

foo_model <- lm(Prop_BTN1_corrected ~ Cover +  fetch + Dist_Port + CA1 + CA2 , data = cancer_2023)

library(car)
vif(foo_model)


mod_glmm_BTN1 <- glmmTMB(Prop_BTN1_corrected ~ Cover + fetch + Dist_Port + CA1 + CA2  + (1|Site), data = cancer_2023, family = beta_family())

summary(mod_glmm_BTN1)

drop1(mod_glmm_BTN1)

mod_glmm_BTN1_1 <- update(mod_glmm_BTN1, . ~ . - CA2) 
drop1(mod_glmm_BTN1_1)


mod_glmm_BTN1_2 <- update(mod_glmm_BTN1_1, . ~ . - fetch) 
drop1(mod_glmm_BTN1_2)

summary(mod_glmm_BTN1_2)



# Модель для BTN2
mod_glmm_BTN2 <- glmmTMB(Prop_BTN2_corrected ~ Cover + fetch + Dist_Port + CA1 + CA2 + (1|Site), data = cancer_2023, family = beta_family())


summary(mod_glmm_BTN2)

drop1(mod_glmm_BTN2)

mod_glmm_BTN2_1 <- update(mod_glmm_BTN2, . ~ . - Cover) 
drop1(mod_glmm_BTN2_1)


mod_glmm_BTN2_2 <- update(mod_glmm_BTN2_1, . ~ . - Dist_Port) 
drop1(mod_glmm_BTN2_2)

mod_glmm_BTN2_3 <- update(mod_glmm_BTN2_2, . ~ . - CA2) 
drop1(mod_glmm_BTN2_3)

mod_glmm_BTN2_4 <- update(mod_glmm_BTN2_3, . ~ . - CA1) 
drop1(mod_glmm_BTN2_4)


summary(mod_glmm_BTN2_4)




###########################


df <- 
cancer_2023 %>% 
  mutate(BTN2 = BTN2.1 + BTN2.2 + BTN2.SAM, Prop_Juv = N_Juv/(N_Juv + N_Large)) %>% 
  dplyr::select(Site, Cover, Dist_Port, fPort, fetch, CA1, CA2, N_Juv, N_Large, Prop_Juv, N, BTN1, BTN2) 

cancer_2023_long <- 
melt(df, id.vars = c("Site", "Cover", "Dist_Port", "fPort", "fetch", "CA1", "CA2", "N_Juv", "N_Large", "Prop_Juv", "N"), variable.name = "Cancer_Type", value.name = "Freq" )


cancer_2023_long$Site <- factor(cancer_2023_long$Site)


cancer_2023_long <-
  cancer_2023_long %>% 
  mutate(Fi = 2 * asin(sqrt(Freq/N)) * 180/pi)

library(mgcv)
Mod_cancer_type <- gam(Fi ~ 
                         s(fetch, k = 8, by = Cancer_Type) + 
                         s(Dist_Port, k = 8, by = Cancer_Type) +
                                                  Cancer_Type +
                         s(Site, bs = "re"), 
                       data = cancer_2023_long)


library(gratia)
appraise(Mod_cancer_type)


summary(Mod_cancer_type)


draw(Mod_cancer_type)





#### Анализ по объединенным пробам для каждого сайта

btn <-
myt %>% 
  group_by(Site_code, Year) %>% 
  dplyr::select(Site_code, N, DN_FC, BTN1, BTN2.1, BTN2.2, BTN2.SAM) %>% 
  mutate(Helth = N - BTN1 - BTN2.1 - BTN2.2 - BTN2.SAM) %>% 
  summarise_all(.funs = sum) %>% 
  rename(Site = Site_code) 



btn_points <- merge(btn, points)

btn_points<-
btn_points %>% filter(Site != "ON")



df_Site <-
btn_points %>% 
  dplyr::select(DN_FC, BTN1, BTN2.1, BTN2.2, Helth)

df_Site <-
df_Site/btn_points$N


mod_cca <- rda(df_Site  ~ fetch + Dist_Port + Salinity + factor(Year) , data = btn_points)

vif.cca(mod_cca)


plot(mod_cca, display = c("cn", "sp"))

anova(mod_cca, by = "margin")
anova(mod_cca)
anova(mod_cca, by = "axis")



btn_points <- 
  btn_points %>% 
  mutate(Prop_BTN1 = BTN1/(BTN1 + BTN2.1 + BTN2.2 + BTN2.SAM))


ggplot(gg_Magadan_large, aes(x = long, y = lat, group = group)) + 
  geom_polygon() + 
  coord_map(xlim = c(150., 151.52), ylim = c(59.4, 59.8) )+
  geom_point(data = btn_points, aes(x = lon, y = lat, group = 1, size = Prop_BTN1, fill = fPort ), shape = 21) +
  scale_fill_manual(values =  c("gray", "yellow"))

ggplot(btn_points, aes(x = Salinity, y = Prop_BTN1)) +
  geom_point()

ggplot(btn_points, aes(x = fetch, y = Prop_BTN1)) +
  geom_point()

ggplot(btn_points, aes(x = Dist_Port, y = Prop_BTN1)) +
  geom_point()


library(betareg)

library(gratia)


btn_points$Prop_BTN1_corrected <- btn_points$Prop_BTN1

btn_points$Prop_BTN1_corrected [btn_points$Prop_BTN1 == 0] <- 0.000001 
btn_points$Prop_BTN1_corrected [btn_points$Prop_BTN1 == 1] <- 0.999999 


Mod_btn <- gam(Prop_BTN1 ~ Salinity + s(fetch, k = 4) + Dist_Port + DN_FC, data = btn_points, family = "gaussian")


appraise(Mod_btn)

summary(Mod_btn)

draw(Mod_btn, residuals = T, parametric = T)







# %>% 
#   dplyr::select(- BTN2.SAM)


dat_rel <- dat[, -c(1:3)]/dat$N 

dat_rel$Site <- dat$Site_code
dat_rel$Year <- dat$Year


ggplot(dat_rel, aes(BTN1, (BTN2.2 + BTN2.1) )) +
  geom_text(aes(label = Site)) + 
  geom_abline() +
  facet_wrap(~Year)






ggplot(dat_rel, aes(BTN1, BTN2.1)) +
  geom_text(aes(label = Site)) + 
  geom_abline() +
  facet_wrap(~Year)

ggplot(dat_rel, aes(BTN2.2, BTN2.1)) +
  geom_text(aes(label = Site)) + 
  geom_abline() +
  facet_wrap(~Year)

ggplot(dat_rel, aes(BTN1, (BTN2.1 + BTN2.2) )) +
  geom_text(aes(label = Site)) + 
  geom_abline() +
  facet_wrap(~Year)


mod_cca <- cca(dat_rel) 

plot(mod_cca, display = "sp")
plot(mod_cca, display = "site")


summary(mod_cca)

scores(mod_cca)

dat_rel$PC1 <- scores(mod_cca)$sites[,1]
dat_rel$PC2 <- scores(mod_cca)$sites[,2]

dat_rel$Site <- dat$Site_code
dat_rel$Year <- dat$Year

library(ggplot2)
ggplot(dat_rel, aes(x = PC2, y = BTN1)) +
  geom_point()

ggplot(dat_rel, aes(x = PC2, y = BTN2.1)) +
  geom_point()

ggplot(dat_rel, aes(x = PC2, y = BTN2.2)) +
  geom_point()



ggplot(dat_rel, aes(x = PC1, y = BTN1)) +
  geom_point()


ggplot(dat_rel, aes(x = PC1, y = BTN2.1)) +
  geom_point()

ggplot(dat_rel, aes(x = PC1, y = BTN2.2)) +
  geom_point()


ggplot(dat_rel, aes(PC1, PC2)) +
  geom_text(aes(label = Site)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)




dat<-
  myt %>% 
  # filter(Year == 2023) %>% 
  dplyr::select(Site_code, N, DN_FC, BTN1, BTN2.1, BTN2.2, BTN2.SAM) %>% 
  mutate(Helth = N - BTN1 - BTN2.1 - BTN2.2 - BTN2.SAM) 


dat_rel <- dat[, -c(1:2)]/dat$N 

mod_cca <- cca(dat_rel) 

plot(mod_cca, display = "sp")
plot(mod_cca, display = "site")


dat_rel$PC1 <- scores(mod_cca)$sites[,1]
dat_rel$PC2 <- scores(mod_cca)$sites[,2]

dat_rel$Site <- dat$Site_code
dat_rel$Year <- myt$Year


ggplot(dat_rel, aes(PC1, PC2)) +
  geom_text(aes(label = Site)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)


ggplot(dat_rel, aes(Site, PC2)) +
  geom_boxplot()+
  geom_hline(yintercept = 0)
  
Pl_BTN2.2 <- 
ggplot(dat_rel, aes(Site, BTN2.2, fill = factor(Year) )) +
  geom_boxplot()+
  geom_hline(yintercept = 0)

Pl_BTN2.1 <- 
ggplot(dat_rel, aes(Site, BTN2.1, fill = factor(Year))) +
  geom_boxplot()+
  geom_hline(yintercept = 0)

Pl_BTN1 <- 
ggplot(dat_rel, aes(Site, BTN1, fill = factor(Year))) +
  geom_boxplot()+
  geom_hline(yintercept = 0)


library(patchwork)

Pl_BTN2.2/Pl_BTN1

ggplot(dat_rel, aes(Site, BTN2.2, fill = factor(Year))) +
  geom_boxplot()+
  geom_hline(yintercept = 0)


ggplot(dat_rel, aes(Site, BTN1, fill = factor(Year))) +
  geom_boxplot()+
  geom_hline(yintercept = 0)
