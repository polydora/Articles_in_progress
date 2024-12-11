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




# scam_23 <- scam %>% filter(Year == 2023)


pca_scam <- rda(decostand(scam[ , -c(1,2)], method = "hellinger" ))

plot(pca_scam, display = "sp")

sum_pca_scam <- summary(pca_scam)

pca_scam_size_scores <- as.data.frame(scores(pca_scam)$species)

# pca_scam_23_size_scores %>% arrange(CA1)

pca_scam_scores <- as.data.frame(scores(pca_scam)$sites)

pca_scam_scores$N_Juv <- scam$L3 + scam$L8 + scam$L13 + scam$L18   

pca_scam_scores$N_Large =   scam$L23 + scam$L28 + scam$L33 + scam$L38 + scam$L43 + scam$L48 + scam$L53 + scam$L58

pca_scam_scores$N_Total <- scam$L3 + scam$L8 + scam$L13 + scam$L18 + scam$L23 + scam$L28 + scam$L33 + scam$L38 + scam$L43 + scam$L48 + scam$L53 + scam$L58

pca_scores_scam <- data.frame(Year = scam$Year, Site = scam$Site, pca_scam_scores)




# Проективное покрытие мидий

cover_23 <- read_excel("Data/Magadan_2021_2023_ecology.xlsx", sheet = "Покрытия миидий 2023")

mean_cover_23 <-
  cover_23 %>%
  group_by(Site) %>%
  summarise(Cover = mean(`Number of squares`/30))

# Ранжируем сайты по покрытию мидий

library(tidyverse)
library(magrittr)

mean_cover_23 %<>% 
  arrange(desc(Cover)) %>% 
  mutate(Cover_class = case_when(
    Cover >= median(Cover) ~ "Dense",
    Cover < median(Cover) ~ "Sparse",
  )) 



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
  mutate(Prop_BTN1 = BTN1/N, Prop_BTN2 = (BTN2.1 + BTN2.2)/N, Prop_BTN2.1 = (BTN2.1)/N,  Prop_BTN2.2 = (BTN2.2)/N ) 


cancer2 <-
  cancer %>% 
  filter(Year == 2023) %>% 
  select(-Site) %>% 
  rename(Site = Site_code)




cancer_2023 <- 
  merge(points, cancer2) %>% 
  filter(Year == 2023)

pca_scores_scam_23 <- 
  pca_scores_scam %>% 
  filter(Year == 2023)

cancer_2023 <- 
merge(cancer_2023, pca_scores_scam_23)

cancer_2023 <- 
  merge(cancer_2023, mean_cover_23)



cancer_2023 <- 
  cancer_2023 %>% 
  mutate(BTN2 = BTN2.1 + BTN2.2,
         BTN2.1 = BTN2.1,
         BTN2.2 = BTN2.2)




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

cancer_2023 %>% 
  group_by(Site) %>% 
  summarise(fetch = mean(fetch), Prop_BTN2.1 = sum(BTN2.1)/sum(N)) %>% 
  ggplot(., aes(fetch, Prop_BTN2.1)) +
  geom_point()

cancer_2023 %>% 
  group_by(Site) %>% 
  summarise(fetch = mean(fetch), Prop_BTN2.2 = sum(BTN2.2)/sum(N)) %>% 
  ggplot(., aes(fetch, Prop_BTN2.2)) +
  geom_point()


# Данные по росту ###############3

growth <- read_excel("Data/Growth_Magadan2023_itog.xlsx")

growth_data <- 
growth %>% 
  group_by(Site, Sample) %>% 
  summarise(OGP_sample = mean(OGP_sample), OGP_site = mean(OGP_site) )
  

cancer_2023 <- merge(cancer_2023, growth_data)




# Модель для Prop_BTN1, Prop_BTN2.1 и Prop_BTN2.2 в одной модели #################

library(reshape2)

cancer_2023_long <- 
cancer_2023 %>% 
  select(Site, Salinity, Dist_Port, fetch, fPort, Sample, PC1, PC2, N_Large, N_Juv, N_Total, Cover, N, BTN1, BTN2, OGP_sample, OGP_site) %>%
  mutate(Sample = 1:nrow(.)) %>% 
  melt(., id.vars = c("Site", "Salinity", "Dist_Port", "fetch", "fPort", "Sample", "PC1", "PC2", "N_Large", "N_Juv", "N_Total",  "Cover", "N", "OGP_sample", "OGP_site"), variable.name = "Lineage", value.name = "N_cancer") %>% 
  mutate(N_helthy = N - N_cancer)


df <- 
cancer_2023_long %>% 
  filter(Lineage == "BTN1")

mod_foo <- lm(BTN1 ~ Dist_Port + fetch + PC1 + N_Large  + OGP_sample, data = cancer_2023)

library(car)
vif(mod_foo)



library(mgcv)
library(gratia)



Mod_btn1_btn2 <- gam(cbind(N_cancer, N_helthy) ~ s((Dist_Port), by = Lineage, k = 9, bs = "tp") + s((fetch), by = Lineage, k = 9, bs = "tp")  + s((PC1), by = Lineage, k = 9, bs = "tp") + s((N_Large), by = Lineage, k = 9, bs = "tp") +  Lineage  + s(Sample, bs = "re"), data = cancer_2023_long, family = "binomial", method = "REML" )



appraise(Mod_btn1_btn2)

summary(Mod_btn1_btn2)

# DHARMa

draw(Mod_btn1_btn2, residuals = F)


cancer_2023_long %>% 
  filter(Lineage == "BTN1") %>% 
  ggplot(aes(x = PC1, y = N_cancer/N_helthy )) +
  geom_point()

cancer_2023_long %>% 
  filter(Lineage == "BTN1") %>% 
  ggplot(aes(x = fetch, y = N_cancer/N_helthy )) +
  geom_point()


cancer_2023_long %>% 
  filter(Lineage == "BTN1") %>% 
  ggplot(aes(x = Dist_Port, y = N_cancer/N_helthy )) +
  geom_point()


cancer_2023_long %>% 
  filter(Lineage == "BTN1") %>% 
  ggplot(aes(x = OGP_site , y = N_cancer/N_helthy )) +
  geom_point()

##############################

library(glmmTMB)


Mod_btn1_btn2 <- glmmTMB(cbind(N_cancer, N_helthy) ~ scale(Dist_Port) * Lineage + scale(fetch) *Lineage +  scale(PC1) * Lineage + scale(N_Large)*Lineage + scale(OGP_sample) * Lineage + (1|Sample), data = cancer_2023_long, family = "binomial")

summary(Mod_btn1_btn2)

drop1(Mod_btn1_btn2)


Mod_btn1_btn2_2 <- update(Mod_btn1_btn2, . ~ . - Lineage:scale(fetch))
drop1(Mod_btn1_btn2_2)

Mod_btn1_btn2_3 <- update(Mod_btn1_btn2_2, . ~ . -Lineage:scale(OGP_sample))
drop1(Mod_btn1_btn2_3)


Mod_btn1_btn2_4 <- update(Mod_btn1_btn2_3, . ~ . -Lineage:scale(PC1))
drop1(Mod_btn1_btn2_4, test = "Chi")


Mod_btn1_btn2_5 <- update(Mod_btn1_btn2_4, . ~ . -scale(Dist_Port):Lineage)
drop1(Mod_btn1_btn2_5, test = "Chi")

Mod_btn1_btn2_6 <- update(Mod_btn1_btn2_5, . ~ . - Lineage:scale(N_Large))
drop1(Mod_btn1_btn2_6, test = "Chi")

Mod_btn1_btn2_7 <- update(Mod_btn1_btn2_6, . ~ . - scale(N_Large))
drop1(Mod_btn1_btn2_7, test = "Chi")


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
cancer_2023_site <- 
cancer_2023 %>% 
  group_by(Site) %>% 
  summarise(Year = mean(Year),
            lon = mean(lon),
            lat = mean(lat),
            Salinity = mean(Salinity),
            Dist_Port = mean(Dist_Port),  
            fetch = mean(fetch),
            fPor = unique(fPort),
            N = sum(N), 
            PC1 = mean(PC1),
            PC2 = mean(PC2),
            N_Juv = mean(N_Juv),
            N_Large = mean(N_Large), 
            N_Total = mean(N_Total),
            Cover = mean(Cover),
            OGP_site = mean(OGP_site),
            BTN1 = sum(BTN1),
            BTN2 = sum(BTN2.1 + BTN2.2 + BTN2.SAM))



cancer_2023_site_long <- 
  cancer_2023_site %>% 
  select(Site, Salinity, Dist_Port, fetch, PC1, PC2, N_Large, N_Juv, N_Total, Cover, N, BTN1, BTN2, OGP_site) %>%
  melt(., id.vars = c("Site", "Salinity", "Dist_Port", "fetch", "PC1", "PC2", "N_Large", "N_Juv", "N_Total",  "Cover", "N", "OGP_site"), variable.name = "Lineage", value.name = "N_cancer") %>% 
  mutate(N_helthy = N - N_cancer, Prop_BTN = N_cancer/N, Fi_BTN = 2*asin(sqrt(Prop_BTN))*180/pi)



Mod_site <- gam(N_cancer ~ s(scale(Dist_Port), by = Lineage) + s(scale(fetch), by = Lineage)  + s(scale(OGP_site), by = Lineage) + s(N, by = Lineage, k = 5) + Lineage , data = cancer_2023_site_long, family = "nb", method = "REML" )


appraise(Mod_site)

summary(Mod_site)

