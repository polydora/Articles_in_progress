# В этом коде анализирется распределение BTN в Кольском заливе и связь частот заболевания с предикторами


library(dplyr)
library(broom)
library(readxl)
library(ggmap)
library(maps)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(reshape2)
library(scatterpie)

library(sp)
library(mapproj)


### Строим карту встречаемости BTN в Кольком заливе


Murmansk_region <- st_read("Data/Maps/Murmanskaya_obl/boundary-polygon-land-lvl4.shp", quiet =  TRUE)

Pl_kola <- 
ggplot() +
  geom_sf(data = Murmansk_region, fill = "gray50") +
  coord_sf(
    xlim = c(32.96, 33.62),  
    ylim = c(68.9, 69.3)) +
  theme_bw()+
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())
  

kola_cancer <- read_excel("Data/Кольский рак было-стало.xlsx")

kola_cancer <-
  kola_cancer %>% 
  arrange((BTN))

Pl_kola + 
  geom_point(data = kola_cancer, aes( x = Longitude, y = Latitude, fill = BTN), shape = 21, size = 4, position = position_jitter(width = 0.02, height = 0.001)) +
  facet_wrap(~Period, ncol = 2) +
  scale_fill_manual(values = c("yellow", "red")) +
  labs(x = "", y = "")



#### Анализируем связь с предикторами #####

myt <- read_excel("Data/BTN_ecology_KolaBay.xlsx")


myt %>% 
  ggplot(aes(x = Length)) + 
  geom_histogram(binwidth = 5) + 
  facet_wrap(~Site_code, ncol = 3, scales = "free_y")



# str(myt)

# myt <- read.table("Data/BTN_ecology_KolaBay.csv", header = TRUE, sep = ";", dec = ",")

# Вычисляем PC для описания размерной структуры

library(reshape2)


size <- 
  myt %>% 
  dplyr::select(Site_code, Size_class)


scam <- dcast(Site_code ~ Size_class, data = size)


# scam_23 <- scam %>% filter(Year == 2023)

library(vegan)
pca_scam <- rda(decostand(scam[ , -c(1)], method = "hellinger" ))

plot(pca_scam, display = "species", type = "t")




sum_pca_scam <- summary(pca_scam)

pca_scam_size_scores <- as.data.frame(scores(pca_scam)$species)

pca_scam_scores <- as.data.frame(scores(pca_scam)$sites)


pca_scam_scores$Site_code <- scam$Site_code

myt %>% 
  merge(., pca_scam_scores) %>% 
  arrange(PC1) %>% 
  ggplot(aes(x = Length)) + 
  geom_histogram(binwidth = 5) + 
  facet_wrap(~Site_code, ncol = 3, scales = "free_y", dir = "v")


# Получаем общее описание предикторов для сайтов


points <- read_excel("Data/sites_info_KolaBay.xlsx")



points2 <-
merge(points, pca_scam_scores)


myt %>% 
  group_by(Site_code) %>% 
  summarise(N_sq_m = mean(N_m2), B_sq_m = mean(B_m2), PT = mean(PT), Prevalence_BTN = mean(Prevalence_BTN) ) %>% 
  merge(points2, .) ->
  kola_btn

# 
# library(writexl)
# write_xlsx(x = kola_btn, path = "Data/sites_info_KolaBay_with_predictors.xlsx")

# Читаем файл со всеми предикторами
kola_btn_predictors <- read_excel("Data/sites_info_KolaBay_with_predictors.xlsx")


kola_btn_predictors <-
  kola_btn_predictors %>% 
  mutate(N_helthy = N_FC - N_BTN )


hist(kola_btn_predictors$PT)

names(kola_btn_predictors)


M_foo <- lm(Prevalence_BTN ~ PC1 + PC2 +  B_sq_m + Distance_to_BK + Distance_to_TU + Coast + Fetch, data = kola_btn_predictors)

library(car)

vif(M_foo)

names(kola_btn_predictors)

Mod <- glm(cbind(N_BTN, N_helthy) ~ PC1 + PC2 +  B_sq_m + Distance_to_BK + Distance_to_TU + Coast + Fetch, data = kola_btn_predictors, family = "binomial")

drop1(Mod)

Mod2 <- update(Mod, . ~.-Fetch) 

drop1(Mod2)

Mod3 <- update(Mod2, . ~.-Coast) 

drop1(Mod3)

Mod4 <- update(Mod3, . ~.-PC2) 


drop1(Mod4)

Mod5 <- update(Mod4, . ~.-Distance_to_TU)


drop1(Mod5)


# Mod5 - final model

### провера валидности

library(performance)

check_overdispersion(Mod5)

library(DHARMa)

check_residuals(Mod5)

sim_res <- simulate_residuals(Mod5)
plot(sim_res)

# Итоги
summary(Mod5)

# Для оценки относительной силы влияния предикторов

Mod5_scaled <- update(Mod5, .~scale(PC1) + scale(B_sq_m) + scale(Distance_to_BK))

summary(Mod5_scaled)

# Итого, чем больше PC1 (положительная связь с обилием моллюсков размером до 10 мм) тем меньше рака. То есть рак часто встречается там, где доминируют старые особи. Ну и чем дальше от Белокаменки тем меньше рака. Расстояние до Белокаменки немного более влиятельный предиктор, чем все остальные (собственно, PC1)  


# Рисовать смысла не вижу, но если хочется, то вот

library(ggeffects)


model_data <- as.data.frame(model.frame(Mod5))


  as.data.frame(ggpredict(Mod5, terms = "PC1", typical = "weighted.mean")) %>% 
  select(x, predicted, conf.low, conf.high) %>% 
  ggplot(aes(x, predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
    geom_line(color = "blue") +
    labs(x = "PC1", y = "Частота BTN")
    
  
  
  
  as.data.frame(ggpredict(Mod5, terms = "Distance_to_BK", typical = "weighted.mean")) %>% 
    select(x, predicted, conf.low, conf.high) %>% 
    ggplot(aes(x, predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
    geom_line(color = "blue")+
    labs(x = "Расстояние до Белокаменки", y = "Частота BTN")
  
