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
  select(Site_code, Size_class)


scam <- dcast(Site_code ~ Size_class, data = size)


# scam_23 <- scam %>% filter(Year == 2023)

library(vegan)
pca_scam <- rda(decostand(scam[ , -c(1)], method = "hellinger" ))


sum_pca_scam <- summary(pca_scam)

pca_scam_size_scores <- as.data.frame(scores(pca_scam)$species)

pca_scam_scores <- as.data.frame(scores(pca_scam)$sites)


pca_scam_scores$Site_code <- scam$Site_code


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


kola_btn_predictors <- read_excel("Data/sites_info_KolaBay_with_predictors.xlsx")


kola_btn_predictors <-
  kola_btn_predictors %>% 
  mutate(N_helthy = N_FC - N_BTN )

M_foo <- lm(Prevalence_BTN ~ PC1 + PC2 +  B_sq_m + Distance_to_BK + Fetch, data = kola_btn)

library(car)

vif(M_foo)

names(kola_btn_predictors)

Mod <- glm(cbind(N_BTN, N_helthy) ~ PC1 + PC2 +  B_sq_m + Distance_to_BK + Fetch, data = kola_btn_predictors, family = "binomial")

summary(Mod)

library(performance)

check_overdispersion(Mod)

library(DHARMa)

check_residuals(Mod)

sim_res <- simulate_residuals(Mod)
plot(sim_res)



Mod <- gam(cbind(N_BTN, N_helthy) ~ s(PC1, k = 5) + s(PC2, k = 5) +  s(B_sq_m, k = 5) + s(Distance_to_BK, k = 5) + s(Fetch, k = 5), data = kola_btn_predictors, family = "binomial")

library(ggplot2)
library(gratia)

summary(Mod)


plot(Mod)
