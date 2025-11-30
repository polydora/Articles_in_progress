# В этом коде проводится новая попытка анализа связи частоты гибридов с какими-то средовыми факторами в Баренцевом море
library(readxl)
library(dplyr)
library(tidyr)
library(mgcv)
library(gratia)
library(magrittr)
library(DHARMa)
library(ggplot2)

# Читаем данные
myt <- read_excel("Data/ALLO_BarentsSea_inter.xlsx", na = "NA")
str(myt)

myt <- 
  myt %>% 
  rename(Lon = E, Lat = N)


# myt_tyuv <-
#   myt %>%
#   filter(region == "Tyuva")

myt_kola <-
  myt %>%
  filter(region %in% c("Kola Bay and vicinity", "Tyuva"))

site_excluded <- c("Ura Inlet", "Mogil'naya", "Kil'din, sunduki", "Kil'dinskaya salma" )

myt_kola <- 
  myt_kola %>% 
  filter(!site %in% site_excluded)


myt_kola %>% 
  group_by(site, pop) %>% 
  summarise(Lon = mean(Lon), 
            Lat = mean(Lat), 
            N_total = n(), 
            N_hybr = sum(str < 0.9 &  str > 0.1), 
            N_hybr_1 = sum(str < 0.6 &  str > 0.4), 
            N_pure = N_total - N_hybr, 
            Prop_hybr = N_hybr/N_total,
            Prop_hybr_1 = N_hybr_1/N_total,
            Mean_str = mean(str),
            SD_str = sd(str)) %>% 
  rename(Sample_ID = pop, Site_ID = site) ->
  myt_hybr

# myt_hybr %>% 
#   ggplot(aes(Lon, Lat))+
#   geom_point() +
#   geom_text(aes(label = Site_ID))



hist(myt_hybr$Mean_str)




library(raster)

lst <- list.files(path="D:/Data_LMBE/BIO_Oracle_predictors/",pattern='asc$',full.names = T) # list the name of files in the specified

predictors <- stack(lst)

names(predictors) <- c(
  "Chlorophyll",
  "Current",
  "Dissolved_O2",
  "Nitrate",
  "pH",
  "Phosphate",
  "Phytoplankton",
  "Primary_Prod",
  "Salinity",
  "Silicate",
  "Temperature"
)


my.sites <- as.matrix(myt_hybr %>% ungroup() %>% dplyr::select(Lon, Lat))

df_predictors <- as.data.frame(extract(predictors, my.sites, method = 'bilinear'))


preds <- data.frame(Lon = my.sites[, 1], Lat = my.sites[, 2],  df_predictors)



###### Расстояние от основного источника опреснения ############



Tuloma_mouth <- data.frame(Lon = 32.839244, Lat = 68.832372)


library(geosphere)

# Вычисляем расстояния в метрах Для точек с индивидуальными данными STRUCTURE
distances <- distGeo(
  c(Tuloma_mouth$Lon, Tuloma_mouth$Lat),
  myt_kola[, c("Lon", "Lat")]
)

myt_kola$Distance_Tuloma <- distances/1000


Pl_str_vs_dist <-
myt_kola %>% 
  filter(region %in% c("Tyuva", "Kola Bay and vicinity")) %>% 
  ggplot(aes(x = Distance_Tuloma, y = str)) +
  geom_point() +
  geom_smooth(method = "gam") +
  theme_bw() +
  labs(x = "Расстояние от устья Туломы (км)", y = "STRUCTURE")

Pl_str_vs_dist


# Вычисляем расстояния в метрах Для точек с оцененной долей гибридов
distances <- distGeo(
  c(Tuloma_mouth$Lon, Tuloma_mouth$Lat),
  myt_hybr[, c("Lon", "Lat")]
)

myt_hybr$Distance_Tuloma <- distances/1000


myt_hybr %>% 
  ggplot(aes(x = Distance_Tuloma, y = SD_str)) +
  geom_point() +
  geom_smooth()


# Объединяем данные по доле гибридов с данными по предикторам из базы ORACLE
myt_hybr_preds <-
merge(myt_hybr, preds)


myt_hybr_preds %>% 
  ggplot(aes(x = Distance_Tuloma, y = Salinity)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Расстояние от устья Туломы", y = "Соленость") +
  theme_bw()




library(mgcv)
library(gratia)

Mod <- gam(cbind(N_hybr, N_pure) ~ s(Distance_Tuloma, k = -1) + s(Mean_str, k = 6), family = binomial(link = "logit"), data = myt_hybr_preds, method = "REML")


appraise(Mod)

library(DHARMa)

simulateResiduals(Mod, n = 500, refit = FALSE, plot = TRUE)


summary(Mod)

draw(Mod)




Mod_additive <- gam(
  cbind(N_hybr, N_pure) ~ 
    s(Distance_Tuloma, k = -1) +
    s(Mean_str, k = 5) +
    ti(Distance_Tuloma, Mean_str, k = c(8, 6)),   # Взаимодействие
  family = binomial(link = "logit"),
  data = myt_hybr_preds, 
  method = "REML"
)

appraise(Mod_additive)


library(DHARMa)

simulateResiduals(Mod_additive, n = 500, refit = FALSE, plot = TRUE)

summary(Mod_additive)

draw(Mod_additive)


AIC(Mod,  Mod_additive)


############ Визуализация ############################



logit_back <- function(x) exp(x)/(1 + exp(x)) # обратная логит-трансформация


sm <- smooth_estimates(Mod_additive)  %>% 
  add_confint()



sm <-
  sm %>%
  mutate(.estimate_pi = logit_back(.estimate + coef(Mod_additive)["(Intercept)"]), 
         .lower_ci_pi = logit_back(.lower_ci + coef(Mod_additive)["(Intercept)"]), 
         .upper_ci_pi = logit_back(.upper_ci +coef(Mod_additive)["(Intercept)"]))

Pl_Distance_Tuloma <- 
sm %>% 
  filter(.smooth == "s(Distance_Tuloma)") %>% 
  ggplot(aes(x = Distance_Tuloma, y = .estimate_pi)) +
  geom_ribbon(aes(ymin = .lower_ci_pi, ymax = .upper_ci_pi), alpha = 0.2) +
  geom_line() +
  geom_hline(yintercept = mean(myt_hybr_preds$Prop_hybr), linetype = 2) +
  geom_point(data = myt_hybr, aes(y = Prop_hybr)) +
  labs(x = "Расстояние от устья Туломы", y = "Partial effect") +
  theme_bw()



Pl_Mean_str <- 
sm %>% 
  filter(.smooth == "s(Mean_str)") %>% 
  ggplot(aes(x = Mean_str, y = .estimate_pi)) +
  geom_ribbon(aes(ymin = .lower_ci_pi, ymax = .upper_ci_pi), alpha = 0.2) +
  geom_line() +
  geom_hline(yintercept = mean(myt_hybr_preds$Prop_hybr), linetype = 2) +
  geom_point(data = myt_hybr, aes(y = Prop_hybr)) +
  labs(x = "Среднее значение STRUCTURE", y = "Partial effect") +
  theme_bw()

My_data <- expand.grid(Mean_str = seq(0, 1, length.out = 100), Distance_Tuloma = seq(0, max(myt_hybr_preds$Distance_Tuloma), length.out = 100))


My_data$Predicted <- predict(Mod_additive, newdata = My_data, type = "response")

Pl_interaction <- 
My_data %>% 
  ggplot(aes(x = Distance_Tuloma, y = Mean_str, z = Predicted)) +
  geom_tile(aes(fill= Predicted)) +
  scale_fill_gradient(low = "yellow", high = "red") +
  geom_contour(color = "black", alpha = 0.6, binwidth = 0.25 ) +   # кол-во изолиний
  labs(x = "Расстояние от устья Туломы", y = "Среднее значение STRUCTURE", fill = "Частота гибридов") +
  theme_bw() 
  




