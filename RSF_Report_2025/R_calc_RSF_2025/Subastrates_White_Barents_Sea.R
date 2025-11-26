# В этом коде анализируется расхождение по субстратам в двух морях. Белое море - по данным из Khaitov et al.2025 Баренцево море - данные полученные группой Стрелкова

library(readxl)
library(ggplot2)
library(lubridate)
library(dplyr)
library(mgcv)
library(gratia)
library(DHARMa)
library(broom)
library(reshape2)

substr <- read_excel("Data/substrates_White_Barents_Sea.xlsx", na = "NA")

substr$sea <- factor(substr$sea)

substr <- 
  substr %>% 
  filter(habitat == "littoral")

Mod_1 <- gam(diff_Ptros ~ s(Ptros_mean, bs = "cs") + sea, data = substr,  method = "REML")

Mod_2 <- gam(diff_Ptros ~ s(Ptros_mean, by = sea, bs = "cs") + sea, data = substr, method = "REML")

AIC(Mod_1, Mod_2)

summary(Mod_2)
draw(Mod_2)


substr_long <-
substr %>% 
  select(-diff_Ptros) %>% 
  melt(., id.vars = c( "sea", "habitat", "site", "Ptros_mean"), variable.name = "Substrate", value.name = "Ptros")


Mod_3 <- gam(Ptros ~ Substrate*sea + s(Ptros_mean, bs = "cr"), family = "betar", data = substr_long, method = "REML")


appraise(Mod_3)

draw(Mod_3, parametric = T)

library(performance)

summary(Mod_3)



# Визуализация модели
library(ggeffects)

My_data <-
  as.data.frame(ggpredict(Mod_3, terms = c("sea",  "Substrate"), typical = "weighted.mean")) %>% 
  select(x, predicted, conf.low, conf.high) %>% 
  mutate(Substrate = rep(c("Algae", "Bottom"), 2) )

My_data %>% 
  ggplot(aes(x = x, y = predicted, fill = Substrate)) +
  geom_col(position  = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, position  = position_dodge(width = 0.9)) +
  labs(x = "", y = "Ptros")

