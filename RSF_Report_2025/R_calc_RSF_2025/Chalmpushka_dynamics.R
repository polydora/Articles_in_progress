# В этом коде анализируется сезонная и многолетняя динамика prevalence BTN  в губе Чалмпушка 

library(readxl)
library(ggplot2)
library(lubridate)
library(dplyr)
library(mgcv)
library(gratia)
library(DHARMa)
library(broom)

chalm <- read_excel("Data/сезонная чалмпушка на рак 201125.xlsx", sheet = "Tidy", na = "NA")

chalm <- 
chalm %>% 
  mutate(DOY_2 = yday(data), N_healthy = N_total - N_cancer, fYear = factor(year)) 

chalm <- 
  chalm %>% 
  filter(year !=2020)
 

Mod_chalm_1 <- gam(cbind(N_cancer, N_total) ~ s(DOY_2, k=6, bs = "cc") + s(year, bs = "tp", k = 5), data = chalm, family = "binomial")

Mod_chalm_2 <- gam(cbind(N_cancer, N_total) ~ s(DOY_2, k=6, bs = "cc") + year, data = chalm, family = "binomial")


# Mod_chalm_2 <- gam(cbind(N_cancer, N_total) ~ s(DOY_2, k=6, bs = "cc", by = fYear) + fYear, data = chalm, family = "binomial")
# 
# Mod_chalm_3 <- gam(cbind(N_cancer, N_total) ~ s(DOY_2, k=5, bs = "cc") + s(year, k = 5), data = chalm, family = "binomial")
# 
# 
# AIC(Mod_chalm_1, Mod_chalm_2)

summary(Mod_chalm_1)



appraise(Mod_chalm_1)


simulateResiduals(Mod_chalm_1, plot = T)


draw(Mod_chalm_2, parametric = T)


logit_back <- function(x) exp(x)/(1 + exp(x)) # обратная логит-трансформация


sm <- smooth_estimates(Mod_chalm_1)  %>% 
  add_confint()



sm <-
  sm %>%
  mutate(.estimate_pi = logit_back(.estimate + coef(Mod_chalm_1)["(Intercept)"]), 
         .lower_ci_pi = logit_back(.lower_ci + coef(Mod_chalm_1)["(Intercept)"]), 
         .upper_ci_pi = logit_back(.upper_ci +coef(Mod_chalm_1)["(Intercept)"]))
  

Pl_seasons <-
sm %>% 
  filter(.smooth == "s(DOY_2)") %>% 
  ggplot(aes(x = DOY_2, y = .estimate_pi)) +
  geom_ribbon(aes(ymax = .upper_ci_pi,
                  ymin = .lower_ci_pi),
              alpha = 0.4) +
  geom_line(color = "blue", linewidth = 1) + 
  geom_point(data = chalm, aes(y = BTN_prevalence, fill = fYear), size = 4, shape = 21) +
  scale_fill_viridis_d()  +
  scale_x_continuous(
    name = "Месяц",
    breaks = c(15, 45, 75, 105, 135, 165, 195, 225, 255, 285, 315, 345),
    labels = c("Янв", "Фев", "Мар", "Апр", "Май", "Июн", 
               "Июл", "Авг", "Сен", "Окт", "Ноя", "Дек")
  ) +
  labs(x = "Месяц", y = "Частота", fill = "Годы") +
  theme_bw()



Pl_years <- 
  sm %>% 
  filter(.smooth == "s(year)") %>% 
  ggplot(aes(x = year, y = .estimate_pi)) +
  geom_ribbon(aes(ymax = .upper_ci_pi,
                  ymin = .lower_ci_pi),
              alpha = 0.4) +
  geom_line(color = "blue", linewidth = 1) +
    geom_point(data = chalm, aes(y = BTN_prevalence), size = 4, shape = 21, fill = "blue")+
    labs(x = "Годы", y = "Частота") +
    theme_bw()



