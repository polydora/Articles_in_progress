# В этом коде анализируется сезонная и многолетняя динамика prevalence BTN  в губе Чалмпушка 

library(readxl)
library(ggplot2)
library(lubridate)
library(dplyr)
library(mgcv)
library(gratia)


chalm <- read_excel("Data/сезонная чалмпушка на рак 201125.xlsx", sheet = "Tidy", na = "NA")

chalm <- 
chalm %>% 
  mutate(DOY_2 = yday(data), N_healthy = N_total - N_cancer, fYear = factor(year)) 

chalm <- 
  chalm %>% 
  filter(year !=2020)
 

Mod_chalm_1 <- gam(cbind(N_cancer, N_total) ~ s(DOY_2, k=6, bs = "cc") + s(fYear, bs = "re"), data = chalm, family = "binomial")


Mod_chalm_2 <- gam(cbind(N_cancer, N_total) ~ s(DOY_2, k=6, bs = "cc", by = fYear) + fYear, data = chalm, family = "binomial")

Mod_chalm_3 <- gam(cbind(N_cancer, N_total) ~ s(DOY_2, k=5, bs = "cc") + s(year, k = 5), data = chalm, family = "binomial")


AIC(Mod_chalm_1, Mod_chalm_2, Mod_chalm_3)

summary(Mod_chalm_1)



appraise(Mod_chalm_3)

library(DHARMa)
simulateResiduals(Mod_chalm_3, plot = T)


draw(Mod_chalm_3)


sm <- smooth_estimates(Mod_chalm_1)  %>% 
  add_confint()



sm <-
  sm %>% 
  

sm %>% 
  filter(.smooth == "s(year)") %>% 
  ggplot(aes(x = year, y = .estimate  )) +
  geom_line()


