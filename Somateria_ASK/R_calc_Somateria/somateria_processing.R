library(ggplot2)
library(readxl)
library(lubridate)
library(mgcv) 
library(gratia)
library(dplyr)
library(DHARMa)

som <- read_excel("Data/Somateria_escape_distance.xlsx", na = "NA")

som <- 
  som %>% 
  mutate(Year = year(Date))


som <-
  som %>% 
  filter(complete.cases(.)) %>% 
  filter(incubation_period < 30)

isl <- read_excel("Data/Острова -2005-2011_ расстояния_eng.xls")

som_isl <- merge(som, isl)

som_isl$Place <- factor(som_isl$Place)
som_isl$Region <- factor(som_isl$Region)

som_isl$Log_Area <- log(som_isl$Area)
som_isl$Log_Ed <- log(som_isl$Ed + 1)


str(som_isl)

mod <- gam(Log_Ed ~ s(N_eggs, k = 4) + s(incubation_period) + s(Year, k = 6) + s(Log_Area, k = 5 ) + s(Dist_to_Mainland) + s(Dist_to_town, k = 5)  + Region + s(Place, k = 87, bs = "re"), data = som_isl)


appraise(mod)


simulateResiduals(mod, plot = T)

summary(mod)


draw(mod, select = c(1:4))


sm <- smooth_estimates(mod) |>
  add_confint()

unique(sm$.smooth)

sm %>% 
  filter(.smooth == "s(N_eggs)") %>% 
    ggplot(aes(x = N_eggs, y= .estimate)) + 
      geom_line() + 
  geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci), alpha = 0.5) 
  



library(itsadug)
wald_compar <- wald_gam(mod)


library(ggplot2)
library(ggsignif)


ggplot(som_isl, aes(x = Region, y = Log_Ed)) +
  geom_boxplot(fill = "gray80") +
  annotate(geom = "text", label = "a", x = 1, y = 4.5, size = 8) +
  annotate(geom = "text", label = "b", x = 2, y = 4.5, size = 8) +
  annotate(geom = "text", label = "a", x = 3, y = 4.5, size = 8) +
  theme_bw()
