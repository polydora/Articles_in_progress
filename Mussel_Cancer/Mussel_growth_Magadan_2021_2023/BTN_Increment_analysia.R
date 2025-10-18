# Здесь обрабатываются данные по индивидуальным оценкам разных параметров у мидий 

library(readxl)
library(dplyr)
library(tidyr)
library(magrittr)
library(mgcv)
library(gratia)
library(ggplot2)
library(DHARMa)


# Данные по индивидуальным характристикам мидий
myt_ind <- read_excel("Data/Mussel_growth_Magadan_2021_2023.xlsx", na = "NA")

myt_ind$Sample <- paste(myt_ind$Site_code, myt_ind$Sample, sep = "_")


# Удаляем идий, которые были испольованы для анализа роста, но не анализировались с для определения BTN

myt_ind %<>%
  filter(!is.na(DN))


myt_ind$DN <- factor(myt_ind$DN)
myt_ind$Site_code <- factor(myt_ind$Site_code)
myt_ind$Sample <- factor(myt_ind$Sample)
myt_ind$Rate_of_aneuploid_cells <- as.numeric(myt_ind$Rate_of_aneuploid_cells)

myt_ind %<>%
  mutate(Prop_Increment = Increment/L) %>% 
  mutate(BTN_Type = case_when(BTN_genotype == "BTN1" ~ "BTN1",
                              BTN_genotype %in% c("BTN2.1", "BTN2.2") ~ "BTN2",
                              BTN_genotype == "healthy" ~ "healthy"))

myt_ind$BTN_Type <- factor(myt_ind$BTN_Type)


myt_ind %<>%
  mutate(Gonad_quality = case_when(Sex == "male" ~ "Developed",
                                   Sex == "female" ~ "Developed",
                                   Sex == "no gametes" ~ "No"))


myt_ind <-
  myt_ind %>% 
  filter(!is.na(BTN_Type))



myt_ind %>%  
  filter(!is.na(Sex)) %>% 
  ggplot(aes(BTN_Type, Prop_Increment, fill = BTN_Type)) +
  geom_boxplot() +
  facet_grid(Sex ~ Year)




myt_ind_clean <- 
  myt_ind %>%  
  filter(!is.na(BTN_Type)) %>% 
  filter(Sex != "hermaphrodite") %>% 
  filter(!is.na(Sex)) 

myt_ind_clean$Fi_Increment <- 2*asin(sqrt(myt_ind_clean$Prop_Increment)) * 180/pi

points <- read_excel("Data/Magadan_2021_2023_ecology.xlsx", sheet = "Points  characteristic 2021-23", na = "NA")

points %<>%
  rename(Site_code = Site)


# points_2023 <- 
#   points %>% 
#   filter(Year == 2023) %>% 
#   rename(Site_code = Site)

myt_ind_clean <- merge(myt_ind_clean, points)


table(myt_ind_clean$BTN_Type, myt_ind_clean$Gonad_quality)



# Mod <- gam(Increment ~ s(Rate_of_aneuploid_cells) + s(Last_ring, k = 5) + s(Sample, bs = "re"), data = myt_ind, family = "gaussian")
# 
# 
# res <- simulateResiduals(Mod)
# plot(res)
# 
# testResiduals(Mod)
# appraise(Mod)
# 
# summary(Mod)
# 
# 
# draw(Mod)



Mod_prop_incr <- gam(Fi_Increment ~ s(Rate_of_aneuploid_cells) + s(Last_ring, by = DN) + BTN_Type + s(fetch) + s(Sample, bs = "re"), family = "gaussian", data = myt_ind_clean, method = "REML")

appraise(Mod_prop_incr)

res <- simulateResiduals(Mod_prop_incr)
plot(res)

testResiduals(Mod_prop_incr)


qplot(x = fitted(Mod_prop_incr), y = residuals(Mod_prop_incr, type = "pearson")) + 
  geom_smooth(method = "gam") +
  geom_hline(yintercept = 0)




summary(Mod_prop_incr)

draw(Mod_prop_incr, scales = "free", grouped_by = T, parametric = T)




Mod_prop_incr_btn <- gam(Prop_Increment ~ BTN_Type + s(Last_ring, by = BTN_Type, k = 5) + s(Rate_of_aneuploid_cells) + s(fetch, k = 5) + s(Sample, bs = "re"), family = "betar", data = myt_ind_clean, method = "REML")

appraise(Mod_prop_incr_btn)

res <- simulateResiduals(Mod_prop_incr_btn, plot = T)

testResiduals(Mod_prop_incr)

qplot(x = fitted(Mod_prop_incr_btn), y = residuals(Mod_prop_incr_btn, type = "pearson")) + 
  geom_smooth(method = "gam") +
  geom_hline(yintercept = 0)


summary(Mod_prop_incr_btn)

draw(Mod_prop_incr_btn, scales = "free", grouped_by = T, parametric = T)


table(myt_ind$Sex, myt_ind$BTN_Type )




# %>% 
#   ggplot(aes(BTN_Type, Prop_Increment, fill = BTN_Type)) +
#   geom_boxplot() +
#   facet_wrap(~Sex)

myt_ind_clean$Sex <- factor(myt_ind_clean$Sex)

myt_ind_clean$fYear <- factor(myt_ind_clean$Year)


Mod_prop_incr_btn <- gam(Fi_Increment ~ BTN_Type + s(Last_ring, bs = "cr", by = BTN_Type)  + s(Sample, bs = "re"), family = "gaussian", data = myt_ind_clean, method = "REML")

gam.check(Mod_prop_incr_btn)

appraise(Mod_prop_incr_btn)

res <- simulateResiduals(Mod_prop_incr_btn, plot = T)

testResiduals(Mod_prop_incr_btn)

summary(Mod_prop_incr_btn)

draw(Mod_prop_incr_btn, scales = "free", grouped_by = T, parametric = T)


My_data <- data.frame(BTN_Type = levels(myt_ind_clean$BTN_Type), Last_ring = mean(myt_ind_clean$Last_ring))



Predicted <- predict(Mod_prop_incr_btn, newdata = My_data, exclude = "s(Sample)", newdata.guaranteed=TRUE, se.fit = TRUE)

My_data$Fit <- Predicted$fit
My_data$SE <- Predicted$se.fit

library(ggplot2)
ggplot(My_data, aes(x = BTN_Type, y = Fit)) +
  geom_col() +
  geom_errorbar(aes(ymin = Fit - 2*SE, ymax = Fit + 2*SE), width = 0.2)


library(itsadug)

wald_gam(Mod_prop_incr_btn)




library(multcomp)
  
compare <- glht(Mod_prop_incr_btn, linfct = mcp(BTN_Type  = "Tukey"))

summary(compare)

######################################

# Анализ зависимости вероятности встретить мидию с неразвитыми гонадами от доли ануеплоидных клеток

myt_ind_clean %>%
  mutate(Gonad_Out = ifelse(Gonad_quality == "No", 1, 0)) %>% 
  filter(BTN_Type != "healthy") ->
  myt_ind_clean_not_healthy

Mod_gonad <- gam(Gonad_Out ~ s(Rate_of_aneuploid_cells, by = BTN_Type) + BTN_Type , family = "binomial",  method = "REML", data = myt_ind_clean_not_healthy)

appraise(Mod_gonad)

summary(Mod_gonad)

Pl_gonad_BTN1 <- 
  draw(Mod_gonad,  scales = "fixed", select = 1) +
  geom_hline(yintercept = 0, linetype = 2)

Pl_gonad_BTN2 <- 
  draw(Mod_gonad,  scales = "fixed", select = 2) +
  geom_hline(yintercept = 0, linetype = 2)

library(cowplot)

plot_grid(Pl_gonad_BTN1, Pl_gonad_BTN2)

