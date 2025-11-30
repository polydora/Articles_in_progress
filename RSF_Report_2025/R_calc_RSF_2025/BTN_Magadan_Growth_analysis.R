
library(readxl)
library(ggrepel)
library(dplyr)
library(reshape2)
library(cowplot)
library(magrittr)
library(patchwork)
library(vegan)
library(mgcv)
library(gratia)
library(broom.mixed) 



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



# myt_ind %>%  
#   filter(!is.na(Sex)) %>% 
#   ggplot(aes(BTN_Type, Prop_Increment, fill = BTN_Type)) +
#   geom_boxplot() +
#   facet_grid(Sex ~ Year)




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


myt_ind_clean$BTN_Type <- factor(myt_ind_clean$BTN_Type, labels = c("BTN1", "BTN2", "Здоровые"))

myt_ind_clean$Gonad_quality <- factor(myt_ind_clean$Gonad_quality, labels = c("Есть гаметы", "Нет гамет"))


myt_ind_clean %>%
  mutate(Gonad_Out = ifelse(Gonad_quality == "Нет гамет", 1, 0)) %>% 
  filter(BTN_Type != "Здоровые") ->
  myt_ind_clean_not_healthy

myt_ind_clean_not_healthy <-
  myt_ind_clean_not_healthy %>% 
  mutate(Lineage = BTN_Type, Aneuploid_Proportion = Rate_of_aneuploid_cells)




Mod_increment <- gam(Fi_Increment ~ s(Last_ring, bs = "cr") + Gonad_quality + BTN_Type + s(Sample, bs = "re"), family = "gaussian", data = myt_ind_clean, method = "REML")

appraise(Mod_increment)

library(DHARMa)

simulateResiduals(Mod_increment, plot = T)


summary(Mod_increment)


draw(Mod_increment)
