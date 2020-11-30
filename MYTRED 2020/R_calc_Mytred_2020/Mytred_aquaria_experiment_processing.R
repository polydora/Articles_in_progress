library(ggplot2)
library(readxl)
library(dplyr)
library(cowplot)




myt_aq <- read_excel("Data/Mytred_2020_growth.xlsx", sheet = "Growth for STAT", na = "NA")
str(myt_aq)


myt_aq$Origin <- ifelse(myt_aq$Species == "Edulis", "Ed", "Tr")

myt_aq_full <- myt_aq %>% filter(!is.na(Init.Size)) %>% filter(Status == "Alive")



myt_fon <- read_excel("Data/Mytred_2020_growth.xlsx", sheet = "Background mussels", na = "NA")

myt_fon <- myt_fon %>% mutate(Prop_T = T_alive/(T_alive + E_alive))


myt <- merge(myt_aq_full, myt_fon, by = "Cage")

myt <- myt %>% filter(!is.na(Last_ring))

is.na(myt$Last_ring)




# Прирост от последнего кольца первичные данные
ggplot(myt_aq_full, aes(x = Fon, y = log(Last_ring), fill = Origin)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("blue", "red"))

Mod_growth <- lm(Last_ring ~ Prop_T*Origin + Init.Size, data = myt)

# plot(Mod_growth)

summary(Mod_growth)


myt_aq_full %>% filter(Fon == "Control") %>% group_by(Origin) %>% summarise(Mean_last_ring = mean(Last_ring, na.rm = T))

ggplot(myt, aes(x = Prop_T, y = log(Last_ring + 1), color = Origin)) + 
  geom_point() +
  scale_color_manual(values = c("blue", "red")) + geom_smooth(method = "lm") + geom_hline(yintercept = )

