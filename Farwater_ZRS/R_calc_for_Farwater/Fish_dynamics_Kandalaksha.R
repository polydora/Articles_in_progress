library(readxl)
library(dplyr)
library(ggplot2)


fish <- read_excel("Data/Fish_catching_Severny_Archipelago_2005_2023.xls")

fish <-
  fish %>% 
  mutate(Cod_norm = Cod/Duration/Catchers_number,
         Navaga_norm = Navaga/Duration/Catchers_number)

Pl_cod <- 
ggplot(fish, aes(x = Year, y = Cod_norm)) +
  geom_point(position = position_jitter(width = 0.1)) +
  geom_smooth(method = "gam", se = F) +
  geom_hline(yintercept = mean(fish$Cod_norm, na.rm = T), linetype = 2) +
  theme_bw() +
  labs(x = "Годы", y = "Количество отловов за 1 час", title = "Треска") +
  ylim(0, 50)

Pl_navaga <- 
  ggplot(fish, aes(x = Year, y = Navaga_norm)) +
  geom_point(position = position_jitter(width = 0.1)) +
  geom_smooth(method = "gam", se = F) +
  geom_hline(yintercept = mean(fish$Navaga_norm, na.rm = T), linetype = 2) +
  theme_bw() +
  labs(x = "Годы", y = "Количество отловов за 1 час", title = "Навага") +
  ylim(0, 50)

library(cowplot)
plot_grid(Pl_cod, Pl_navaga)



