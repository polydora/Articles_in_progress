# Скрипт для обработки данных мониторинга поселений пескожилов на литорали ЗРС

library(readxl)
library(dplyr)
library(mgcv)
library(reshape2)
library(gratia)
library(ggplot2)

aren_old <- read_excel("data/Arenicola_ZRS_1994_2016.xlsx", na = "NA")

aren_new <- read_excel("data/Arenicola_ZRS_2021_2024.xlsx", na = "NA")

aren <-
rbind(melt(aren_old, id.vars = c("Year", "Site", "L"), variable.name = "H", value.name = "Abundance"), aren_new)

aren$Site <- factor(aren$Site)

aren$Log_abund <- log(aren$Abundance + 1)
str(aren)

library(reshape2)
library(writexl)

aren %>%
  arrange(Year) %>%
  dcast(formula = Year + Site + L ~ H) %>%
  write_xlsx(path = "Arenicola_monitoring.xlsx")





Mod_A <- gam(Log_abund ~ s(Year, L), data = aren %>% filter(Site == "A"))

draw(Mod)

My_data <- expand.grid(Year = seq(min(aren$Year), max(aren$Year)), L = seq(1:max(aren$L)))

My_data$Fit <- predict(Mod_A, newdata = My_data)

My_data$Site <- "Разрез A"

Pl_A <-
ggplot(My_data, aes(x = Year, y = L, fill = Fit)) +
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "red") +
  scale_x_continuous(breaks = seq(min(My_data$Year), max(My_data$Year), 1)) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(trans = "reverse") +
  ggtitle("Разрез 'A'") +
  labs(x = "Годы", y = "Расстояние от верхней точки разреза", fill = "Плотность \nпоселения \n(Log)")




Mod_B <- gam(Log_abund ~ s(Year, L), data = aren %>% filter(Site == "B"))

draw(Mod_B)

My_data2 <- expand.grid(Year = seq(min(aren$Year), max(aren$Year)), L = seq(1:max(aren$L)))

My_data2$Fit <- predict(Mod_B, newdata = My_data2)

My_data2$Site <- "Разрез B"


Pl_B <-
  ggplot(My_data, aes(x = Year, y = L, fill = Fit)) +
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "red") +
  scale_x_continuous(breaks = seq(min(My_data$Year), max(My_data$Year), 1)) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(trans = "reverse") +
  ggtitle("Разрез 'B'") +
  labs(x = "Годы", y = "Расстояние от верхней точки разреза", fill = "Плотность \nпоселения \n(Log)")

library(cowplot)

plot_grid(Pl_A, Pl_B)





My_data_all <- rbind(My_data, My_data2)

ggplot(My_data_all, aes(x = Year, y = L, fill = Fit)) +
  geom_tile() +
  facet_wrap(~Site) +
  scale_fill_gradient(low = "yellow", high = "red") +
  scale_x_continuous(breaks = seq(min(My_data_all$Year), max(My_data_all$Year), 1)) +
  theme(axis.text.x = element_text(angle = 90, size = 7)) +
  scale_y_continuous(trans = "reverse")  +
  labs(x = "Годы", y = "Расстояние от верхней точки разреза", fill = "Плотность \nпоселения \n(Log)")

str(aren)


Mod_dynam <- gam(Abund ~ s(Year, by = Site) + Site, data = aren)

draw(Mod_dynam)

