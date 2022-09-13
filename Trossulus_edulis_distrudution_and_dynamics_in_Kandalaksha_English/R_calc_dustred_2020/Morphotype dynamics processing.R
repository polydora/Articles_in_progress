library(dplyr)
library(readxl)
library(ggplot2)
library(ggrepel)


myt <- read_excel("data/Mytilus old and new samples morphotype count 2022.xlsx", na = "NA")

myt %>% filter(!is.na(Year)) %>% group_by(Site, Year, Period, Site_number) %>% summarise(N_T = sum(N_T), N_E = sum(N_E)) %>% mutate(PropT = N_T/(N_T + N_E)) %>%   
  ggplot(., aes(x = Year, y = PropT, shape = Period)) + 
  geom_text_repel(aes(label = Site_number, color = Period), size = 3, ylim = c(0, 1), box.padding = 0.01) +
  scale_color_manual(values = c("blue", "black")) + ylim(0,1)

myt$Period = factor(myt$Period, labels = c("Современные сборы", "Исторические сборы"))



myt %>% filter(!is.na(Year)) %>% group_by(Site, Year, Period, Site_number) %>% summarise(N_T = sum(N_T), N_E = sum(N_E)) %>% mutate(PropT = N_T/(N_T + N_E)) %>%    
  ggplot(., aes(x = Year, y = PropT, shape = Period)) + 
  ylim(0,1) +
  theme_bw() +
  geom_point(size = 6, aes(fill = Period)) +
  scale_shape_manual(values = c(21, 22))+
  scale_fill_manual(values = c("blue", "gray")) + 
  geom_text(aes(label = Site_number, color = Period), size = 3, ylim = c(0, 1), box.padding = 0.01) +
  scale_color_manual(values = c("white", "black")) +
  labs(x = "Год", y = "Частота особей Т-морфотипа", fill = "") + 
  theme(legend.position = "bottom")

