#install.packages("ggplot2")
library(ggplot2)
library(tidyr)
#install.packages("readxl")
library(readxl)
library(dplyr)
# install.packages("cowplot")
library(cowplot)
# install.packages("patchwork")
library(patchwork)
#install.packages("ggrepel")
library(ggrepel)
setwd("/home/birch/Документы/Tridonta_Borialis/tridonta")
size <- read_excel("data/Tridonta_Ilistaya_inlet_2023.xlsx", sheet = "Size")
Nomber_Age_size <- read_excel("data/Nomber_Age_size.xlsx", sheet = "Лист1")

#зависимость размера от возраста
ggplot(data = Nomber_Age_size, aes(x = age_years, y = width_mm))+
  geom_point()+
  theme_bw()+
  labs(x = "возраст,года", y = "высота,мм.")

#визуализация размерной структуры этого года
ggplot(data = size, aes(x = L, fill = Status)) +
  geom_histogram(binwidth = 2) +
  facet_wrap(~Status, ncol = 1)
#самый главный график в работе...
live<- size %>% filter(Status == "live")
ggplot(data = live, aes(x = L))+
  geom_histogram(binwidth = 3, color = "gray70", fill = "gray70")+
  geom_text_repel(data = Nomber_Age_size, size = 2.5, max.overlaps = Inf, aes(x = width_mm, y = 0, label = age_years))+
  theme_bw()+
  labs(x = "размер, мм.", y = "количество")



#старые и новые, смена генераций
all_sizes <- read_excel("data/Tridonta borealis data base 1987-2023.xls", sheet = "T.borealis")
all_sizes <- all_sizes %>% drop_na(L)
clusters <-
  all_sizes %>%
  select(L,Year) %>% 
  kmeans(centers = 2)

all_sizes$cluster <- clusters$cluster

ggplot(all_sizes, aes(x = L)) + geom_histogram() + facet_wrap(~cluster)

ggplot(data = all_sizes, aes(x = Year, y = L,  group = Year))+
  geom_boxplot(outlier.size = 0.1) + facet_wrap(~cluster)

ggplot(data = all_sizes, aes(x = Year, y = L))+
  geom_point(size = 0.1) + geom_smooth() + facet_wrap(~cluster)

