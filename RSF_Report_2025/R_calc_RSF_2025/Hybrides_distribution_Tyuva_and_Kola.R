# В этом коде проводится новая попытка анализа связи частоты гибридов с какими-то средовыми факторами в Баренцевом море
library(readxl)
library(dplyr)
library(tidyr)
library(mgcv)
library(gratia)
library(magrittr)
library(DHARMa)
library(ggplot2)

# Читаем данные
myt <- read_excel("Data/ALLO_BarentsSea_inter.xlsx", na = "NA")
str(myt)

myt_tyuv <- 
  myt %>% 
  filter(region == "Tyuva")

myt_tyuv %>% 
  group_by(pop) %>% 
  summarise(N_total = n(), N_hybr = sum(str < 0.9 &  str > 0.1), N_pure = N_total - N_hybr, Prop_hybr = N_hybr/N_total) %>% 
  rename(Site_ID = pop) ->
  myt_tyuv_hybr



tyuv_predictors <- read_excel("Data/TuMyt_2009_2010_for_SDM.xlsx")

tyuv_predictors <- 
  tyuv_predictors



merge(myt_tyuv_hybr, tyuv_predictors, all = T)


