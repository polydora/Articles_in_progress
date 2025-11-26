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
