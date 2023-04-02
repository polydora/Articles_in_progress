library(readxl)
library(reshape2)
library(dplyr)




df <- read_excel("Data/Учет_разорения_2015_2022.xlsx", na = "NA")

isl <- read_excel("Data/Kandalaksha_Bay_Islands_2021.xlsx", na = "NA")

df <- df %>% filter(! Region %in% "Кемь-лудский арх.")


df %>% filter(!Name %in% unique(isl$Name)) %>% pull(Name) %>%  unique()
