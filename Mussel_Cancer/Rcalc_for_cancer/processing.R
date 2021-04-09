library(readxl)
library(dplyr)
library(ggplot2)

point <- read_excel("Data/Point_presence.xlsx", na = "NA") 


point <- point %>%  mutate(Npoint = pmax(Left,Right))

point %>% filter(Seria == "J") %>% filter(Cancer == "No") %>%  mutate(Npoint = round((Left + Right)/2)) %>% filter(Npoint > quantile(Npoint)[4]) %>% select(Number) %>% pull() 



point %>% filter(!is.na(Seria)) %>% 
  ggplot(., aes(x = Seria, y = Npoint, fill = Cancer)) + geom_boxplot(varwidth = T) + scale_fill_manual(values = c("green", "red")) + labs(x = "Точка сбора", y = "Количество точечек на раковине")


point %>% filter(Seria == "J") %>% 
  ggplot(., aes(x = Cancer, y = Npoint)) + geom_boxplot()
