library(readxl)
library(dplyr)


point <- read_excel("Data/Point_presence.xlsx", na = "NA") 

point %>% filter(Seria == "J") %>% filter(Cancer == "No") %>%  mutate(Npoint = round((Left + Right)/2)) %>% filter(Npoint > quantile(Npoint)[4]) %>% select(Number) %>% pull() 



point %>% filter(Seria == "J") %>% filter(Cancer == "No") %>%  mutate(Npoint = round((Left + Right)/2)) %>% filter(Npoint < quantile(Npoint)[4]) %>% select(Number)  %>% pull() %>% sample(., 4)
