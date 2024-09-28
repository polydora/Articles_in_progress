library(readxl)

fish <- read_excel("Data/Fish_catching_Severny_Archipelago_2005_2023.xls")

fish <-
  fish %>% 
  mutate(Cod_norm = )