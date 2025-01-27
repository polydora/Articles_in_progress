# В этом скрипте обрабатываем данные по всем PNIS как из старых данных, так и из добавленных в 2025 из базы https://www.iucngisd.org/gisd/
library(dplyr)
library(ggplot2)

all_species <- read.csv("Data/All_species_ecology.csv")


all_species %>% 
  filter(Status == "Native") %>%
  select(species, Group, Status) %>% 
  unique() 

  
all_species %>% 
  filter(Status == "PNIS") %>%
  select(species, Group, Status) %>% 
  filter(Group == "Plancton") %>% 
  unique() 


all_species %>% 
  filter(Status == "Native") %>%
  select(species, Group, Status) %>% 
  filter(Group == "Plancton") %>% 
  unique() %>% 
  arrange(species)




library(ggplot2)

ggplot(all_species, aes(x = Temp, y = Sal, color = Status)) + 
  geom_density_2d() +
  facet_wrap(~Status)


