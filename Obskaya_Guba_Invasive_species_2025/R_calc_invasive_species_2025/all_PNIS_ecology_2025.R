# В этом скрипте обрабатываем данные по всем PNIS как из старых данных, так и из добавленных в 2025 из базы https://www.iucngisd.org/gisd/

benthos <- read.csv("Data/benthos_environment_all_final.csv")
benthos$Group <- "Benthos" 


plancton <- read.csv("Data/plancton_environment_all_final.csv")
plancton$Group <- "Plancton"


all_species <- rbind(benthos, plancton)



library(dplyr)
library(readxl)

add_PNIS <- read_excel("Data/export_gisd_PNIS.xlsx")

add_PNIS <-
  add_PNIS %>% 
  select(Species, Group) %>% 
  rename(species = Species)

add_PNIS_ecology <- read.csv("Data/additional_PNIS_2025_environmental.csv")
add_PNIS_ecology$Status <- "PNIS"


add_PNIS_ecology <- merge(add_PNIS_ecology, add_PNIS)

all_species<-
rbind(all_species, add_PNIS_ecology)


write.csv(all_species, "Data/All_species_ecology.csv", row.names = F)

all_species <- read.csv("Data/All_species_ecology.csv")
str(all_species)


library(ggplot2)

ggplot(all_species, aes(x = Temp, y = Sal, color = Status)) + 
  geom_density_2d() +
  facet_wrap(~Status)


